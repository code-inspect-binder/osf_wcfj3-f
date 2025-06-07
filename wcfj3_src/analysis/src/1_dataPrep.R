### Load behavioral and demographic data ####

behDat = read.csv(paste(inputFolder, "behData.csv", sep = "/"), sep = ",") # cognition and socio-affect
demDat = read.csv(paste(inputFolder, "demData.csv", sep = "/"), sep = ";") # background variables

# join behDat and demDat
allData = inner_join(behDat, demDat, by = "userCode")

# Correct for version mean performance in Verbal Fluency s by adding the difference from a given version's mean and the overall mean to each of the scores
allData <- allData %>%
  group_by(RWT_Version) %>%
  mutate(RWT_mean = mean(RWT, na.rm = T)) %>%
  ungroup() %>%
  mutate(RWT_overall_mean = mean(RWT_mean, na.rm = T),
         RWT_mean_diff = RWT_overall_mean - RWT_mean,
         RWT = RWT + RWT_mean_diff) %>%
  select(-c(RWT_mean, RWT_overall_mean, RWT_mean_diff))

# Reduce dataset to cognitive variables and their possible predictors
allData = allData %>%
  group_by(userCode) %>%
  mutate(Time = 1:length(session)) %>%
  mutate(BLP = (BLP_overall_and + BLP_overall_eng + BLP_overall_franz+ BLP_overall_ital) / 4) %>%
  select(userCode, Time, Group, class, age, gend, Act_total, BLP, educ_y,
        Wellbeing, TrainingMotivation, 
         rtAlert = Alertness, sDivAtt = sDividedAttention_sens, 
         rtDivAtt = DividedAttention, sOpSpan = OperationSpan, sNBack = sNBack_sens,
         rtNBack = NBack, sRWT = RWT)

# Remove rhhs2 and 0gnkw (had stroke during participation)
allData = allData %>%
  filter(userCode != "rhhs2" & userCode != "0gnkw")

# save to file
write.csv(file = paste(inputFolder, "selectedData.csv", sep = "/"), allData, row.names = F)











### Make df - 1 column per skill  ################################################################## 
# Load
df = read.csv(paste(inputFolder, "selectedData.csv", sep = "/"))

# Preprocess data
df = preproc(df)

# Remove RWT heu7g from session 20. Probably incorrectly entered data.
df$sRWT[(df$userCode == "heu7g" & df$Time == 20)] = NA

# add column for baseline level of each skill
# baseline level calculated as GAM per subject and task
allIntercepts = find_initLevel(df, columns = c("sWM", "rtWM", "rtDivAtt", "rtAlert", "sRWT"))
df = inner_join(df, allIntercepts, by = "userCode")

# give jw6up manual intercept for rtDivAtt, because they did the test incorrectly for the first couple of weeks
# first correctly done session will be used as baseline
df$initLevel_rtDivAtt[df$userCode == "jw6up"] = df$rtDivAtt[which((is.na(df$rtDivAtt) == F) & df$userCode == "jw6up")[1]]
df$rtDivAtt[df$userCode == "jw6up" & df$Time %in% c(0:11)] = unique(df$initLevel_rtDivAtt[df$userCode == "jw6up"])

# remove time points above 28 (median max value for Time)
df = df %>%
  filter(Time < 29)

# make sure it's a dataframe
df = as.data.frame(df)

# save data
save(df, file = paste(inputFolder, "df_withIntercepts.rda", sep = "/"))


### Standardize Variables ####

#z-scoring
df_overall = df
df_overall$sWM = scale(df_overall$sWM)
df_overall$rtWM = scale(df_overall$rtWM)
df_overall$rtDivAtt = scale(df_overall$rtDivAtt)
df_overall$rtAlert = scale(df_overall$rtAlert)
df_overall$sRWT = scale(df_overall$sRWT)

# remove previous intercepts (which were non-normalized)
df_overall$initLevel_sWM = NULL
df_overall$initLevel_rtWM = NULL
df_overall$initLevel_rtDivAtt = NULL
df_overall$initLevel_rtAlert = NULL
df_overall$initLevel_sRWT = NULL

# find baseline again based on z-scores
allIntercepts = find_initLevel(df_overall, columns = c("sWM", "rtWM", "rtDivAtt", "rtAlert", "sRWT"))
df_overall = inner_join(df_overall, allIntercepts, by = "userCode")

# sort by userCode and Time
df_overall = df_overall %>%
  arrange(userCode, Time)

# to long format
tmp1 = melt(df_overall, id.vars = c("userCode", "Time", "age", "BLP", "educ_y", "Act_total",
                                  "start.event", "Group", "isLANG", "isLANG0", "isACTV", "isACTV0", "isPASV", "isPASV0", 
                                  "class", "gend", "socioAffect"),
                  measure.vars = c("sWM", "rtWM", "rtDivAtt", "rtAlert", "sRWT"),
                  variable.name = "Tasks",
                  value.name = "score")

tmp2 = melt(df_overall, id.vars = c('userCode', 'Time'),
           measure.vars = c('initLevel_sWM', 'initLevel_rtWM', 'initLevel_rtDivAtt',
                            'initLevel_rtAlert', 'initLevel_sRWT'),
           variable.name = "Task",
           value.name = "initLevel")
tmp2 = tmp2 %>% select(-Task)

df_overall = cbind(tmp1, tmp2$initLevel)
names(df_overall)[names(df_overall) == 'tmp2$initLevel'] = 'initLevel'

df_overall = df_overall %>%
  arrange(userCode, Tasks, Time)

# Invert RTs so that scale is interpretable as positive =  better ####

df_overall = df_overall %>%
  rowwise() %>%
  mutate(score = ifelse(Tasks %in% c("rtWM", "rtDivAtt", "rtAlert"), 
                        score *-1,
                        score),
         initLevel = ifelse(Tasks %in% c("rtWM", "rtDivAtt", "rtAlert"), 
                            initLevel *-1,
                            initLevel))

# Remove time points above 29, because not everyone attended that often ####
df_overall = df_overall %>% filter(Time < 29) %>% ungroup()

# Make everyone's Timepoints go from 0-28 so that start.event has same number of cases
for (i in 1:length(unique(df_overall$userCode))) { # go through all subjects
  #print(i)
  
  if(length(unique(df_overall$Time[df_overall$userCode == unique(df_overall$userCode)[i]])) < 29) {
    for (j in 1:5) { # go through all tasks
      tmp = df_overall %>% filter(userCode == unique(df_overall$userCode)[i] &
                                    Tasks == unique(df_overall$Tasks)[j]) # this subject's and task's data
      missing = which(c(0:28) %in% tmp$Time == F) # find missing sessions
      toInsert = data.frame()
      
      # copy any line times the number of missing sessions
      if (length(missing)) {
        for (k in 1:length(missing)) {
          toInsert = rbind.data.frame(toInsert, tmp[10,])
        }
        
        toInsert$Time = missing
        toInsert$score = NA
      }
      
      df_overall = rbind.data.frame(df_overall, toInsert)
    } 
  }
}


# Make interaction terms between group and task ####
df_overall$GroupTask = interaction(df_overall$Group, df_overall$Tasks)

# reinsert contrasts
contrasts(df_overall$isLANG0) = "contr.treatment"
contrasts(df_overall$isACTV0) = "contr.treatment"
contrasts(df_overall$isPASV0) = "contr.treatment"

# Make new ordered factors for Group and Task combinations
df_overall$isACTVissWM = ifelse(df_overall$Group == "ACTV" & df_overall$Tasks == "sWM",1,0)
df_overall$isACTVissWM0 = as.ordered(factor(df_overall$isACTVissWM))
df_overall$isACTVisrtWM = ifelse(df_overall$Group == "ACTV" & df_overall$Tasks == "rtWM",1,0)
df_overall$isACTVisrtWM0 = as.ordered(factor(df_overall$isACTVisrtWM))
df_overall$isACTVisrtDivAtt = ifelse(df_overall$Group == "ACTV" & df_overall$Tasks == "rtDivAtt",1,0)
df_overall$isACTVisrtDivAtt0 = as.ordered(factor(df_overall$isACTVisrtDivAtt))
df_overall$isACTVisrtAlert = ifelse(df_overall$Group == "ACTV" & df_overall$Tasks == "rtAlert",1,0)
df_overall$isACTVisrtAlert0 = as.ordered(factor(df_overall$isACTVisrtAlert))
df_overall$isACTVissRWT = ifelse(df_overall$Group == "ACTV" & df_overall$Tasks == "sRWT",1,0)
df_overall$isACTVissRWT0 = as.ordered(factor(df_overall$isACTVissRWT))

df_overall$isPASVissWM = ifelse(df_overall$Group == "PASV" & df_overall$Tasks == "sWM",1,0)
df_overall$isPASVissWM0 = as.ordered(factor(df_overall$isPASVissWM))
df_overall$isPASVisrtWM = ifelse(df_overall$Group == "PASV" & df_overall$Tasks == "rtWM",1,0)
df_overall$isPASVisrtWM0 = as.ordered(factor(df_overall$isPASVisrtWM))
df_overall$isPASVisrtDivAtt = ifelse(df_overall$Group == "PASV" & df_overall$Tasks == "rtDivAtt",1,0)
df_overall$isPASVisrtDivAtt0 = as.ordered(factor(df_overall$isPASVisrtDivAtt))
df_overall$isPASVisrtAlert = ifelse(df_overall$Group == "PASV" & df_overall$Tasks == "rtAlert",1,0)
df_overall$isPASVisrtAlert0 = as.ordered(factor(df_overall$isPASVisrtAlert ))
df_overall$isPASVissRWT = ifelse(df_overall$Group == "PASV" & df_overall$Tasks == "sRWT",1,0)
df_overall$isPASVissRWT0 = as.ordered(factor(df_overall$isPASVissRWT))

contrasts(df_overall$isACTVissWM0) = "contr.treatment"
contrasts(df_overall$isPASVissWM0) = "contr.treatment"
contrasts(df_overall$isACTVisrtWM0) = "contr.treatment"
contrasts(df_overall$isPASVisrtWM0) = "contr.treatment"
contrasts(df_overall$isACTVisrtDivAtt0) = "contr.treatment"
contrasts(df_overall$isPASVisrtDivAtt0) = "contr.treatment"
contrasts(df_overall$isPASVisrtAlert0) = "contr.treatment"
contrasts(df_overall$isACTVisrtAlert0) = "contr.treatment"
contrasts(df_overall$isPASVissRWT0) = "contr.treatment"
contrasts(df_overall$isACTVissRWT0) = "contr.treatment"

# ensure that userCode is a factor
df_overall$userCode = as.factor(df_overall$userCode)


# save to file
save(df_overall, file = paste(inputFolder, "dfOverall_withIntercepts.rda", sep = "/"))






### Make id_df (df for background variables)  ##################################################################################################
# Dataframe to assess differences between groups based on ID variables:
# Education, multilingualism, number of hobbies, socio-affect, cognitive baseline

load(file = paste(inputFolder, "dfOverall_withIntercepts.rda", sep = "/"))

# get ID variables of interest
id_df = df_overall %>%
  group_by(userCode) %>%
  mutate(socioAffect_init = mean(c(socioAffect[1], socioAffect[2], socioAffect[3]), na.rm = T)) %>%
  mutate(socioAffect_init = ifelse(is.na(socioAffect_init), socioAffect[4], socioAffect_init)) %>%
  ungroup() %>%
  mutate(Time = 0,
         ageZ = scale(age),
         educ_yZ = scale(educ_y),
         BLPZ = scale(BLP),
         Act_totalZ = scale(Act_total)) %>%
  group_by(userCode, Group, isLANG0, isACTV0, Time, age, 
           ageZ, gend, educ_y, educ_yZ, BLP,
           BLPZ, Act_total, Act_totalZ, socioAffect_init) %>%
  summarise(initLevel = mean(initLevel)) 


#save 
save(id_df, file = paste(inputFolder, "id_df.rda", sep = "/"))


## To long format
id_dfLong = melt(id_df, id.vars = c("userCode", "Group"),
                 measure.vars = c("age", "ageZ", "educ_y", "educ_yZ", "BLP",
                                  "BLPZ", "Act_total", "Act_totalZ", "gend"),
                 variable.name = "Measure",
                 value.name = "Value")

#save 
save(id_dfLong, file = paste(inputFolder, "id_dfLong.rda", sep = "/"))

#### Clean up ######################################################
remove(tmp1, tmp2, id_df, id_dfLong, allData, behDat, demDat)

