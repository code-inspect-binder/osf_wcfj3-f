### GAMM analyses investigating the influence of experimental group (PASV, ACTV, LANG) 
### on cognitive development
### Input: all cognitive and control variables 
### Output: GAMMs (summary + plots)


### Load Data  ########################################################################################################################
############################################################################################################
# See dataPrep.R for how each dataframe is made

# Dataframe where tasks are split up by columns
load(file = paste(inputFolder, "df_withIntercepts.rda", sep = "/"))

# Dataframe where task values are normalized and in long format
load(file = paste(inputFolder, "dfOverall_withIntercepts.rda", sep = "/"))



############################################################################################################

### M1: Group as Group  ##########################################################
### Predictor(s): Time, Group
### Random Effect(s): per subject within group

# overall cognition as model over the entire dataset
m1 = bam(score ~ 
            Group +
            s(Time, by = Group) +
            s(Time, userCode, by = Group, bs = "fs", m = 1),
          data = df_overall, discrete = T, nthreads = 7)
summ_m1 = summary(m1)

## Save to file
save(m1, file = paste(outputFolder, "tables_and_summaries", "m1.rda", sep = "/"))
save(summ_m1, file = paste(outputFolder, "tables_and_summaries", "summ_m1.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "m1.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_m1.rda", sep = "/"))

## Remove (to save memory)
remove(m1, summ_m1)  

  
  
  
  

### M2: Groups as OF ##########################################################
### Predictor(s): Time, Group (as ordered)
### Random Effect(s): per subject within group

# overall cognition as model over the entire dataset
m2_old = bam(score ~ 
            isACTV0 +
            isPASV0 +
            s(Time) +
            s(Time, by = isACTV0) +
            s(Time, by = isPASV0) +
            s(Time, userCode, bs = "fs", m = 1),
          data = df_overall, discrete = T, nthreads = 7)

r1 = start_value_rho(m2_old)

m2 = bam(score ~ 
            isACTV0 +
            isPASV0 +
            s(Time) +
            s(Time, by = isACTV0) +
            s(Time, by = isPASV0) +
            s(Time, userCode, bs = "fs", m = 1),
          data = df_overall, rho = r1,  AR.start = df_overall$start.event,
          discrete = T, nthreads = 7)


summ_m2 = summary(m2)


## Save to file
save(m2, file = paste(outputFolder, "tables_and_summaries", "m2.rda", sep = "/"))
save(summ_m2, file = paste(outputFolder, "tables_and_summaries", "summ_m2.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "m2.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_m2.rda", sep = "/"))

## Clean up
remove(m2, summ_m2)

















### M2.bin: Groups as binary curve ##########################################################
### Predictor(s): Time, Group (as ordered)
### Random Effect(s): per subject within group

# overall cognition as model over the entire dataset
m2.bin_old = bam(score ~ 
               s(Time) +
               s(Time, by = isACTV) +
               s(Time, by = isPASV) +
               s(Time, userCode, bs = "fs", m = 1),
             data = df_overall, discrete = T, nthreads = 7)

r1 = start_value_rho(m2.bin_old)

m2.bin = bam(score ~ 
               s(Time) +
               s(Time, by = isACTV) +
               s(Time, by = isPASV) +
           s(Time, userCode, bs = "fs", m = 1),
         data = df_overall, rho = r1,  AR.start = df_overall$start.event,
         discrete = T, nthreads = 7)


summ_m2.bin = summary(m2.bin)


## Save to file
save(m2.bin, file = paste(outputFolder, "tables_and_summaries", "m2.bin.rda", sep = "/"))
save(summ_m2.bin, file = paste(outputFolder, "tables_and_summaries", "summ_m2.bin.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "m2.bin.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_m2.bin.rda", sep = "/"))

## Clean up
remove(m2.bin, summ_m2.bin)













### M3: Groups as OF with Task Distinction ##########################################################
### Predictor(s): Time, Group per Task (as ordered)
### Random Effect(s): per subject within group

m3_old = bam(score ~ 
               
               # smooths for reference level LANG
               s(Time, by = Tasks) + 
               Tasks + 
               
               # smooths for ACTV as ordered factors
               s(Time, by = isACTVissWM0) +
               isACTVissWM0 + 
               s(Time, by = isACTVisrtWM0) +
               isACTVisrtWM0 + 
               s(Time, by = isACTVisrtDivAtt0) +
               isACTVisrtDivAtt0 + 
               s(Time, by = isACTVisrtAlert0) +
               isACTVisrtAlert0 + 
               s(Time, by = isACTVissRWT0) +
               isACTVissRWT0 + 
               
               # smooths for PASV as ordered factors
               s(Time, by = isPASVissWM0) +
               isPASVissWM0 + 
               s(Time, by = isPASVisrtWM0) +
               isPASVisrtWM0 + 
               s(Time, by = isPASVisrtDivAtt0) +
               isPASVisrtDivAtt0 + 
               s(Time, by = isPASVisrtAlert0) +
               isPASVisrtAlert0 + 
               s(Time, by = isPASVissRWT0) +
               isPASVissRWT0 +
               
               # random effects for subject and task in LANG
               s(Time, userCode, bs = "fs", m = 1),
             data = df_overall, discrete = T, nthreads = 7)

r1 = start_value_rho(m3_old)

m3 = bam(score ~ 
           # smooths for reference level LANG
           s(Time, by = Tasks) + 
           Tasks + 
           
           # smooths for ACTV as ordered factors
           s(Time, by = isACTVissWM0) +
           isACTVissWM0 + 
           s(Time, by = isACTVisrtWM0) +
           isACTVisrtWM0 + 
           s(Time, by = isACTVisrtDivAtt0) +
           isACTVisrtDivAtt0 + 
           s(Time, by = isACTVisrtAlert0) +
           isACTVisrtAlert0 + 
           s(Time, by = isACTVissRWT0) +
           isACTVissRWT0 + 
           
           # smooths for PASV as ordered factors
           s(Time, by = isPASVissWM0) +
           isPASVissWM0 + 
           s(Time, by = isPASVisrtWM0) +
           isPASVisrtWM0 + 
           s(Time, by = isPASVisrtDivAtt0) +
           isPASVisrtDivAtt0 + 
           s(Time, by = isPASVisrtAlert0) +
           isPASVisrtAlert0 + 
           s(Time, by = isPASVissRWT0) +
           isPASVissRWT0 +
           
           # random effects for subject and task in LANG
           s(Time, userCode, bs = "fs", m = 1),
         
         data = df_overall, rho = r1,  AR.start = df_overall$start.event,
         discrete = T, nthreads = 7)

summ_m3 = summary(m3)

## Save to file
save(m3, file = paste(outputFolder, "tables_and_summaries", "m3.rda", sep = "/"))
save(summ_m3, file = paste(outputFolder, "tables_and_summaries", "summ_m3.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "m3.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_m3.rda", sep = "/"))

# Model criticism
qqp(resid(m3))
acf_resid(m3)

# clean up
remove(m3, m3_old, summ_m3)









### M3.bin: Groups as Binary with Task Distinction ##########################################################
### Predictor(s): Time, Group per Task (as binary)
### Random Effect(s): per subject within group

m3.bin_old = bam(score ~ 
                   
                   # smooths for reference level LANG
                   s(Time, by = Tasks) +
                   
                   # smooths for ACTV as ordered factors
                   s(Time, by = isACTVissWM) +
                   s(Time, by = isACTVisrtWM) +
                   s(Time, by = isACTVisrtDivAtt) +
                   s(Time, by = isACTVisrtAlert) +
                   s(Time, by = isACTVissRWT) +
                   
                   # smooths for PASV as ordered factors
                   s(Time, by = isPASVissWM) +
                   s(Time, by = isPASVisrtWM) +
                   s(Time, by = isPASVisrtDivAtt) +
                   s(Time, by = isPASVisrtAlert) +
                   s(Time, by = isPASVissRWT) +
                   
                   # random effects for subject and task in LANG
                   s(Time, userCode, bs = "fs", m = 1),
                 data = df_overall, family = "scat", discrete = T, nthreads = 7)

r1 = start_value_rho(m3.bin_old)

m3.bin = bam(score ~ 
               # smooths for reference level LANG
               s(Time, by = Tasks) +
               
               # smooths for ACTV as ordered factors
               s(Time, by = isACTVissWM) +
               s(Time, by = isACTVisrtWM) +
               s(Time, by = isACTVisrtDivAtt) +
               s(Time, by = isACTVisrtAlert) +
               s(Time, by = isACTVissRWT) +
               
               # smooths for PASV as ordered factors
               s(Time, by = isPASVissWM) +
               s(Time, by = isPASVisrtWM) +
               s(Time, by = isPASVisrtDivAtt) +
               s(Time, by = isPASVisrtAlert) +
               s(Time, by = isPASVissRWT) +
               
               # random effects for subject and task in LANG
               s(Time, userCode, bs = "fs", m = 1),
             
             data = df_overall, family = "scat", rho = r1,  AR.start = df_overall$start.event,
             discrete = T, nthreads = 7)

summ_m3.bin = summary(m3.bin)

## Save to file
save(m3.bin, file = paste(outputFolder, "tables_and_summaries", "m3_bin.rda", sep = "/"))
save(summ_m3.bin, file = paste(outputFolder, "tables_and_summaries", "summ_m3_bin.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "m3_bin.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_m3_bin.rda", sep = "/"))

# Model criticism
qqp(resid(m3.bin))
acf_resid(m3.bin)

# clean up
remove(m3.bin, m3.bin_old, summ_m3.bin)













### M4_te: Baseline as Predictor using te() ##########################################################
### Predictor(s): Time, Group (as ordered), Baseline Level
### Random Effect(s): per subject within group

m4_te_old = bam(score ~ 
                  te(Time, initLevel) +
                  te(Time, initLevel, by = isACTV) +
                  te(Time, initLevel, by = isPASV) +
                  s(Time, userCode, bs = "fs", m = 1),
                data = df_overall, discrete = T, nthreads = 7)

r1 = start_value_rho(m4_te_old)

m4_te = bam(score ~ 
              te(Time, initLevel) +
              te(Time, initLevel, by = isACTV) +
              te(Time, initLevel, by = isPASV) +
              s(Time, userCode, bs = "fs", m = 1),
            data = df_overall, rho = r1,  AR.start = df_overall$start.event,
            discrete = T, nthreads = 7)


summ_m4_te = summary(m4_te)


## Save to file
save(m4_te, file = paste(outputFolder, "tables_and_summaries", "m4_te.rda", sep = "/"))
save(summ_m4_te, file = paste(outputFolder, "tables_and_summaries", "summ_m4_te.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "m4_te.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_m4_te.rda", sep = "/"))


## Clean up
remove(m4_te, summ_m4_te)















## M4_threeOut (without outliers < -3.5SD)  ##################################################################
out = which(abs(df_overall$initLevel) > 3.5)
df_overall2 = df_overall[-out,] 

m4_threeOut_old = bam(score ~ 
                  te(Time, initLevel) +
                  te(Time, initLevel, by = isACTV) +
                  te(Time, initLevel, by = isPASV) +
                  s(Time, userCode, bs = "fs", m = 1),
                data = df_overall2, discrete = T, nthreads = 7)

r1 = start_value_rho(m4_threeOut_old)

m4_threeOut = bam(score ~ 
              te(Time, initLevel) +
              te(Time, initLevel, by = isACTV) +
              te(Time, initLevel, by = isPASV) +
              s(Time, userCode, bs = "fs", m = 1),
            data = df_overall2, rho = r1,  AR.start = df_overall2$start.event,
            discrete = T, nthreads = 7)


summ_m4_threeOut = summary(m4_threeOut)

## Save to file
save(m4_threeOut, file = paste(outputFolder, "tables_and_summaries", "m4_threeOut.rda", sep = "/"))
save(summ_m4_threeOut, file = paste(outputFolder, "tables_and_summaries", "summ_m4_threeOut.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "m4_threeOut.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_m4_threeOut.rda", sep = "/"))

## Clean up
remove(m4_threeOut, summ_m4_threeOut)






















## Model Comparison M2 vs. M4 ##################################################################
# Outliers < -3.5SD are removed from both models
# Both models use binary curves

df_overall3 = df_overall %>%
  filter(initLevel > -3.5 )


m2.alt_old = bam(score ~ 
                   isACTV0 +
                   isPASV0 +
                   s(Time) +
                   s(Time, by = isACTV0) +
                   s(Time, by = isPASV0) +
                   s(Time, userCode, bs = "fs", m = 1),
                 data = df_overall3, discrete = T, nthreads = 7,
                 select = T)

r1 = start_value_rho(m2.alt_old)
remove(m2.alt_old)

m2.alt = bam(score ~ 
               isACTV0 +
               isPASV0 +
               s(Time) +
               s(Time, by = isACTV0) +
               s(Time, by = isPASV0) +
               s(Time, userCode, bs = "fs", m = 1),
             data = df_overall3, rho = r1,  AR.start = df_overall3$start.event,
             discrete = T, select = T, nthreads = 7)

m4.alt_old = bam(score ~ 
                      te(Time, initLevel) +
                      te(Time, initLevel, by = isACTV) +
                      te(Time, initLevel, by = isPASV) +
                      s(Time, userCode, bs = "fs", m = 1),
                    data = df_overall3, discrete = T, nthreads = 7,
                 select = T)

r1 = start_value_rho(m4.alt_old)
remove(m4.alt_old)

m4.alt = bam(score ~ 
                  te(Time, initLevel) +
                  te(Time, initLevel, by = isACTV) +
                  te(Time, initLevel, by = isPASV) +
                  s(Time, userCode, bs = "fs", m = 1),
                data = df_overall3, rho = r1,  select = T, AR.start = df_overall3$start.event,
                discrete = T, nthreads = 7)

# save to file
save(m2.alt, file = paste(outputFolder, "tables_and_summaries", "m2_threeout_alt.rda", sep = "/"))
save(m4.alt, file = paste(outputFolder, "tables_and_summaries", "m4_threeout_alt.rda", sep = "/"))

# load
load(file = paste(outputFolder, "tables_and_summaries", "m2_threeout_alt.rda", sep = "/"))
load(file = paste(outputFolder, "tables_and_summaries", "m4_threeout_alt.rda", sep = "/"))

compareML(m2.alt, m4.alt)








### M5_threeOut: Same as M4_threeOut but with Task Distinction ##########################################################
### Predictor(s): Time, Group per Task (as ordered), Baseline Level
### Random Effect(s): per subject within group

df_overall3 = df_overall %>%
  filter(initLevel > -3.5 )


m5_threeOut_old = bam(score ~ 
                      te(Time, initLevel, by = Tasks) +
                      te(Time, initLevel, by = isACTVissWM) +
                      te(Time, initLevel, by = isACTVisrtWM) +
                      te(Time, initLevel, by = isACTVisrtDivAtt) +
                      te(Time, initLevel, by = isACTVisrtAlert) +
                      te(Time, initLevel, by = isACTVissRWT) +
                      
                      te(Time, initLevel, by = isPASVissWM) +
                      te(Time, initLevel, by = isPASVisrtWM) +
                      te(Time, initLevel, by = isPASVisrtDivAtt) +
                      te(Time, initLevel, by = isPASVisrtAlert) +
                      te(Time, initLevel, by = isPASVissRWT) +
                      
                      s(Time, userCode, bs = "fs", m = 1),
                    data = df_overall3, discrete = T, nthreads = 7)

r1 = start_value_rho(m5_threeOut_old)

m5_threeOut = bam(score ~ 
                  te(Time, initLevel, by = Tasks) +
                  te(Time, initLevel, by = isACTVissWM) +
                  te(Time, initLevel, by = isACTVisrtWM) +
                  te(Time, initLevel, by = isACTVisrtDivAtt) +
                  te(Time, initLevel, by = isACTVisrtAlert) +
                  te(Time, initLevel, by = isACTVissRWT) +
                  
                  te(Time, initLevel, by = isPASVissWM) +
                  te(Time, initLevel, by = isPASVisrtWM) +
                  te(Time, initLevel, by = isPASVisrtDivAtt) +
                  te(Time, initLevel, by = isPASVisrtAlert) +
                  te(Time, initLevel, by = isPASVissRWT) +
                  s(Time, userCode, bs = "fs", m = 1),
                data = df_overall3, rho = r1,  AR.start = df_overall3$start.event,
                discrete = T, nthreads = 7)


summ_m5_threeOut = summary(m5_threeOut)

## Save to file
save(m5_threeOut, file = paste(outputFolder, "tables_and_summaries", "m5_threeOut.rda", sep = "/"))
save(summ_m5_threeOut, file = paste(outputFolder, "tables_and_summaries", "summ_m5_threeOut.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "m5_threeOut.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_m5_threeOut.rda", sep = "/"))


## Clean up
remove(m5_threeOut, summ_m5_threeOut)





















############################################################################################################
############################################################################################################



### Socio-Affect Model with Group as Group ##########################################################
### Predictor(s): Time, ExpGroup
### Random Effect(s): per subject within group

# overall cognition as model over the entire dataset
df$userCode = as.factor(df$userCode)
m1 = bam(socioAffect ~ 
           Group +
           s(Time, by = Group) +
           s(Time, userCode, by = Group, bs = "fs", m = 1),
         data = df, family = "scat", discrete = T, nthreads = 7)
summ_m1 = summary(m1)

## Save to file
save(m1, file = paste(outputFolder, "tables_and_summaries", "socioAffect_gamm.rda", sep = "/"))
save(summ_m1, file = paste(outputFolder, "tables_and_summaries", "summ_socioAffect_gamm.rda", sep = "/"))

## Clean up
remove(m1, summ_m1)



### Socio-Affect Visualization of GAMM #########################################################


## Prepare data
# For boxplot
socioDF = df %>%
  select(userCode, Group, socioAffect) %>%
  group_by(userCode, Group) %>%
  summarise(socioAffect = mean(socioAffect, na.rm = T))


load(paste(outputFolder, "tables_and_summaries", "socioAffect_gamm.rda", sep = "/")) # Model is run in idDiffs.R
# model is called m1

# Make dataset for model prediction
# Time column
Time <-  seq(min(df$Time),
             max(df$Time), length.out=200)

# start df with Time
plot.df = data.frame( Time = rep(Time, nlevels(df_overall$userCode)))

# add userCodes
plot.df$userCode = rep(levels(df_overall$userCode), each = 200)

# add Group
plot.df <- plot.df %>%
  rowwise() %>%
  mutate(Group = unique(df_overall$Group[df_overall$userCode == userCode]))

# add prediction
plot.df = cbind(plot.df, as.data.frame(mgcv::predict.bam(m1, plot.df, se.fit = TRUE)))

# remove random effects by calculating the mean of CIs and fit over subjects
plot.df = plot.df %>%
  group_by(Group, Time) %>%
  summarise(fit = mean(fit),
            se.fit = mean(se.fit))

plot.df$Group = relevel(plot.df$Group, ref = "PASV")

## Make boxplot and smooth plot

png(filename = paste(outputFolder, "figures", "socioAffect.png", sep = "/"), 
    height = 8, width = 20, units = "cm", res = 100)
      # boxplot
      p1 <- socioDF %>%
        ggplot(aes(x = Group, y = socioAffect, fill = Group)) +
        geom_boxplot() +
        theme_bw() +
        xlab("") +
        ylab("Socio-Affect") +
        scale_fill_brewer(palette = "Paired", limits = levels(socioDF$Group), guide = F) +
        ggtitle("Mean Socio-Affect per Group") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ylim(0,100) +
        theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
              axis.title.x = element_text(family = "Times"),
              axis.title.y = element_text(family = "Times"),
              legend.text = element_text(family = "Times"),
              legend.title = element_text(family = "Times"))
      
      # line plot
      p2  <- plot.df %>%
        ggplot(aes(x = Time)) +
        geom_line(aes(y = fit, col = Group), size = 1) +
        geom_ribbon(aes(ymax = fit + 1.96*se.fit,
                        ymin = fit-1.96*se.fit,
                        col = Group,
                        linetype = NA),
                    alpha = 0.05) +
        ggtitle("Socio-Affect over Time") +
        ylab("Predicted") +
        scale_color_brewer(palette = "Paired") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
              axis.title.x = element_text(family = "Times"),
              axis.title.y = element_text(family = "Times"),
              legend.text = element_text(family = "Times"),
              legend.title = element_text(family = "Times"))
      
      grid.arrange(p1, p2, layout_matrix = rbind(c(1,1,1,2,2,2,2)))
dev.off()




### Socio-Affect Model with Group as OF ##########################################################
### Predictor(s): Time, ExpGroup
### Random Effect(s): per subject within group
m1a = bam(socioAffect ~ 
               isACTV0 +
               isPASV0 +
               s(Time) +
               s(Time, by = isACTV0) +
               s(Time, by = isPASV0) +
               s(Time, userCode, bs = "fs", m = 1),
             family = "scat", data = df_overall, discrete = T, nthreads = 7)

# check
qqp(resid(m1a))
acf_resid(m1a)

summ_m1a = summary(m1a)


## Save to file
save(m1a, file = paste(outputFolder, "tables_and_summaries", "socioAffect_OFgamm.rda", sep = "/"))
save(summ_m1a, file = paste(outputFolder, "tables_and_summaries", "summ_socioAffect_OFgamm.rda", sep = "/"))

## Load from file
load(paste(outputFolder, "tables_and_summaries", "socioAffect_OFgamm.rda", sep = "/"))
load(paste(outputFolder, "tables_and_summaries", "summ_socioAffect_OFgamm.rda", sep = "/"))

## Clean up
remove(m1a, summ_m1a)









############################################################################################################
############################################################################################################


## THE GAMM Plot(s) #### 
  ### For overall cognition
  ##  Make a new dataset consisting of predicted values
    load(paste(outputFolder, "tables_and_summaries", "m1.rda", sep = "/"))


    p <- get_predictions(m1, cond = list(Group = "PASV",
                                         Time = c(0:28)), 
                         print.summary = F,
                         rm.ranef = T)
    a <- get_predictions(m1, cond = list(Group = "ACTV",
                                         Time = c(0:28)), 
                         print.summary = F,
                         rm.ranef = T)
    
    l <- get_predictions(m1, cond = list(Group = "LANG",
                                         Time = c(0:28)), 
                         print.summary = F,
                         rm.ranef = T)
    
    plot.df = rbind.data.frame(l,p,a)
    
    plot.df = plot.df %>%
      select(-c(rm.ranef, userCode))

    plot.df$Group = relevel(plot.df$Group, ref = "PASV")

  ## Plot and save as file
  png(filename = paste(outputFolder, "figures"," GroupGAMM.png", sep = "/"), 
      height = 7, width = 12, units = "cm", res = 100)
  
    plot <- plot.df %>%
      ggplot(aes(x = Time)) +
      geom_line(aes(y = fit, col = Group), size = 1) +
      geom_ribbon(aes(ymax = fit + CI,
                      ymin = fit- CI,
                      col = Group,
                      linetype = NA),
                  alpha = 0.05) +
      ggtitle("Overall Cognition") +
      ylab("Predicted") +
      scale_color_brewer(palette = "Paired") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
    plot
  
  dev.off()
  
  # clean up
  remove(plot.df, plot)
  
  
  ## Loop over all tasks and make the above plot for each of them
  
  # Make a list of the plots
  plotList = littleGAMPlots(df)
  
  # save plot arrangement to file
    png(filename = paste(outputFolder, "figures"," littleGroupGAMMs.png", sep = "/"), 
        height = 12, width = 20, units = "cm", res = 100)
    
      emptyCell = grobTree(rectGrob(gp=gpar(col=NA)), textGrob(""))
      grid.arrange(plotList[[1]], plotList[[2]], plotList[[3]], plotList[[4]], plotList[[5]], emptyCell, 
                   layout_matrix = rbind(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),
                                         c(4,4,4,4,4,5,5,5,5,5,5,5,6,6,6)))

    dev.off()
    
    # Clean up
    remove(plotList)



    

    
##### Final Prediction Plots M4_threeOUt ################################################################

load(paste(outputFolder, "tables_and_summaries", "m4_threeOut.rda", sep = "/"))

### Get prediction for initLevel ranging from -2 to 1 for each group

plotDF = data.frame(Group = c(), initLevel = factor(), Time = c(), fit = c())
  ## add LANG estimates
    # initLevel = 1
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 0, isPASV = 0, Time = seq(0,28, length.out = 200), initLevel = 1),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "LANG", initLevel = "1", Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = 0
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 0, isPASV = 0, Time = seq(0,28, length.out = 200), initLevel = 0),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "LANG",  initLevel = "0",Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = -1
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 0, isPASV = 0, Time = seq(0,28, length.out = 200), initLevel = -1),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "LANG",  initLevel = "-1",Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = -2
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 0, isPASV = 0, Time = seq(0,28, length.out = 200), initLevel = -2),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "LANG",  initLevel = "-2",Time = thisPred$Time,
                                                 fit = thisPred$fit))


    ## add PASV estimates
    # initLevel = 1
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 0, isPASV = 1, Time = seq(0,28, length.out = 200), initLevel = 1),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "PASV", initLevel = "1", Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = 0
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 0, isPASV = 1, Time = seq(0,28, length.out = 200), initLevel = 0),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "PASV",  initLevel = "0",Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = -1
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 0, isPASV = 1, Time = seq(0,28, length.out = 200), initLevel = -1),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "PASV",  initLevel = "-1",Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = -2
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 0, isPASV = 1, Time = seq(0,28, length.out = 200), initLevel = -2),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "PASV",  initLevel = "-2",Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    
    
    ## add ACTV estimates
    # initLevel = 1
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 1, isPASV = 0,  Time = seq(0,28, length.out = 200), initLevel = 1),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "ACTV", initLevel = "1",Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = 0
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 1, isPASV = 0, Time = seq(0,28, length.out = 200), initLevel = 0),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "ACTV",  initLevel = "0",Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = -1
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 1, isPASV = 0, Time = seq(0,28, length.out = 200), initLevel = -1),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "ACTV", initLevel = "-1", Time = thisPred$Time,
                                                 fit = thisPred$fit))
    
    # initLevel = -2
    thisPred = get_predictions(m4_threeOut,
                               rm.ranef = T,
                               cond = list(isACTV = 1, isPASV = 0,  Time = seq(0,28, length.out = 200), initLevel = -2),
                               print.summary = F)
    plotDF = rbind.data.frame(plotDF, data.frame(Group = "ACTV", initLevel = "-2",Time = thisPred$Time,
                                                 fit = thisPred$fit))

    
    
### Plot
    
png(filename = paste(outputFolder, "figures"," finalPredictions.png", sep = "/"), 
    height = 9, width = 12, units = "cm", res = 100)    
  
  plot = 
    plotDF %>%
    ggplot(aes(x = Time)) +
    geom_line(aes(y = fit, col = initLevel, lty = Group)) +
    theme_bw() + 
    ggtitle("Estimated Overall Trajectories \n by Baseline Level") +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot
dev.off()


