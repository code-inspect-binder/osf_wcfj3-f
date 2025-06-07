###########################################################################
### Skript for the export of all plots and tables included in the publication
###########################################################################


### Table 2: Participant Characteristics ############################################################
  Groups = c("LANG", "ACTV", "PASV")


  # sample size
  load(file = paste(inputFolder, "id_df.rda", sep = "/"))
  
  sizeAll = c(length(unique(id_df$userCode))) # all participants
  sizeF = c(length(unique(id_df$userCode[id_df$gend == 1]))) # female
  
    for (i in 1:length(Groups)) {
      sizeAll[i+1] = length(unique(id_df$userCode[id_df$Group == Groups[i]]))
      sizeF[i+1] = c(length(unique(id_df$userCode[(id_df$gend == 1) & (id_df$Group == Groups[i])]))) # female
    }
  sizeM = c(sizeAll- sizeF) # male
  
  
  
  # age
  meanAge = mean(id_df$age) 
  sdAge = sd(id_df$age) 
  
  for (i in 1:length(Groups)) {
    meanAge[i+1] = mean(id_df$age[id_df$Group == Groups[i]])
    sdAge[i+1] = sd(id_df$age[id_df$Group == Groups[i]])
  }
  
  
  
  
  # education
  meanEdu = mean(id_df$educ_y) 
  sdEdu = sd(id_df$educ_y) 
  
  for (i in 1:length(Groups)) {
    meanEdu[i+1] = mean(id_df$educ_y[id_df$Group == Groups[i]])
    sdEdu[i+1] = sd(id_df$educ_y[id_df$Group == Groups[i]])
  }
  
  
  
  # multilingualism
  meanBLP = mean(id_df$BLP) 
  sdBLP = sd(id_df$BLP) 
  
  for (i in 1:length(Groups)) {
    meanBLP[i+1] = mean(id_df$BLP[id_df$Group == Groups[i]])
    sdBLP[i+1] = sd(id_df$BLP[id_df$Group == Groups[i]])
  }
  
  
  
  # number of regular activities
  meanAct = mean(id_df$Act_total) 
  sdAct = sd(id_df$Act_total) 
  
  for (i in 1:length(Groups)) {
    meanAct[i+1] = mean(id_df$Act_total[id_df$Group == Groups[i]])
    sdAct[i+1] = sd(id_df$Act_total[id_df$Group == Groups[i]])
  }
  
  
  
  # average socio-affect
  meanMot = mean(df$socioAffect, na.rm = T) 
  sdMot = sd(df$socioAffect, na.rm = T) 
  
  for (i in 1:length(Groups)) {
    meanMot[i+1] = mean(df$socioAffect[df$Group == Groups[i]], na.rm = T)
    sdMot[i+1] = sd(df$socioAffect[df$Group == Groups[i]], na.rm = T)
  }
  
  # clean up
  remove(Groups, meanAct, meanAge, meanBLP, meanEdu, meanMot, 
         sdAct, sdAge, sdBLP, sdEdu, sdMot, sizeAll, sizeF, sizeM)


  
  


  
  
  
  
  
  
  
  

### Table ST2: Descriptive Statistics ############################################################
  thisDat = df %>%
    select(userCode, Group, rtAlert, sDivAtt, rtDivAtt, sWM, rtWM, sRWT, socioAffect) %>%
    tidyr::pivot_longer(rtAlert:socioAffect,
                        names_to = "Variable",
                        values_to = "Score")
  
  # Make Variable a factor
  thisDat$Variable = as.factor(thisDat$Variable)
  
  # Relevel Group for coloring
  thisDat$Group = relevel(thisDat$Group, ref = "ACTV")
  thisDat$Group = relevel(thisDat$Group, ref = "PASV")
  
  # Rename factor levels
  levels(thisDat$Variable) = c("Alertness RT [log]", "Divided Attention RT [log]", "Working Memory RT [log]",
                               "Divided Attention Acc.", "Socio-Affect", "Verbal Fluency", "Working Memory Acc.")
  
  thisDat$Variable <- ordered(thisDat$Variable, levels = c("Working Memory Acc.", "Working Memory RT [log]",
                                                       "Divided Attention Acc.", "Divided Attention RT [log]",
                                                       "Alertness RT [log]", "Verbal Fluency",
                                                       "Socio-Affect"))
  N_vect = thisDat %>%
    group_by(Group, Variable) %>%
    filter(is.na(Score) == F) %>%
    count()
  
  sumDF = thisDat %>%
    group_by(Variable, Group) %>%
    summarise(N = 0,
              Mean = mean(Score, na.rm = T),
              Median = median(Score, na.rm = T),
              "Std. Dev" = sd(Score, na.rm = T),
              Min = min(Score, na.rm = T),
              Max = max(Score, na.rm = T)) 
  

  
  sumDF = full_join(sumDF, N_vect, by = c("Variable", "Group"))
  
  sumDF = sumDF %>%
    dplyr::select(Variable, Group, N = n, Mean, Median, "Std. Dev", Min, Max) %>%
    ungroup()
  
  sumDF$Mean = format(round(sumDF$Mean,2), nsmall = 2)
  sumDF$Median = format(round(sumDF$Median,2), nsmall = 2)
  sumDF$`Std. Dev` = format(round(sumDF$`Std. Dev`,2), nsmall = 2)
  sumDF$Max = format(round(sumDF$Max,2), nsmall = 2)
  sumDF$Min = format(round(sumDF$Min,2), nsmall = 2)
  
  sumDF$Variable = as.character(sumDF$Variable)
  sumDF$Group = as.character(sumDF$Group)

  stargazer(sumDF, type = "html", out = paste(outputFolder, "tables_and_summaries", "Cogn_desc.doc", sep = "/"),
            summary = F, rownames = F)

  
  
### Table 3: Summary Output m2 LANG vs. ACTV/PASV ############################################################

# Load summary (See GAMM.R for respective script)
load(file = paste(outputFolder, "tables_and_summaries", "summ_m2.rda", sep = "/"))

# Save to file  
sink(paste(outputFolder, "tables_and_summaries", "m2_summary.txt", sep = "/"))

  summ_m2

sink() 


### Table ST1: Summary Output m3 with Task Distinction ############################################################

load(file = paste(outputFolder, "tables_and_summaries", "summ_m3.rda", sep = "/"))
sink(paste(outputFolder, "tables_and_summaries", "m3_summary.txt", sep = "/"))
  summ_m3
sink() 





### Table ST3: Summary Output 4 with Baseline as Predictor ############################################################

load(file = paste(outputFolder, "tables_and_summaries", "summ_m4_threeOut.rda", sep = "/"))
sink(paste(outputFolder, "tables_and_summaries","m4_threeOut_summary.txt", sep = "/"))
  summ_m4_threeOut
sink() 




### Table 4: Summary Output 5 with Baseline as Predictor and Tasks ############################################################

load(file = paste(outputFolder, "tables_and_summaries", "summ_m5_threeOut.rda", sep = "/"))
sink(paste(outputFolder, "tables_and_summaries","m5_threeOut_summary.txt", sep = "/"))
  summ_m5_threeOut
sink() 









### Figure SF1: Descriptive Stats ############################################################

thisDat <- df_overall %>%
  group_by(Tasks) %>%
  mutate(outlier.high = score > quantile(score, .75, na.rm = TRUE) + 1.5*IQR(score, na.rm = TRUE),
         outlier.low = score < quantile(score, .25, na.rm = TRUE) - 1.5*IQR(score, na.rm = TRUE)) %>%
  ungroup()

thisDat$outlier.color <- NA
thisDat$outlier.color[thisDat$outlier.high] <- "red"
thisDat$outlier.color[thisDat$outlier.low] <- "steelblue"
levels(thisDat$Tasks) = c("WM Acc.", "WM RT", "Div.Att. RT", "Alertness RT", "Verb. Fluency")

# Draw plot

ggplot(thisDat) +
  aes(x = Tasks, y = score) +
  geom_violin(aes(group = Tasks, fill = Tasks),
              trim = FALSE) +
  geom_boxplot(aes(group = Tasks), width = 0.15, outlier.shape = NA) +
  geom_point(data = function(x) dplyr::filter_(x, ~ outlier.high | outlier.low), position = position_jitter(w = 0.1, h = 0.05), color = "darkblue", alpha = 0.5) + # Outliers
  stat_summary(mapping = aes(group = Tasks, fill = Tasks), fun = mean, geom = "point", shape = 23, size = 2.5) +
  scale_fill_brewer(palette = "Blues") + #Alternatives: "Dark2", "RdBu"
  scale_x_discrete() +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Z-Score") +
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman",face = "bold"),
        axis.title.x = element_text(family = "Times New Roman"),
        axis.title.y = element_text(family = "Times New Roman"),
        axis.text.x.bottom = element_text(family = "Times New Roman"),
        axis.text.y.left = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman")) +
  ggtitle("Distribution of Cognitive Variables")

ggsave(paste(outputFolder, "figures", "Desc_sexy_plot.tiff", sep = "/"), dpi = 600, width = 15, height = 10, units = "cm")














### Figure 1: m1 overall Group Effect ############################################################
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

plot.df$Group = relevel(plot.df$Group, ref = "ACTV")

## Plot and save as file
tiff(filename = paste(outputFolder, "figures","GroupGAMM.tiff", sep = "/"), 
    height = 7, width = 12, units = "cm", res = 300)

plot <- plot.df %>%
  ggplot(aes(x = Time)) +
  geom_line(aes(y = fit, lty = Group), size = 1) +
  geom_ribbon(aes(ymax = fit + CI,
                  ymin = fit- CI,
                  group = Group),
              alpha = 0.05) +
  ggtitle("Overall Cognition") +
  ylab("Predicted") +
  scale_linetype_manual(values=c("dashed", "solid", "dotted"))+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))  +   
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman",face = "bold"),
        axis.title.x = element_text(family = "Times New Roman"),
        axis.title.y = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman"))
  

plot

dev.off()

# clean up
remove(plot.df, plot)



















### Figure 2: m3 Group per Task ############################################################
# Make a list of the plots
plotList = littleGAMPlots2(df)

# save plot arrangement to file
png(filename = paste(outputFolder, "figures"," littleGroupGAMMs_bw.png", sep = "/"), 
    height = 12, width = 20, units = "cm", res = 100)

emptyCell = grobTree(rectGrob(gp=gpar(col=NA)), textGrob(""))
grid.arrange(plotList[[1]], plotList[[2]], plotList[[3]], plotList[[4]], plotList[[5]], emptyCell, 
             layout_matrix = rbind(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),
                                   c(4,4,4,4,4,5,5,5,5,5,5,5,6,6,6)))

dev.off()

# Clean up
remove(plotList)








### Figure SF2: m3 Individuals per Task ############################################################
# Make a list of the plots
plotList = list()

taskNames = c("Working Memory Acc.", "Working Memory RT", "Divided Attention RT",
              "Alertness RT", "Verbal Fluency")

varNames = c("sWM", "rtWM", "rtDivAtt", "rtAlert", "sRWT")

## For some reason, this loop does not work, but the code below does the same thing.
# for (i in 1:length(varNames)) {
#   p = df %>%
#     filter(Time < 29) %>%
#     ggplot(aes(x = Time)) +
#     geom_smooth(aes(y = eval(as.name(varNames[i])), lty = Group, color = userCode), size = 1, method = "gam", se = F) +
#     scale_color_hue(l = 70, c = 150, guide = F) +
#     ggtitle(taskNames[i]) +
#     ylab("Predicted") +
#     scale_linetype_manual(values = c("dotted", "dashed", "solid"), guide = ifelse(i < 5, F, "legend")) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
#           axis.title.x = element_text(family = "Times"),
#           axis.title.y = element_text(family = "Times"),
#           legend.text = element_text(family = "Times"),
#           legend.title = element_text(family = "Times"))
#   
#   plotList[[i]] = p
# }

p1 = df %>%
  filter(Time < 29) %>%
  ggplot(aes(x = Time)) +
  geom_smooth(aes(y = eval(as.name(varNames[1])), lty = Group, color = userCode), size = 1, method = "gam", se = F) +
  scale_color_hue(l = 70, c = 150, guide = F) +
  ggtitle(taskNames[1]) +
  ylab("Predicted") +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"), guide = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
        axis.title.x = element_text(family = "Times"),
        axis.title.y = element_text(family = "Times"),
        legend.text = element_text(family = "Times"),
        legend.title = element_text(family = "Times"))

p2 = df %>%
  filter(Time < 29) %>%
  ggplot(aes(x = Time)) +
  geom_smooth(aes(y = eval(as.name(varNames[2])), lty = Group, color = userCode), size = 1, method = "gam", se = F) +
  scale_color_hue(l = 70, c = 150, guide = F) +
  ggtitle(taskNames[2]) +
  ylab("Predicted") +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"), guide = F ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
        axis.title.x = element_text(family = "Times"),
        axis.title.y = element_text(family = "Times"),
        legend.text = element_text(family = "Times"),
        legend.title = element_text(family = "Times"))

p3 = df %>%
  filter(Time < 29) %>%
  ggplot(aes(x = Time)) +
  geom_smooth(aes(y = eval(as.name(varNames[3])), lty = Group, color = userCode), size = 1, method = "gam", se = F) +
  scale_color_hue(l = 70, c = 150, guide = F) +
  ggtitle(taskNames[3]) +
  ylab("Predicted") +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"), guide = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
        axis.title.x = element_text(family = "Times"),
        axis.title.y = element_text(family = "Times"),
        legend.text = element_text(family = "Times"),
        legend.title = element_text(family = "Times"))

p4 = df %>%
  filter(Time < 29) %>%
  ggplot(aes(x = Time)) +
  geom_smooth(aes(y = eval(as.name(varNames[4])), lty = Group, color = userCode), size = 1, method = "gam", se = F) +
  scale_color_hue(l = 70, c = 150, guide = F) +
  ggtitle(taskNames[4]) +
  ylab("Predicted") +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"), guide = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
        axis.title.x = element_text(family = "Times"),
        axis.title.y = element_text(family = "Times"),
        legend.text = element_text(family = "Times"),
        legend.title = element_text(family = "Times"))

p5 = df %>%
  filter(Time < 29) %>%
  ggplot(aes(x = Time)) +
  geom_smooth(aes(y = eval(as.name(varNames[5])), lty = Group, color = userCode), size = 1, method = "gam", se = F) +
  scale_color_hue(l = 70, c = 150, guide = F) +
  ggtitle(taskNames[5]) +
  ylab("Predicted") +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"), guide = ifelse(i < 5, F, "legend")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
        axis.title.x = element_text(family = "Times"),
        axis.title.y = element_text(family = "Times"),
        legend.text = element_text(family = "Times"),
        legend.title = element_text(family = "Times"))


# save plot arrangement to file
png(filename = paste(outputFolder, "figures"," littleGroupGAMMs_indi.png", sep = "/"), 
    height = 36, width = 25, units = "cm", res = 100)

emptyCell = grobTree(rectGrob(gp=gpar(col=NA)), textGrob(""))
grid.arrange(p1, p2, p3, p4, p5, emptyCell, 
             layout_matrix = rbind(c(1,1,1,1,1,2,2,2,2,2),
                                   c(3,3,3,3,3,4,4,4,4,4),
                                   c(5,5,5,5,5,5,6,6,6,6)))

dev.off()

# Clean up
remove(plotList)





### Figure 3: Correlation Baseline Level and ID Variables ############################################################
load(file = paste(inputFolder, "id_df.rda", sep = "/"))

corrDF = melt(id_df, id.vars = c("userCode", "initLevel"),
              measure.vars = c("age", "educ_y",  "BLP", "Act_total", "socioAffect_init"),
              variable.name = "Measure",
              value.name = "Value")

levels(corrDF$Measure) = c("Age", "Education", "Multilingualism", "Number of Hobbies", "Initial Socio-Affect")

# Make a dot and a line color column depending on the significance level
# Check significance level and - if significant - add respective color to new column

ageR = data.frame(dotCol = rep(ifelse(summary(lm(initLevel ~ age, data = id_df))$coefficients[2,4]< 0.05,
                                      "khaki3", "lightgrey"), 
                               length(unique(id_df$userCode))),
                  lineCol = rep(ifelse(summary(lm(initLevel ~ age, data = id_df))$coefficients[2,4]< 0.05,
                                       "cyan4", "darkgrey"), 
                                length(unique(id_df$userCode))))
educR = data.frame(dotCol = rep(ifelse(summary(lm(initLevel ~ educ_y, data = id_df))$coefficients[2,4]< 0.05,
                                       "khaki3", "lightgrey"), 
                                length(unique(id_df$userCode))),
                   lineCol = rep(ifelse(summary(lm(initLevel ~ educ_y, data = id_df))$coefficients[2,4]< 0.05,
                                        "cyan4", "darkgrey"), 
                                 length(unique(id_df$userCode))))
blpR = data.frame(dotCol = rep(ifelse(summary(lm(initLevel ~ BLP, data = id_df))$coefficients[2,4]< 0.05,
                                      "khaki3", "lightgrey"), 
                               length(unique(id_df$userCode))),
                  lineCol = rep(ifelse(summary(lm(initLevel ~ BLP, data = id_df))$coefficients[2,4]< 0.05,
                                       "cyan4", "darkgrey"), 
                                length(unique(id_df$userCode))))
actR = data.frame(dotCol = rep(ifelse(summary(lm(initLevel ~ Act_total, data = id_df))$coefficients[2,4]< 0.05,
                                      "khaki3", "lightgrey"), 
                               length(unique(id_df$userCode))),
                  lineCol = rep(ifelse(summary(lm(initLevel ~ Act_total, data = id_df))$coefficients[2,4]< 0.05,
                                       "cyan4", "darkgrey"), 
                                length(unique(id_df$userCode))))
motivR = data.frame(dotCol = rep(ifelse(summary(lm(initLevel ~ socioAffect_init, data = id_df))$coefficients[2,4]< 0.05,
                                        "khaki3", "lightgrey"), 
                                 length(unique(id_df$userCode))),
                    lineCol = rep(ifelse(summary(lm(initLevel ~ socioAffect_init, data = id_df))$coefficients[2,4]< 0.05,
                                         "cyan4", "darkgrey"), 
                                  length(unique(id_df$userCode))))

tmp = rbind.data.frame(ageR, educR, blpR, actR, motivR)
corrDF = cbind(corrDF, tmp)

# Get corr.coefficients
cor.test(id_df$age, id_df$initLevel)
cor.test(id_df$BLP, id_df$initLevel)
# pearson's r assumes no outliers --> check
testDat = scale(id_df$BLP)
sort(testDat)
max(testDat) # --> max value is an outlier.
min(testDat)

#again without outlier
cor.test(id_df$BLP[id_df$BLP != max(id_df$BLP)], id_df$initLevel[id_df$BLP != max(id_df$BLP)])


cor.test(id_df$educ_y, id_df$initLevel)
cor.test(id_df$Act_total, id_df$initLevel)
cor.test(id_df$socioAffect_init, id_df$initLevel)

cor.test(id_df$BLP, id_df$educ_y)  
#again without outlier
cor.test(id_df$BLP[id_df$BLP != max(id_df$BLP)], id_df$educ_y[id_df$BLP != max(id_df$BLP)])


# clean up
remove(ageR, educR, motivR, actR, blpR, tmp)

# Plot

tiff(filename = paste(outputFolder, "figures", "corID_initLevel_bw.tiff", sep = "/"), 
     height = 7, width = 25, units = "cm", res = 300)

corrDF %>%
  ggplot(aes(x = Value, y = initLevel)) +
  geom_point(aes(color = dotCol)) +
  geom_smooth(method = "lm", alpha = 0.2, aes(color = lineCol)) +
  facet_grid(~ Measure, scales = "free") +
  theme_bw() + 
  ylab("Cognitive Baseline [z-score]") +
  xlab("") +
  # stat_cor(method = "pearson",
  #          label.x = 0.25, label.y = -2, label.sep = "\n",
  #          digits = 2,
  #          p.digits = 2)  +
  scale_color_manual(values = c("black", "darkgrey", "black", "lightgrey")) +
  guides(color = F) +
  ylim(-2,1) +   
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman",face = "bold"),
        axis.title.x = element_text(family = "Times New Roman"),
        axis.title.y = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman"),
        strip.text.x = element_text(family = "Times New Roman"),
        panel.spacing = unit(1, "lines"))

dev.off()













### Figure SF3: Histogram of Baselines ############################################################

tiff(filename = paste(outputFolder, "figures", "histBaselines.tiff", sep = "/"), 
     height = 9, width = 12, units = "cm", res = 300)

  dat = df_overall %>%
    group_by(userCode, Tasks) %>%
    summarise(initLevel = mean(initLevel))
  hist(dat$initLevel, 50, xlim = c(-4,4),xlab = "Baseline Level", main = "Histogram of Baseline Levels",
       family = "Times")

dev.off()








### Figure 4: Interactions of Time & Baseline ############################################################
load(paste(outputFolder, "tables_and_summaries", "m4_threeOut.rda", sep = "/"))

tiff(filename = paste(outputFolder, "figures", "te_m4_threeOut.tiff", sep = "/"), 
     height = 7, width = 22, units = "cm", res = 300)

  par(mfrow = c(1,3))
  
    # te(Time,initLevel):isPASV
    pvisgam(m4_threeOut, select = 1,
            main = "Smooth over Baseline Level \n at Reference Level (LANG)",
            view = c("Time", "initLevel"),
            zlim = c(-2.00, 2.00),
            print.summary = F,
            ylab = "Baseline Level",
            family = "Times")
  
    
    # te(Time,initLevel):isACTV
    pvisgam(m4_threeOut, select = 2,
            main = "Smooth over Baseline Level \n in ACTV",
            view = c("Time", "initLevel"),
            zlim = c(-1, 1),
            print.summary = F,
            ylab = "Baseline Level",
            family = "Times")
    
    # te(Time,initLevel):isPASV
    pvisgam(m4_threeOut, select = 3,
            main = "Smooth over Baseline Level \n in PASV",
            view = c("Time", "initLevel"),
            zlim = c(-1, 1),
            print.summary = F,
            ylab = "Baseline Level",
            family = "Times")
    
dev.off()






### Figure 5: Interactions of Time & Baseline per Task ############################################################

load(paste(outputFolder, "tables_and_summaries", "m5_threeOut.rda", sep = "/"))

tiff(filename = paste(outputFolder, "figures", "te_m5_threeOut.tiff", sep = "/"), 
     height = 10, width = 26, units = "cm", res = 300)

  par(mfrow = c(2,5), mar = c(4,4,3,2))
  
    # te(Time,initLevel):isPASVissWM 
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 11,
            main = "Working Memory Acc. \n in PASV",
            ylab = "Baseline Level",
            zlim = c(-1.6, 1.6),
            print.summary = F,
            family = "Times",
            hide.label = T)
    
    
    # te(Time,initLevel):isPASVisrtWM 
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 12,
            main = "Working Memory RT \n in PASV",
            ylab = "Baseline Level",
            zlim = c(-7.62, 7.62),
            print.summary = F,
            family = "Times",
            hide.label = T)
    
    # te(Time,initLevel):isPASVisrtDivAtt
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 13,
            main = "Divided Attention RT \n in PASV",
            ylab = "Baseline Level",
            zlim = c(-2.3, 2.3),
            print.summary = F,
            family = "Times",
            hide.label = T)
    
    # te(Time,initLevel):isPASVisrtAlert 
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 14,
            main = "Alertness RT \n in PASV",
            ylab = "Baseline Level",
            zlim = c(-2.7, 2.7),
            print.summary = F,
            family = "Times",
            hide.label = T)
    
    # te(Time,initLevel):isPASVissRWT
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 15,
            main = "Verbal Fluency \n in PASV",
            ylab = "Baseline Level",
            zlim = c(-2.2, 2.2),
            print.summary = F,
            family = "Times",
            hide.label = T)
    
    # te(Time,initLevel):isACTVissWM 
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 6,
            main = "Working Memory Acc. \n in ACTV",
            ylab = "Baseline Level",
            zlim = c(-0.70, 0.70),
            print.summary = F,
            family = "Times",
            hide.label = T)
    
    # te(Time,initLevel):isACTVisrtWM 
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 7,
            main = "Working Memory RT \n in ACTV",
            ylab = "Baseline Level",
            zlim = c(-2.6, 2.6),
            print.summary = F,
            family = "Times",
            hide.label = T)
    
    # te(Time,initLevel):isACTVisrtDivAtt
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 8,
            main = "Divided Attention RT \n in ACTV",
            ylab = "Baseline Level",
            zlim = c(-0.3, 0.3),
            print.summary = F,
            family = "Times",
            hide.label = T,
            color = "gray")
    
    # te(Time,initLevel):isACTVisrtAlert 
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 9,
            main = "Alertness RT \n in ACTV",
            ylab = "Baseline Level",
            zlim = c(-2.2, 2.2),
            print.summary = F,
            family = "Times",
            hide.label = T)
    
    # te(Time,initLevel):isACTVissRWT 
    pvisgam(m5_threeOut, view = c("Time", "initLevel"),
            select = 10,
            main = "Verbal Fluency \n in ACTV",
            ylab = "Baseline Level",
            zlim = c(-2.9, 2.9),
            print.summary = F,
            family = "Times",
            hide.label = T,
            color = "gray")

  dev.off()