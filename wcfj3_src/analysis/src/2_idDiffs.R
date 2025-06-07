### This script assesses whether there were differences in ID variables between groups that could
### explain the differences in intercepts between groups or affect group differences in outcomes.

###### Boxplots of Group Differences in ID Variables #################################################
load(file = paste(inputFolder, "id_dfLong.rda", sep = "/"))

# remove standardized variables
id_dfLong2 = id_dfLong %>%
  filter(Measure %in% c("ageZ", "educ_yZ", "BLPZ", "Act_totalZ") )

id_dfLong2 = droplevels(id_dfLong2)

levels(id_dfLong2$Measure) <- c("Age", "Education", "Multilingualism", "Number of Hobbies")
id_dfLong2$Group = relevel(id_dfLong2$Group, ref = "ACTV")
id_dfLong2$Group = relevel(id_dfLong2$Group, ref = "PASV")

png(filename = paste(outputFolder, "figures", "idDifferences.png", sep = "/"), 
    height = 8, width = 16, units = "cm", res = 100)

id_dfLong2 %>%
  ggplot(aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  facet_grid(~ Measure, scales = "free") +
  theme_bw() +
  ylab("Z-Score") +
  xlab("")+
  scale_fill_brewer(palette = "Paired", limits = levels(id_dfLong2$Group)) 

dev.off()




## Corresponding ANOVAs for publication ####
# Age
dat = id_dfLong2 %>%
  filter(Measure == "Age")
anov = aov(Value ~ Group, dat)
summary(anov)

# Education
dat = id_dfLong2 %>%
  filter(Measure == "Education")
anov = aov(Value ~ Group, dat)
summary(anov)

# Multilingualism
dat = id_dfLong2 %>%
  filter(Measure == "Multilingualism")
anov = aov(Value ~ Group, dat)
summary(anov)

# Number of Hobbies
dat = id_dfLong2 %>%
  filter(Measure == "Number of Hobbies")
anov = aov(Value ~ Group, dat)
summary(anov)



# Clean up
remove(anov, dat, id_dfLong2)







### Correlation between ID Variables ########################################################

corstars(id_df[,c("age", "educ_y", "BLP", "socioAffect_init")],
         removeTriangle = "upper", result = "none", method = "spearman")

cor.test(x = id_df$educ_y, y = id_df$BLP, method = "spearman")











### Correlation of ID Variables with Overall Starting Level #################################
load(file = paste(inputFolder, "id_df.rda", sep = "/"))


# Uncomment if you want results with outlier removed
# id_df = id_df[id_df$BLP != 1.00,]

corrDF = melt(id_df, id.vars = c("userCode", "initLevel"),
              measure.vars = c("age", "educ_y",  "BLP", "Act_total", "socioAffect_init"),
              variable.name = "Measure",
              value.name = "Value")

levels(corrDF$Measure) = c("Age", "Education", "Multilingualism", "Number of Hobbies", "Initial Motivation")

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

# clean up
remove(ageR, educR, motivR, actR, blpR, tmp)

# Plot

png(filename = paste(outputFolder, "figures", "corID_initLevel.png", sep = "/"), 
    height = 8, width = 25, units = "cm", res = 100)

  corrDF %>%
    ggplot(aes(x = Value, y = initLevel)) +
    geom_point(aes(color = dotCol)) +
    geom_smooth(method = "lm", alpha = 0.2, aes(color = lineCol)) +
    facet_grid(~ Measure, scales = "free_x") +
    theme_bw() + 
    ylab("Cognitive Baseline") +
    xlab("") +
    stat_cor(method = "pearson", 
             label.x = 0.25, label.y = 0.9, label.sep = "\n")  +
    scale_color_manual(values = c("cyan4", "darkgrey", "khaki3", "lightgrey")) +
    guides(color = F)

dev.off()



### Socio-Affect between Groups #################################

  ## Prepare data
  # For boxplot
  socioDF = df %>%
    select(userCode, Group, socioAffect) %>%
    group_by(userCode, Group) %>%
    summarise(socioAffect = mean(socioAffect, na.rm = T))

  # Data for GAMM-plot based on the model prediction of the GAMM predicting socio-affect per Group
  # see GAMM.R for model output and code

    # Make model
    load(paste(outputFolder,"tables_and_summaries", "socioAffect_gamm.rda", sep = "/"))

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
png(filename = paste(outputFolder, "plots", "socioAffect.png", sep = "/"), 
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


## Clean up
remove(socioDF, corrDF, plot.df, p1, p2)
