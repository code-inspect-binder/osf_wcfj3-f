preproc <- function(df) {
  ## See Report.Rmd for explanation of preprocessing steps.
  # Remove wellbeing and training motivation values of 50, because they might be artefacts
  df$TrainingMotivation[which(df$TrainingMotivation == 50.00)] = NA
  df$Wellbeing[which(df$Wellbeing == 50.00)] = NA
  
  # Concatenate variables:
    # Operation Span and N-Back to Working Memory
    # Wellbeing and TrainingMotivatoin to Socio-Affect
  df$sOpSpan = minMaxNorm(df$sOpSpan)
  df$sNBack = minMaxNorm(df$sNBack)
  df = df %>%
    mutate(socioAffect = ifelse(is.na(Wellbeing), # socio-affect = mean over wellbeing & training motivation. Where one of the two is 50 (NA), the other one counts 100%.
                                ifelse(is.na(TrainingMotivation), NA, TrainingMotivation), 
                                ifelse(is.na(TrainingMotivation), Wellbeing, (Wellbeing + TrainingMotivation)/2)),
           sRWT = sRWT,
           sWM = (sOpSpan + sNBack)/2,
           rtWM = rtNBack,
           sDivAtt = sDivAtt,
           rtDivAtt = rtDivAtt) %>%
    select(-c(Wellbeing, TrainingMotivation, sOpSpan, sNBack))
  
  # Logarithmise all reaction times
  df$rtAlert = log(df$rtAlert)
  df$rtWM = log(df$rtWM)
  df$rtDivAtt = log(df$rtDivAtt)

  # Change reference level to be PASV
  df$Group = relevel(as.factor(df$Group), ref = "LANG")
  
  ## Adding ordered factors
  df$isLANG = ifelse(df$Group == "LANG", 1,0)
  df$isLANG0 = as.ordered(df$isLANG)
  contrasts(df$isLANG0) = "contr.treatment"
  
  df$isACTV = ifelse(df$Group == "ACTV", 1,0)
  df$isACTV0 = as.ordered(df$isACTV)
  contrasts(df$isACTV0) = "contr.treatment"
  
  df$isPASV = ifelse(df$Group == "PASV", 1,0)
  df$isPASV0 = as.ordered(df$isPASV)
  contrasts(df$isPASV0) = "contr.treatment"
  
  ## Reorder columns
  df = df %>%
    select(userCode, age, educ_y, Act_total, BLP, Time, 
           Group, isLANG, isLANG0, isACTV, isACTV0, isPASV, isPASV0, class, gend, socioAffect, sRWT, sWM, 
           rtWM, sDivAtt, rtDivAtt, rtAlert) %>%
    filter(Time > 1) %>% #  remove very first time point (because practice)
    group_by(userCode) %>%
    mutate(Time = 0:(length(Time)-1))
  
  
  # Add start.event (for autorcorrelation)
  df$start.event = df$Time == 0
    
  return(df)
}


















find_initLevel <- function(data, columns) {
  # Finds the intercept for each cognitive skill separately and creates a dataframe
  # with userCode as one column, and each of the variables under "columns" as another column.
  
  # data = the dataset where you find the original variables (sWM etc.)
  # columns = the columns where you want to look for the intercept ("sWM" etc.)
  # families = the respective GAMM family for each of the variables (e.g. "scat")
  
  outputDF = data.frame(userCode = unique(data$userCode))
  
  for (i in 1:length(columns)) {
    
    tmp = data.frame(userCode = unique(data$userCode),
                     vals = 0)
    
    for (s in unique(data$userCode)) {
      thisDat = data %>%
        filter(userCode == s)
      
      #intercept extraction
      mod = gam(eval(as.name(columns[i])) ~
                 s(Time), data = thisDat)
      newDat = data.frame(Time = 0)
      val = predict.gam(object = mod, newdata = newDat)[1]

      
      tmp$vals[tmp$userCode == s] = val
    }
    
    outputDF = cbind.data.frame(outputDF, tmp$vals)
    names(outputDF)[i+1] = paste("initLevel_", columns[i], sep ="")
  }
  
  return(outputDF)
}






minMaxNorm <- function(x)
  # for min-max-normalization
{
  return((x- min(x, na.rm = T)) /(max(x, na.rm = T)-min(x, na.rm = T)))
}







plot_intercept_GAMM <- function(data, task_abbreviations, task_names, families,
                                  filename, height, width, units, res) {

  for (i in 1:length(task_abbreviations)) {
    # Get data for this task
    thisDat = data %>%
      filter(Tasks == task_abbreviations[i])
    
    if(grepl("rt", as.character(thisDat$Tasks[1]))) {
      thisDat$score = 0-thisDat$score
    }
    
    # make model
    mod = bam(score ~
                isLANG0 +
                isACTV0 +
                te(Time, initLevel) +
                te(Time, initLevel, by = isLANG0) +
                te(Time, initLevel, by = isACTV0) +
                s(Time, userCode, bs = "fs", m = 1) +
                s(Time, userCode, by = isLANG0, bs = "fs", m = 1) +
                s(Time, userCode, by = isACTV0, bs = "fs", m = 1),
              data = thisDat, discrete = T, nthreads = 30)
    
    summ_mod = summary(mod)
    
    # define color scheme depending on significance
    colors = c(0,0,0)
    
    for (c in 1:length(colors)) {
      if (summ_mod$s.table[c,4] < 0.05) {
        colors[c] = 1
      } else {colors[c] = 0}
    }
    
    
    # Start plot
    png(filename = paste(modelsFolder, task_abbreviations[i]," interceptOnDevTe.png", sep = "/"), 
        height = height, width = width, units = units, res = res)
    
      layout(matrix(c(1,1,1,1,1,1,2,2,3,3,4,4,5,5,5,6,6,6,7,7,7,8,8,8), 4, 6, byrow = TRUE),
             height = c(2,4,4,4))
        plot.new()
        text(0.5,0.5,task_names[i],cex=2,font=2)
        pvisgam(mod, view = c("Time", "initLevel"), select = 1, ylab = "Baseline", 
                main = "Interaction Time x Baseline \n Overall",
                color = ifelse(colors[1] == 1, "topo", "gray"))
        pvisgam(mod, view = c("Time", "initLevel"), select = 2, ylab = "",
                main = "Interaction Time x Baseline \n in Group LANG",
                color = ifelse(colors[2] == 1, "topo", "gray"))
        pvisgam(mod, view = c("Time", "initLevel"), select = 3, ylab = "",
                main = "Interaction Time x Baseline \n in Group ACTV",
                color = ifelse(colors[3] == 1, "topo", "gray"))
        plot_diff(mod, view = "Time", comp = list(isLANG0 = c("1", "0")), cond = list(isACTV0 = "0",
                                                                                       initLevel = 0.75),
                  rm.ranef = T, main = "LANG minus PASV for \n Baseline = 0.75")
        plot_diff(mod, view = "Time", comp = list(isLANG0 = c("1", "0")), cond = list(isACTV0 = "0",
                                                                                       initLevel = 0.3),
                  rm.ranef = T, main = "LANG minus PASV for \n Baseline = 0.3")
        plot_diff(mod, view = "Time", comp = list(isACTV0 = c("1", "0")), cond = list(isLANG0 = "0",
                                                                                       initLevel = 0.75),
                  rm.ranef = T, main = "ACTV minus PASV for \n Baseline = 0.75")
        plot_diff(mod, view = "Time", comp = list(isACTV0 = c("1", "0")), cond = list(isLANG0 = "0",
                                                                                       initLevel = 0.3),
                  rm.ranef = T, main = "ACTV minus PASV for \n Baseline = 0.3")
        
    dev.off()
  }

}











littleGAMPlots <- function(data) {
  
  taskNames = c("Working Memory Acc.", "Working Memory RT", "Divided Attention RT",
                "Alertness RT", "Verbal Fluency")
  
  varNames = c("sWM", "rtWM", "rtDivAtt", "rtAlert", "sRWT")
  
  plotList = list()
  
  ## get model
  load(paste(outputFolder, "tables_and_summaries", "m3.rda", sep = "/"))
  
  for (i in 1:length(varNames)) {
    
    # get predictions for PASV
    p <- get_predictions(m3, cond = list(Tasks = varNames[i],
                                         isPASVissWM0 = ifelse(i == 1,"1","0"),
                                         isPASVisrtWM0 = ifelse(i == 2,"1","0"),
                                         isPASVisrtDivAtt0 = ifelse(i == 3,"1","0"),
                                         isPASVisrtAlert0 = ifelse(i == 4,"1","0"),
                                         isPASVissRWT0 = ifelse(i == 5,"1","0"),
                                         Time = c(0:28)),
                         print.summary = F,
                         rm.ranef = T)
    # add Group variable
    p = p %>%
      select(Time, fit, CI) %>%
      mutate(Group = "PASV")
    
    # get predictions for ACTV
    a <- get_predictions(m3, cond = list(Tasks = varNames[i],
                                         isACTVissWM0 = ifelse(i == 1,"1","0"),
                                         isACTVisrtWM0 = ifelse(i == 2,"1","0"),
                                         isACTVisrtDivAtt0 = ifelse(i == 3,"1","0"),
                                         isACTVisrtAlert0 = ifelse(i == 4,"1","0"),
                                         isACTVissRWT0 = ifelse(i == 5,"1","0"),
                                         Time = c(0:28)),
                         print.summary = F,
                         rm.ranef = T)
    
    # add Group variable
    a = a %>%
      select(Time, fit, CI) %>%
      mutate(Group = "ACTV")
    
    # get predictions for LANG
    l <- get_predictions(m3, cond = list(Tasks = varNames[i],
                                         Time = c(0:28)),
                         print.summary = F,
                         rm.ranef = T)
    
    # add Group variable
    l = l %>%
      select(Time, fit, CI) %>%
      mutate(Group = "LANG")
    
    plot.df = rbind.data.frame(l,p,a)

    # make PASV the reference level for plotting
    plot.df$Group = relevel(as.factor(plot.df$Group), ref = "PASV")
    
    ## Add to plotList
    plotList[[i]] <- plot.df %>%
      ggplot(aes(x = Time)) +
      geom_line(aes(y = fit, col = Group), size = 1) +
      geom_ribbon(aes(ymax = fit + CI,
                      ymin = fit- CI,
                      col = Group,
                      linetype = NA),
                  alpha = 0.05) +
      ggtitle(taskNames[i]) +
      ylab("Predicted") +
      scale_color_brewer(palette = "Paired", guide = ifelse(i < 5, F, "legend")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  return(plotList)
}












littleGAMPlots2 = function(data, columns) {
  
  taskNames = c("Working Memory Acc.", "Working Memory RT", "Divided Attention RT",
                "Alertness RT", "Verbal Fluency")
  
  varNames = c("sWM", "rtWM", "rtDivAtt", "rtAlert", "sRWT")
  
  plotList = list()
  
  ## get model
  load(paste(outputFolder, "tables_and_summaries", "m3.rda", sep = "/"))
  
  for (i in 1:length(varNames)) {
    
    # get predictions for PASV
    p <- get_predictions(m3, cond = list(Tasks = varNames[i],
                                         isPASVissWM0 = ifelse(i == 1,"1","0"),
                                         isPASVisrtWM0 = ifelse(i == 2,"1","0"),
                                         isPASVisrtDivAtt0 = ifelse(i == 3,"1","0"),
                                         isPASVisrtAlert0 = ifelse(i == 4,"1","0"),
                                         isPASVissRWT0 = ifelse(i == 5,"1","0"),
                                         Time = c(0:28)),
                         print.summary = F,
                         rm.ranef = T)
    # add Group variable
    p = p %>%
      select(Time, fit, CI) %>%
      mutate(Group = "PASV")
    
    # get predictions for ACTV
    a <- get_predictions(m3, cond = list(Tasks = varNames[i],
                                         isACTVissWM0 = ifelse(i == 1,"1","0"),
                                         isACTVisrtWM0 = ifelse(i == 2,"1","0"),
                                         isACTVisrtDivAtt0 = ifelse(i == 3,"1","0"),
                                         isACTVisrtAlert0 = ifelse(i == 4,"1","0"),
                                         isACTVissRWT0 = ifelse(i == 5,"1","0"),
                                         Time = c(0:28)),
                         print.summary = F,
                         rm.ranef = T)
    
    # add Group variable
    a = a %>%
      select(Time, fit, CI) %>%
      mutate(Group = "ACTV")
    
    # get predictions for LANG
    l <- get_predictions(m3, cond = list(Tasks = varNames[i],
                                         Time = c(0:28)),
                         print.summary = F,
                         rm.ranef = T)
    
    # add Group variable
    l = l %>%
      select(Time, fit, CI) %>%
      mutate(Group = "LANG")
    
    plot.df = rbind.data.frame(l,p,a)
    
    # make PASV the reference level for plotting
    plot.df$Group = relevel(as.factor(plot.df$Group), ref = "PASV")
    
    ## Add to plotList
    plotList[[i]] <- plot.df %>%
      ggplot(aes(x = Time)) +
      geom_line(aes(y = fit, lty = Group), size = 1) +
      geom_ribbon(aes(ymax = fit + CI,
                      ymin = fit- CI,
                      group = Group),
                  alpha = 0.05) +
      ggtitle(taskNames[i]) +
      ylab("Predicted") +
      scale_linetype_manual(values = c("dotted", "dashed", "solid"), guide = ifelse(i < 5, F, "legend")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(hjust = 0.5, family = "Times",face = "bold"),
            axis.title.x = element_text(family = "Times"),
            axis.title.y = element_text(family = "Times"),
            legend.text = element_text(family = "Times"),
            legend.title = element_text(family = "Times"))
  }
  
  return(plotList)
}



















plot_ID_GAMM <- function(data, predictor, filename, height,
                         width, units, res) {
  
  # Plots as many different plot_smooth plots as predictors indicated. The smooth plots show the 
  # main effect of a given predictor and its interaction over time for all of the cognitive tasks.
  # The plots are grey if nothing is significant and colored if the main effect or the interaction
  # is significant. The significance terms are spelled out in text form and annotated on top of the
  # plot.
  
  data = data %>%
    select(userCode, Time, predictVar = as.name(predictor), sWM, rtWM2, rtDivAtt, rtAlert, sRWT)
  
  # Start file
  png(filename, width = width, height = height, units = units, res = res)
  
  tasks = c("sWM", "rtWM2", "rtDivAtt", "rtAlert", "sRWT")
  task_names = c("Working Memory Acc.", "Working Memory RT", "Divided Attention RT",
                 "Alertness RT", "Verbal Fluency")
  families = c("gaussian", "binomial", "scat", "scat", "scat")
  
  if (predictor == "age") {
    k = 12
    ypos1 = 72
    ypos2 = 70  
    pred_name = "Age"
  } else if (predictor == "educ_y") {
    k = 15
    ypos1 = 18
    ypos2 = 16
    pred_name = "Education"
  } else if (predictor == "BLP"){
    k = 20
    ypos1 = 80
    ypos2 = 60
    pred_name = "Multilingualism"
  } else if (predictor == "Act_total") {
    k = 13
    ypos1 = 20
    ypos2 = 16
    pred_name = "Age"
  }
  
  par(mfrow = c(2,3))
  
  for(i in 1:5) { # because there are 5 tests
    # set family
    fam = families[i]
    
    # create respective GAMM
    mod = bam(eval(as.name(tasks[i])) ~ 
                s(Time) +
                s(predictVar, k = k) +
                ti(Time, predictVar, k = k) +
                s(Time, userCode, bs = "fs", m = 1),
              data = data, discrete = T, family = fam, nthreads = 30)
    
    sum = summary(mod)
    
    ##Plot
    
    if((sum$s.pv[2] < 0.05) | (sum$s.pv[3] < 0.05)) {
      fvisgam(mod, view = c("Time", "predictVar"), rm.ranef = T, main = task_names[i], 
              ylab = predictor)
    } else {
      fvisgam(mod, view = c("Time", "predictVar"), rm.ranef = T, main = task_names[i], 
              ylab = predictor, color = "gray")
    }
    
    
    if (sum$s.pv[2] < 0.05) {
      text(5,ypos1, paste("Main Effect of ", pred_name, ": \n p =", round(sum$s.pv[2],2), sep = ""),
           pos = 4)
    } else {
      text(5,ypos1, "Main effect not significant.", col = "red",
           pos = 4)
    }
    
    if (sum$s.pv[3] < 0.05) {
      text(5,ypos2, paste("Interaction Time x ", pred_name, "\n p =", round(sum$s.pv[3],2), sep = ""),
           pos = 4)
    } else {
      text(5,ypos2, paste("Interaction Time x ", pred_name, "\n not significant.", sep = ""), col = "red",
           pos = 4)
    }
    
  }
  
  dev.off()
  
}










# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 








#' Plot difference surface based on model predictions.
#' 
#' @export
#' @import mgcv
#' @import stats
#' @import grDevices
#' @import graphics
#' @import plotfunctions
#' @aliases plotDiff2D
#' @param model A GAMM model, resulting from the functions
#' \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
#' @param  view Name of continuous predictors that should be plotted on the x-
#'  and y-axes. Vector of two values.
#' @param comp Named list with the grouping predictor (categorical variable)
#' and the 2 levels to calculate the difference for.
#' @param cond Named list of the values to use for the other predictor terms 
#' (not in view). 
#' @param color The color scheme to use for plots. One of "topo", "heat", 
#' "cm", "terrain", "gray" or "bw". Alternatively a vector with some colors 
#' can be provided for a custom color palette.
#' @param nCol Range of colors of background of contour plot.
#' @param col Line color.
#' @param add.color.legend Logical: whether or not to add a color legend. 
#' Default is TRUE. If FALSE (omitted), one could use the function
#' \code{\link{gradientLegend}} to add a legend manually at any position.
#' @param se If less than or equal to zero then only the predicted surface is 
#' plotted, but if greater than zero, then the predicted values plus 
#' confidence intervals are plotted. 
#' The value of \code{se} will be multiplied with 
#' the standard error (i.e., 1.96 results in 95\%CI and 2.58). 
#' Default is set to 1.96 (95\%CI).
#' @param sim.ci Logical: Using simultaneous confidence intervals or not 
#' (default set to FALSE). The implementation of simultaneous CIs follows 
#' Gavin Simpson's blog of December 15, 2016: 
#' \url{http://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/}. 
#' This interval is calculated from simulations based. 
#' Please specify a seed (e.g., \code{set.seed(123)}) for reproducable results. 
#' Note: in contrast with Gavin Simpson's code, here the Bayesian posterior 
#' covariance matrix of the parameters is uncertainty corrected 
#' (\code{unconditional=TRUE}) to reflect the uncertainty on the estimation of 
#' smoothness parameters.
#' @param show.diff Logical: whether or not to indicate the regions that 
#' are significantly different from zero. Note that these regions are just 
#' an indication and dependent on the value of \code{n.grid}. 
#' Defaults to FALSE.
#' @param col.diff Color to shade the nonsignificant areas.
#' @param alpha.diff Level of transparency to mark the nonsignificant areas.
#' @param n.grid Resolution.
#' @param nlevels Levels of contour lines.
#' @param zlim A two item array giving the lower and upper limits for the z-
#' axis scale. NULL to choose automatically.
#' @param xlim A two item array giving the lower and upper limits for the x-
#' axis scale. NULL to choose automatically.
#' @param ylim A two item array giving the lower and upper limits for the y-
#' axis scale. NULL to choose automatically.
#' @param main Title of plot.
#' @param xlab Label x-axis.
#' @param ylab Label y-axis.
#' @param rm.ranef Logical: whether or not to remove random effects. 
#' Default is FALSE. Alternatively a string (or vector of strings) with the 
#' name of the random effect(s) to remove.
#' @param transform.view List with two functions for transforming 
#' the values on the x- and y-axis respectively. If one of the axes 
#' need to be transformed, set the other to NULL (no transformation). 
#' (See \code{\link{fvisgam}} for more info.)
#' @param print.summary Logical: whether or not to print a summary.
#' Default set to the print info messages option 
#' (see \code{\link{infoMessages}}).
#' @param hide.label Logical: whether or not to hide the label 
#' (i.e., "difference"). Default is FALSE.
#' @param dec Numeric: number of decimals for rounding the color legend. 
#' When NULL (default), no rounding. If -1 (default), automatically determined. 
#' Note: if value = -1 (default), rounding will be applied also when 
#' \code{zlim} is provided.
#' @param ... Optional arguments for \code{\link[plotfunctions]{plotsurface}}.
#' @section Warning:
#' When the argument \code{show.diff} is set to TRUE a shading area indicates 
#' where the confidence intervals include zero. Or, in other words, the areas 
#' that are not significantly different from zero. Be careful with the 
#' interpretation, however, as the precise shape of the surface is dependent 
#' on model constraints such as the value of \code{\link[mgcv]{choose.k}} and the 
#' smooth function used, and the size of the confidence intervals are 
#' dependent on the model fit and model characteristics 
#' (see \code{vignette('acf', package='itsadug')}). In addition, the value of 
#' \code{n.grid} determines the precision of the plot.
#' @return If the result is not being plotted, a list is 
#' returned with the estimated difference (\code{est}) and the standard error 
#' over the estimate (\code{se.est}) and the x-values (\code{x}) is returned.
#' @author Martijn Wieling, reimplemented by Jacolien van Rij
#'
#' @examples
#' data(simdat)
#' \dontrun{
#' m1 <- bam(Y ~ Group + te(Time, Trial, by=Group),
#'     data=simdat)
#' plot_diff2(m1, view=c('Time', 'Trial'), 
#'     comp=list(Group=c("Children", "Adults")))
#' }
#' @family Testing for significance
# plots differences in 2D plot
plot_diff3 <- function(model, view, comp, cond=NULL, 
                       color='topo', nCol=100, col=NULL, add.color.legend=TRUE,
                       se=1.96, sim.ci=FALSE, show.diff=FALSE, col.diff=1, alpha.diff=.5,
                       n.grid=30, nlevels=10, 
                       zlim=NULL, xlim=NULL, ylim=NULL, 
                       main=NULL, xlab=NULL, ylab=NULL,
                       rm.ranef=NULL, transform.view=NULL,
                       hide.label=FALSE,
                       dec=NULL,
                         print.summary=getOption('itsadug_print'), ...) { 
  dat = model$model
  xvar <- NULL
  yvar <- NULL
  by_predictor <- NULL
  # check view
  if(length(view) < 2){
    stop('Provide predictors for x- and y-axes in view.')
  }else{
    xvar <- view[1]
    yvar <- view[2]
    if(xvar %in% names(cond)){
      warning(sprintf('Predictor %s specified in view and cond. Values in cond being used, rather than the whole range of %s.', xvar, xvar))
    }else{
      cond[[xvar]] <- seq(min(na.exclude(dat[,xvar])), max(na.exclude(dat[,xvar])), length=n.grid)
      if(!is.null(xlim)){
        if(length(xlim) != 2){
          warning("Invalid xlim values specified. Argument xlim is being ignored.")
        }else{ 
          cond[[xvar]] <- seq(xlim[1], xlim[2], length=n.grid)
        }
      }
    }
    if(yvar %in% names(cond)){
      warning(sprintf('Predictor %s specified in view and cond. Values in cond being used, rather than the whole range of %s.', yvar, yvar))
      cond[[yvar]] <- NULL
    }else{
      cond[[yvar]] <- seq(min(na.exclude(dat[,yvar])), max(na.exclude(dat[,yvar])), length=n.grid)
      if(!is.null(ylim)){
        if(length(ylim) != 2){
          warning("Invalid ylim values specified. Argument ylim is being ignored.")
        }else{ 
          cond[[yvar]] <- seq(ylim[1], ylim[2], length=n.grid)
        }
      }
    }
  }
  newd <- c()
  newd <- get_difference(model, comp=comp, cond=cond, 
                         se=ifelse(se>0, TRUE, FALSE), 
                         f=ifelse(se>0, se, 1.96), sim.ci=sim.ci, 
                         print.summary=print.summary, rm.ranef=rm.ranef)
  # transform values x- and y-axes:
  errormessage <- function(name){
    return(sprintf("Error: the function specified in transformation.view cannot be applied to %s-values, because infinite or missing values are not allowed.", name))
    
  }
  if(!is.null(transform.view)){
    if(length(transform.view)==1){
      tryCatch(newd[,xvar] <- sapply(newd[,xvar], transform.view), 
               error=function(x){},
               warning=function(x){})
      tryCatch(newd[,yvar] <- sapply(newd[,yvar], transform.view), 
               error=function(x){},
               warning=function(x){})
      if(any(is.infinite(newd[,xvar])) | any(is.nan(newd[,xvar])) | any(is.na(newd[,xvar]))){
        stop(errormessage("x"))
      }
      if(any(is.infinite(newd[,yvar])) | any(is.nan(newd[,yvar])) | any(is.na(newd[,yvar]))){
        stop(errormessage("y"))
      }
      if(print.summary){
        cat("\t* Note: The same transformation is applied to values of x-axis and y-axis.\n")
      }
    }else if(length(transform.view) >= 2){
      if(!is.null(transform.view[[1]])){
        tryCatch(newd[,xvar] <- sapply(newd[,xvar], transform.view[[1]]), 
                 error=function(x){},
                 warning=function(x){})
        if(any(is.infinite(newd[,xvar])) | any(is.nan(newd[,xvar])) | any(is.na(newd[,xvar]))){
          stop(errormessage("x"))
        }
      }
      if(!is.null(transform.view[[2]])){
        tryCatch(newd[,yvar] <- sapply(newd[,yvar], transform.view[[2]]), 
                 error=function(x){},
                 warning=function(x){})
        if(any(is.infinite(newd[,yvar])) | any(is.nan(newd[,yvar])) | any(is.na(newd[,yvar]))){
          stop(errormessage("y"))
        }
      }
      if(print.summary){
        cat("\t* Note: Transformation function(s) applied to values of x-axis and / or y-axis.\n")
      }
    }          
  }
  if (is.null(main)) {
    levels1 <- paste(sapply(comp, function(x) x[1]), collapse='.')
    levels2 <- paste(sapply(comp, function(x) x[2]), collapse='.')
    main = sprintf('Difference between %s and %s', levels1, levels2)
  } 
  if(is.null(ylab)) {
    ylab = view[2]
  }
  if(is.null(xlab)) {
    xlab = view[1]
  }
  if(se > 0){
    # p <- plotfunctions::plotsurface(newd, view=view, predictor="difference", valCI='CI',
    #                                 main=main, xlab=xlab, ylab=ylab, 
    #                                 zlim=zlim, 
    #                                 col=col, color=color, nCol=nCol, add.color.legend=add.color.legend,
    #                                 nlevels=nlevels, dec=dec, ...)
    
    ggplot(newd, aes(x = Time, y = initLevel, z = difference)) + 
      geom_contour_filled(aes(colour = ..level..), 
                          breaks = c(-0.3,-0.225, -0.15, -0.075, - 0.001, 0, 
                                     0.001, 0.03, 0.06, 0.12, 0.15)) +
      theme_bw() +
      theme(legend.position = "none")
    
    p <- plotfunctions::plotsurface(newd, view=view, predictor="difference", valCI='CI',
              main=main, xlab=xlab, ylab=ylab,
              zlim=zlim,
              col=col, color=color, nCol=nCol, add.color.legend=add.color.legend,
              nlevels=nlevels, dec=dec)
    
    if(hide.label==FALSE){
      addlabel = "difference"
      if(!is.null(rm.ranef)){
        if(rm.ranef !=FALSE){
          addlabel = paste(addlabel, "excl. random", sep=", ")
        }
      }
      mtext(addlabel, side=4, line=0, adj=0, 
            cex=.75, col='gray35', xpd=TRUE)
    }     		
  }else{
    p <- plotfunctions::plotsurface(newd, view=view, predictor="difference", 
                                    main=main, xlab=xlab, ylab=ylab, 
                                    zlim=zlim, 
                                    col=col, color=color, nCol=nCol, add.color.legend=add.color.legend,
                                    nlevels=nlevels, dec=dec, ...)	
    if(hide.label==FALSE){
      addlabel = "difference"
      if(!is.null(rm.ranef)){
        if(rm.ranef !=FALSE){
          addlabel = paste(addlabel, "excl. random", sep=", ")
        }
      }
      if(sim.ci==TRUE){
        addlabel = paste(addlabel, "simult.CI", sep=", ")
      }
      mtext(addlabel, side=4, line=0, adj=0, 
            cex=.75, col='gray35', xpd=TRUE)
    }  	
  }
  if(show.diff){
    plot_signifArea(newd, view=view, predictor="difference", valCI="CI", col=col.diff, alpha=alpha.diff)
  }
  
  p[['zlim']] <- zlim
  invisible(p)
}












plot_diff_bw = function (model, view, comp, cond = NULL, se = 1.96, sim.ci = FALSE, 
          n.grid = 100, add = FALSE, rm.ranef = NULL, mark.diff = TRUE, 
          col.diff = "black", col = "black", eegAxis = FALSE, transform.view = NULL, 
          print.summary = getOption("itsadug_print"), plot = TRUE, 
          main = NULL, ylab = NULL, xlab = NULL, xlim = NULL, ylim = NULL, 
          hide.label = FALSE, ...) 
{
  if (sim.ci == TRUE) {
    n.grid = max(n.grid, 200)
  }
  dat = model$model
  xvar <- NULL
  by_predictor <- NULL
  if (length(view) > 1) {
    warning("Only first element of 'view' is being used. Use plot_diff2 for plotting difference surfaces.")
  }
  else {
    xvar <- view[1]
    if (xvar %in% names(cond)) {
      warning(sprintf("Predictor %s specified in view and cond. Values in cond being used, rather than the whole range of %s.", 
                      xvar, xvar))
    }
    else {
      cond[[xvar]] <- seq(min(na.exclude(dat[, xvar])), 
                          max(na.exclude(dat[, xvar])), length = n.grid)
    }
  }
  if (!is.null(xlim)) {
    if (length(xlim) != 2) {
      warning("Invalid xlim values specified. Argument xlim is being ignored.")
    }
    else {
      cond[[xvar]] <- seq(xlim[1], xlim[2], length = n.grid)
    }
  }
  newd <- c()
  newd <- get_difference(model, comp = comp, cond = cond, 
                         se = ifelse(se > 0, TRUE, FALSE), f = ifelse(se > 0, 
                                                                      se, 1.96), sim.ci = sim.ci, print.summary = print.summary, 
                         rm.ranef = rm.ranef)
  errormessage <- function() {
    return("Error: the function specified in transformation.view cannot be applied to x-values, because infinite or missing values are not allowed.")
  }
  if (!is.null(transform.view)) {
    tryCatch(newd[, xvar] <- sapply(newd[, xvar], transform.view), 
             error = function(x) {
             }, warning = function(x) {
             })
    if (any(is.infinite(newd[, xvar])) | any(is.nan(newd[, 
                                                         xvar])) | any(is.na(newd[, xvar]))) {
      stop(errormessage())
    }
    if (print.summary) {
      cat("\t* Note: x-values are transformed.\n")
    }
  }
  out <- data.frame(est = newd$difference, x = newd[, xvar])
  names(out)[2] <- xvar
  if (se > 0) {
    out$CI <- newd$CI
    out$f <- se
    if (sim.ci == TRUE) {
      out$sim.CI <- newd$sim.CI
    }
  }
  out$comp = list2str(names(comp), comp)
  if (is.null(main)) {
    levels1 <- paste(sapply(comp, function(x) x[1]), collapse = ".")
    levels2 <- paste(sapply(comp, function(x) x[2]), collapse = ".")
    main = sprintf("Difference %s - %s", levels1, levels2)
  }
  if (is.null(ylab)) {
    ylab = sprintf("Est. difference in %s", as.character(model$formula[[2]]))
  }
  if (is.null(xlab)) {
    xlab = xvar
  }
  if (is.null(ylim)) {
    ylim <- range(newd$difference)
    if (se > 0) {
      ylim <- with(newd, range(c(difference + CI, difference - 
                                   CI)))
    }
  }
  if (is.null(xlim)) {
    xlim <- range(newd[, xvar])
  }
  par = list(...)
  if (!"h0" %in% names(par)) {
    par[["h0"]] <- 0
  }
  if (!"shade" %in% names(par)) {
    par[["shade"]] <- TRUE
  }
  area.par <- c("shade", "type", "pch", "lty", "bg", "cex", 
                "lwd", "lend", "ljoin", "lmitre", "ci.lty", "ci.lwd", 
                "border", "alpha", "density", "angle")
  line.par <- c("type", "pch", "lty", "bg", "cex", "lwd", 
                "lend", "ljoin", "lmitre")
  area.args <- list2str(area.par, par)
  line.args <- list2str(line.par, par)
  plot.args <- list2str(x = names(par)[!names(par) %in% c(line.par, 
                                                          area.par)], par)
  if (plot == TRUE) {
    if (add == FALSE) {
      eval(parse(text = sprintf("emptyPlot(xlim, ylim, \n\t\t\t\tmain=main, xlab=xlab, ylab=ylab, \n\t\t\t\teegAxis=eegAxis, %s)", 
                                plot.args)))
      if (hide.label == FALSE) {
        addlabel = "difference"
        if (!is.null(rm.ranef)) {
          if (rm.ranef != FALSE) {
            addlabel = paste(addlabel, "excl. random", 
                             sep = ", ")
          }
        }
        if (sim.ci == TRUE) {
          addlabel = paste(addlabel, "simult.CI", sep = ", ")
        }
        mtext(addlabel, side = 4, line = 0, adj = 0, 
              cex = 0.75, col = "gray35", xpd = TRUE)
      }
    }
    if (se > 0) {
      if (sim.ci == TRUE) {
        eval(parse(text = sprintf("plot_error(newd[,xvar], newd$difference, newd$sim.CI, col=col, %s)", 
                                  area.args)))
      }
      else {
        eval(parse(text = sprintf("plot_error(newd[,xvar], newd$difference, newd$CI, col=col, %s)", 
                                  area.args)))
      }
    }
    else {
      if (line.args == "") {
        lines(newd[, xvar], newd$difference, col = col)
      }
      else {
        eval(parse(text = sprintf("lines(newd[,xvar], newd$difference, col=col, %s)", 
                                  line.args)))
      }
    }
    if (mark.diff == TRUE) {
      diff <- find_difference(newd$difference, newd$CI, 
                              newd[, xvar])
      if (sim.ci == TRUE) {
        diff <- find_difference(newd$difference, newd$sim.CI, 
                                newd[, xvar])
      }
      if (length(diff$start) > 0) {
        addInterval(pos = getFigCoords("p")[3], diff$start, 
                    diff$end, col = col.diff, lwd = 20 * par()$lwd, 
                    length = 0, xpd = FALSE, lend = "butt")
        abline(v = c(diff$start, diff$end), lty = 3, 
               col = col.diff)
      }
    }
    if (print.summary) {
      if (length(diff$start) > 0) {
        tmp <- c(sprintf("%s window(s) of significant difference(s):", 
                         xvar), sprintf("\t%f - %f", diff$start, diff$end))
      }
      else {
        tmp <- "Difference is not significant."
      }
      cat("\n")
      cat(paste(tmp, collapse = "\n"))
      cat("\n")
    }
    invisible(out)
  }
  else {
    return(out)
  }
}

