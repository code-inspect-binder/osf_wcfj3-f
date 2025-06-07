##########################################################
##                       LIBRARIES                      ##
##########################################################
# check whether required packages are already installed
packages = c("dplyr",
             "itsadug",
             "mgcv",
             "ggplot2",
             "car",
             "plotly",
             "ggsignif",
             "ggpubr",
             "voxel",
             "visreg",
             "reshape2",
             "gridExtra",
             "kableExtra",
             "xtable",
             "directlabels",
             "grid",
             "gridBase",
             "extrafont",
             "stargazer")
# for package xlsx choose "no" for Mac
newPackages = packages[!(packages %in% installed.packages()[ ,"Package"])]

# install any new packages
if(length(newPackages)) {
  install.packages(newPackages)
}

### Load libraries
library(dplyr)
library(itsadug)
library(mgcv)
library(ggplot2)
library(car)
library(plotly)
library(ggsignif)
library(ggpubr)
library(voxel)
library(visreg)
library(reshape2)
library(gridExtra)
library(kableExtra)
library(xtable)
library(directlabels)
library(grid)
library(gridBase)
library(extrafont)
library(stargazer)
#font_import()
#loadfonts(device="win")



# Clean up
remove(packages, newPackages)

##########################################################
##                       SETTINGS                       ##
##########################################################

### Set environment
workspace = getwd()
inputFolder = paste(workspace, "data", sep = "/") # where the data are
outputFolder = paste(workspace, "results", sep = "/") # output other than GAMMs



##########################################################
##                       LOAD DATA                     ##
##########################################################

### Load functions
source("src/funs.R")

### Load data preparation script
source("src/1_dataPrep.R")

### Differences in ID variables and their influence on the intercept
#source("2_idDiffs.R")


### GAMMs and all associated plots
#source("3_GAMMs.R")

### Figures created for publication
#source("4_publiPlots.R")


