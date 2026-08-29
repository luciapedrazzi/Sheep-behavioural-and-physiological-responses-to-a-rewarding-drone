##### LIBRARIES ----
library(broom.mixed)
library(corrplot)
library(cowplot)
library(DHARMa)
library(dplyr)
library(flextable)
library(ggeffects)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(lmerTest)
library(pbkrtest)
library(performance)
library(tidyr)

##### SETTINGS ----
Sys.setenv(tz="UTC")
Sys.setlocale("LC_TIME", "English_United Kingdom") 
memory.limit(size = 40960)
theme_set(theme_cowplot())

##### DIRECTORIES ----
pdir <- "Your_Path/Figures" 


