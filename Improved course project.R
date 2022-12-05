library(tidyverse)
library(readxl)
library(readxl)
library(tidyverse)
library(janitor)
library(rgl)
library(car)
library(tidyverse)
library(ggfortify)
library(ggforce)
library(mgcv)
library(easystats)
library(MASS)
library(gamlss)
library(performance)
library(ggplot2)
library(GGally)


print_cols <- function(x) cat(colnames(x),sep="\n")

#wild metal dropped. we need to take the averages and log transform

library(readxl)
Core_metals <- read_excel("C:/Users/17178/Downloads/Core_metals (1).xlsx")
View(Core_metals)


#combined core data : cultivated amf, wild log 10 as cd ni, cultivated salt 

library(readxl)
CombinedCoreData <- read_excel("C:/Users/17178/Downloads/CombinedCoreData (1).xlsx")
View(CombinedCoreData)

CombinedCoreData %>% select (As: Ni, `Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
) %>% plot()

cor(As:Ni)


ggpairs(CombinedCoreData[ 2:4, 6:8, 25, 34])

attach(CombinedCoreData)
nullmodel <- lm (`Mean Colonization Rate (LS Mean)` ~ 1)
modelAs <- lm(`Mean Colonization Rate (LS Mean)`~ As, data= CombinedCoreData)
modelCd <- lm(`Mean Colonization Rate (LS Mean)`~ Cd, data= CombinedCoreData)
modelNi <- lm(`Mean Colonization Rate (LS Mean)`~ Ni, data= CombinedCoreData)

AICctab(nullmodel, modelAs, modelCd, modelNi)











