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


print_cols <- function(x) cat(colnames(x),sep="\n")

wild_log10 <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/wild_log10_As_Cd_Ni.xlsx")
View(wild_log10)  


#just the core12
cultivated_log10_As_Cd_Ni <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/wild_log10_As_Cd_Ni.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM",species))

View(cultivated_log10_As_Cd_Ni)



library(readxl)
cultivated_AMF <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/cultivated_AMF.xlsx")
View(cultivated_AMF)

library(readxl)
Data12S1 <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/Data12S1.xlsx")
View(Data12S1)



merged_metals_amf <- merge(cultivated_log10_As_Cd_Ni, Data12S1, by = "species" , all = TRUE)
View(merged_metals_amf)

#get the cor
merged_metals_amf %>% 
  select (`Mean Colonization Rate (LS Mean)`,As:Ni) %>% cor(use = "pair")



merged_metals_amf %>% select (`Mean Colonization Rate (LS Mean)`,As:Ni) %>% correlation() %>% summary(
) %>% plot()

#goodone

merged_metals_amf %>% select (As:Ni, `Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
) %>% plot()



modelamfbymetals <- lm(`Mean Colonization Rate (LS Mean)` ~ As, data = merged_metals_amf)
plot(modelamfbymetals)

library(easystats)


library(readxl)
cultivated_salt <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/cultivated_salt.xlsx")
View(cultivated_salt)



merged_metals_amf_salt <- merge(merged_metals_amf, cultivated_salt, by = "species" , all = TRUE)
View(merged_metals_amf_salt)


merged_metals_amf_salt %>% select (LateralRoot_logdiff,`Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
) %>% plot()


library(readxl)
wild_metal_dropped <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/wild_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM",species))
View(wild_metal_dropped)


#wild metal droped
library(readxl)
Core_metals <- read_excel("C:/Users/17178/Downloads/Core_metals.xlsx")
View(Core_metals)



#combined core data

cultivated amf, wild log 10 as cd ni, cultivated salt  

library(readxl)
CombinedCoreData <- read_excel("C:/Users/17178/Downloads/CombinedCoreData.xlsx")
View(CombinedCoreData)


# core metals we need to take the log averages from wild metal dropped





#all 3 of them , now you have to filter out each me
merged_metals_amf_metaldropped <- merge(merged_metals_amf, Core_metals, by = "species" , all = TRUE)
View(merged_metals_amf_metaldropped)


#just the 020... idk how to analyze because obviously the stdv will be zero because its all 020

sam020metals <- filter (merged_metals_amf_metaldropped, species == "SAM020")
View(sam020metals)
sam020metals %>% select (height,`Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
) %>% plot()


sam020metals %>% 
  select (`Mean Colonization Rate (LS Mean)`,As.x:Ni.x) %>% cor(use = "pair")

sam020metals %>% select (height,`Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
) %>% plot()

````````````````````````````````````````````````
#getting just the metals and the core 12s


#Al

samcultivatedAl <- filter (merged_metals_amf_metaldropped, Treatment == "Al")
View(samcultivatedAl)

samcultivatedAl %>% 
  select (`Mean Colonization Rate (LS Mean)`,As.x:Ni.x) %>% cor(use = "pair")

samcultivatedAl %>% 
  select (`Mean Colonization Rate (LS Mean)`,Al:Zn) %>% cor(use = "pair")

samcultivatedAl %>% select (Al:Zn,`Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
) %>% plot()

model_amf_height_Al <- lm (`Mean Colonization Rate (LS Mean)`~ height, data= samcultivatedAl)
summary(model1)

model_amf_Al <- lm (`Mean Colonization Rate (LS Mean)`~ Al, data= samcultivatedAl)
summary(model_amf_Al)


#Pb

samcultivatedPb <- filter (merged_metals_amf_metaldropped, Treatment == "Pb")
View(samcultivatedPb)


model_amf_Pb <- lm (`Mean Colonization Rate (LS Mean)`~ Pb, data= samcultivatedPb)
summary(model_amf_Pb)



#Cr

samcultivatedCr <- filter (merged_metals_amf_metaldropped, Treatment == "Cr")
View(samcultivatedCr)

model_amf_Cr <- lm (`Mean Colonization Rate (LS Mean)`~ Cr, data= samcultivatedCr)
summary(model_amf_Cr)



#Fe

samcultivatedFe <- filter (merged_metals_amf_metaldropped, Treatment == "Fe")
View(samcultivatedFe)

model_amf_Fe <- lm (`Mean Colonization Rate (LS Mean)`~ Fe, data= samcultivatedFe)
summary(model_amf_Fe)



#Cd

samcultivatedCd <- filter (merged_metals_amf_metaldropped, Treatment == "Cd")
View(samcultivatedCd)

#not working
model_amf_Cd <- lm (`Mean Colonization Rate (LS Mean)`~ Cd, data= samcultivatedCd)
summary(model_amf_Cd)



#As

samcultivatedAs <- filter (merged_metals_amf_metaldropped, Treatment == "As")
View(samcultivatedAs)

#not working
model_amf_As <- lm (`Mean Colonization Rate (LS Mean)`~ As, data= samcultivatedAs)
summary(model_amf_As)




#Ct

samcultivatedCt <- filter (merged_metals_amf_metaldropped, Treatment == "Ct")
View(samcultivatedCt)


#notworking
model_amf_Ct <- lm (`Mean Colonization Rate (LS Mean)`~ Ct, data= samcultivatedCt)
summary(model_amf_Ct)


#Ni

samcultivatedNi <- filter (merged_metals_amf_metaldropped, Treatment == "Ni")
View(samcultivatedNi)


#notworking
model_amf_Ni <- lm (`Mean Colonization Rate (LS Mean)`~ Ni, data= samcultivatedNi)
summary(model_amf_Ni)



#Mn

samcultivatedMn <- filter (merged_metals_amf_metaldropped, Treatment == "Mn")
View(samcultivatedMn)


model_amf_Mn <- lm (`Mean Colonization Rate (LS Mean)`~ Mn, data= samcultivatedMn)
summary(model_amf_Mn)


#Zn

samcultivatedZn <- filter (merged_metals_amf_metaldropped, Treatment == "Zn")
View(samcultivatedZn)


model_amf_Zn <- lm (`Mean Colonization Rate (LS Mean)`~ Zn, data= samcultivatedZn)
summary(model_amf_Zn)


#Cu

samcultivatedCu <- filter (merged_metals_amf_metaldropped, Treatment == "Cu")
View(samcultivatedCu)


model_amf_Cu <- lm (`Mean Colonization Rate (LS Mean)`~ Cu, data= samcultivatedCu)
summary(model_amf_Cu)

Cu_cor <- correlation(samcultivatedCu)



`````````` taken from "improved course project" RB I am consolidated it here````
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

`````````````````````````



library(ggplot2)
library(tidyverse)
library(MASS)
library(gamlss)
library(performance)
library(performance)
AICctab(model_amf_Al, model_amf_Pb, model_amf_Cr,model_amf_Fe, model_amf_Mn, model_amf_Zn, model_amf_Cu, base=T, delta=T, weights=T)



# sunflower metabolomics
HPLC_leaves <- read.csv("data/HPLC_leaves.csv")
GCMS_leaves <- read.csv("data/GCMS_leaves.csv")
GCMS_petals <- read.csv("data/GCMS_petals.csv")



# cultivated metals with log10 of As, Cd, and Ni only
cultivated_log10_As_Cd_Ni <- read_excel("data/wild_log10_As_Cd_Ni.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM",species))

# cultivated AMF
cultivated_AMF <- read_excel("data/cultivated_AMF.xlsx",skip = 1) %>% as.data.frame()

`````````instructions 

# merge multiple wild datasets together
wild_merged <- merge(genome_sizes,wild_powdery,by = "species",all = TRUE)
wild_merged <- merge(wild_merged,wild_biomass,by = "species",all = TRUE)

# merge multiple cultivated datasets together
cultivated_merged <- merge(cultivated_LES,cultivated_floral,by = "Line",all = TRUE)
cultivated_merged <- merge(cultivated_merged,cultivated_AMF,by = "Line",all = TRUE)


# how to select specific variables for correlation matrix

# first print column names
colnames(cultivated_merged)

# then type the column names you want (separated by commas) using select() then add cor(use="pair")
  # NOTE: if you want to span multiple columns, you can use a colon and type the first and last column only.
# for example: this selects every column from WaterContent to Solidity (columns 5:10), as well as TotalFlavanoids and LES_PC1 (21 and 22)
cultivated_merged %>% 
  select(WaterContent:Solidity,TotalFlavonoids,LES_PC1) %>%
  cor(use = "pair") %>% 
  round(3)

# identical
cultivated_merged %>% select(5:10,21,22)













