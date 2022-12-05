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



library(ggplot2)
library(tidyverse)
library(MASS)
library(gamlss)
library(performance)
library(performance)
AICctab(model_amf_Al, model_amf_Pb, model_amf_Cr,model_amf_Fe, model_amf_Mn, model_amf_Zn, model_amf_Cu, base=T, delta=T, weights=T)






library(readxl)
wild_metal <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/wild_metal.xlsx")
View(wild_metal)

library(readxl)
wild_nickel_dose_response <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/wild_nickel_dose_response.xlsx")
View(wild_nickel_dose_response)

library(readxl)
wild_nickel_summary <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/wild_nickel_summary.xlsx")
View(wild_nickel_summary)

library(readxl)
wild_root_traits <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/wild_root_traits.xlsx")
View(wild_root_traits)


library(readxl)
wild_salt <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/wild_salt.xlsx")
View(wild_salt)


library(readxl)
cultivated_nutrients <- read_excel("C:/Users/17178/OneDrive/Desktop/FALL 2022 CLASSES/Plant genomics and biochem/COURSE PROJECT/cultivated_nutrients.xlsx")
View(cultivated_nutrients)





# sunflower genome sizes
genome_sizes <- read.csv("data/genome_size_means.csv")
print_cols(genome_sizes)

# sunflower metabolomics
HPLC_leaves <- read.csv("data/HPLC_leaves.csv")
GCMS_leaves <- read.csv("data/GCMS_leaves.csv")
GCMS_petals <- read.csv("data/GCMS_petals.csv")

# sunflower GC-MS metabolomics, but with relative proportion instead of quantity
GCMS_leaves_prop <- read.csv("data/GCMS_leaves_prop.csv")
GCMS_petals_prop <- read.csv("data/GCMS_petals_prop.csv")

# wild powdery mildew
wild_powdery <- read_excel("data/wild_fungus.xlsx") %>% 
  as.data.frame() %>% select(1:5) %>% 
  filter(grepl("H.",wild_powdery$species))

# wild biomass allocation
wild_biomass <- read_excel("data/wild_biomass_alloc.xlsx",sheet = "species") %>% as.data.frame()

# wild biomass allocation (population level)
wild_biomass_pop <- read_excel("data/wild_biomass_alloc.xlsx",sheet = "pop") %>% as.data.frame()

# wild floral traits
wild_floral <- read_excel("data/wild_floral.xlsx",sheet = "Species_Means_Env") %>% as.data.frame()

# wild floral traits (population level)
wild_floral_pop <- read_excel("data/wild_floral.xlsx",sheet = "Pop_LSmeans_Env") %>% as.data.frame()

# wild leaf & env traits (species level)
wild_leaf_env <- read_excel("data/wild_LES_env_species.xlsx") %>% as.data.frame()

# wild leaf & env traits (population level)
wild_leaf_env <- read_excel("data/wild_LES_env_species.xlsx") %>% as.data.frame()

# wild root (population level)wild_leaf_env_pop <- read_excel("data/wild_LES_env.xlsx") %>% as.data.frame()
wild_root <- read_excel("data/wild_root_traits.xlsx") %>% as.data.frame()

# wild salt dose response data
wild_salt <- read_excel("data/wild_salt.xlsx") %>% as.data.frame() %>% filter(species != "HA")

# wild multiple metals data -- suggest only looking at As, Cd, and Ni treatments
wild_metals_all <- read_excel("data/wild_metal.xlsx") %>% as.data.frame()

# wild multiple metals data (data dropped if leaf mass for analysis was <0.1g) -- suggest only looking at As, Cd, and Ni treatments
wild_metals_dropped <- read_excel("data/wild_metal_dropped.xlsx") %>% as.data.frame()

# wild multiple metals with log10 of As, Cd, and Ni only
wild_log10_As_Cd_Ni <- read_excel("data/wild_log10_As_Cd_Ni.xlsx") %>% as.data.frame()

# wild nickel species mean accumulation and tolerance (slope of soil Ni vs total leaf area)
wild_Ni_summary <- read_excel("data/wild_nickel_summary.xlsx") %>% as.data.frame()

# wild nickel -- full dose response curve data and traits
wild_Ni_all <- read_excel("data/wild_nickel_dose_response.xlsx") %>% as.data.frame()


# cultivated metals with log10 of As, Cd, and Ni only
cultivated_log10_As_Cd_Ni <- read_excel("data/wild_log10_As_Cd_Ni.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM",species))

# cultivated AMF
cultivated_AMF <- read_excel("data/cultivated_AMF.xlsx",skip = 1) %>% as.data.frame()


# cultivated drought
cultivated_drought <- read_excel("data/cultivated_drought.xlsx") %>% 
  pivot_wider(id_cols = Line,names_from = Treatment,values_from = `Stem Diameter (mm)`:`TRL Allocation`) %>%
  as.data.frame()
colnames(cultivated_drought)[grepl("Water-limited",colnames(cultivated_drought))] <- paste("Drought",gsub("_Water-limited","",colnames(cultivated_drought)[grepl("Water-limited",colnames(cultivated_drought))]))
colnames(cultivated_drought)[grepl("Well-watered",colnames(cultivated_drought))] <- paste("Water",gsub("_Well-watered","",colnames(cultivated_drought)[grepl("Well-watered",colnames(cultivated_drought))]))

# cultivated floral
cultivated_floral <- read_excel("data/cultivated_floral.xlsx") %>% as.data.frame()
cultivated_floral$Line <- gsub("PPN","SAM",cultivated_floral$Line)

# cultivated LES, phenolics, etc
cultivated_LES <- read_excel("data/cultivated_LES_phenolics_flav.xlsx") %>% as.data.frame()
colnames(cultivated_LES)[1] <- "Line"
cultivated_LES$Line <- gsub("PPN_","SAM",cultivated_LES$Line)

# cultivated salt
cultivated_salt <- read_excel("data/cultivated_salt.xlsx") %>% as.data.frame()
colnames(cultivated_salt)[1] <- "Line"

# cultivated nutrients
cultivated_nutrients <- read_excel("data/cultivated_nutrients.xlsx") %>% as.data.frame()
colnames(cultivated_nutrients)[1] <- "Line"





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


