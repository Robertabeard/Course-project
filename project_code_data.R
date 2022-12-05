

knitr::opts_chunk$set(echo = TRUE,message = FALSE,warning = FALSE)
library(readxl)
library(tidyverse)
library(janitor)
library(rgl)
library(car)
library(ggfortify)
library(ggforce)
library(data.table)
library(easystats)
library(performance)
knitr::knit_hooks$set(webgl = hook_webgl)
cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#CC79A7") # custom color-blind friendly palette

install.packages("rgl")
install.packages("car")
install.packages("ggfortify")
install.packages("ggforce")
install.packages("MASS")

print_cols <- function(x) cat(colnames(x),sep="\n")


wild_log10 <- read_excel("wild_log10_As_Cd_Ni.xlsx")
View(wild_log10)  

#just the core12
cultivated_log10_As_Cd_Ni <- read_excel("wild_log10_As_Cd_Ni.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM",species))
View(cultivated_log10_As_Cd_Ni)

Test_cultivated_AMF <- read_excel("Test_cultivated_AMF.xlsx", 
  skip = 1)
View(Test_cultivated_AMF)


merged_metals_amf <- merge.data.frame(cultivated_log10_As_Cd_Ni, Test_cultivated_AMF, by = "species" , all = TRUE)
View(merged_metals_amf)


#cor
merged_metals_amf %>% 
  select (`Mean Colonization Rate (LS Mean)`,As:Ni) %>% cor(use = "pair")

glimpse(merged_metals_amf)

#simple scatter plots
ggplot(merged_metals_amf, aes(x = `Mean Colonization Rate (LS Mean)`, y = `As`)) +
  geom_point()

ggplot(merged_metals_amf, aes(x = `Mean Colonization Rate (LS Mean)`, y = `Cd`)) +
  geom_point()

ggplot(merged_metals_amf, aes(x = `Mean Colonization Rate (LS Mean)`, y = `Ni`)) +
  geom_point()

#correlation tests
cor.test(~ `Mean Colonization Rate (LS Mean)` + `As`, method = "pearson", data = merged_metals_amf)
#Not likely to be correlated

cor.test(~ `Mean Colonization Rate (LS Mean)` + `Cd`, method = "pearson", data = merged_metals_amf)
#Stronger correlation, negatively correlated

cor.test(~ `Mean Colonization Rate (LS Mean)` + `Ni`, method = "pearson", data = merged_metals_amf)
#Not significantly correlated



#Looking at wild_metal_dropped, going to cut out the non-core and take the mean of the columns
#nothing works and it makes me sad

#just the core 12 metals
test_metal_dropped <- read_excel("test_metal_dropped.xlsx")
View(test_metal_dropped)


#not working
mean_metals <- test_metal_dropped %>% group_by(species) %>% summarise(across(cols = 18:39, fns = mean))



#SAM_20
metals_020 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_020",species))


Means_020 <- colMeans(metals_020[c(5,18:39)], na.rm = TRUE)
final_means_020 <- as.data.frame(t(Means_020))
View(final_means_020)

#22
metals_022 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_022",species))

Means_022 <- colMeans(metals_022[c(5,18:39)], na.rm = TRUE)
final_means_022 <- as.data.frame(t(Means_022))
View(final_means_022)



#27
metals_027 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_027",species))

Means_027 <- colMeans(metals_027[c(5,18:39)], na.rm = TRUE)
final_means_027 <- as.data.frame(t(Means_027))
View(final_means_027)

#93
metals_093 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_093",species))

Means_093 <- colMeans(metals_093[c(5,18:39)], na.rm = TRUE)
final_means_093 <- as.data.frame(t(Means_093))
View(final_means_093)

#SAM 94
metals_094 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_094",species))

Means_094 <- colMeans(metals_094[c(5,18:39)], na.rm = TRUE)
final_means_094 <- as.data.frame(t(Means_094))
View(final_means_094)

#176
metals_176 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_176",species))

Means_176 <- colMeans(metals_176[c(5,18:39)], na.rm = TRUE)
final_means_176 <- as.data.frame(t(Means_176))
View(final_means_176)

#185
metals_185 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_185",species))

Means_185 <- colMeans(metals_185[c(5,18:39)], na.rm = TRUE)
final_means_185 <- as.data.frame(t(Means_185))
View(final_means_185)

#191
metals_191 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_191",species))

Means_191 <- colMeans(metals_191[c(5,18:39)], na.rm = TRUE)
final_means_191 <- as.data.frame(t(Means_191))
View(final_means_191)

#203
metals_203 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_203",species))

Means_203 <- colMeans(metals_203[c(5,18:39)], na.rm = TRUE)
final_means_203 <- as.data.frame(t(Means_203))
View(final_means_203)

#237
metals_237 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_237",species))

Means_237 <- colMeans(metals_237[c(5,18:39)], na.rm = TRUE)
final_means_237 <- as.data.frame(t(Means_237))
View(final_means_237)

#240
metals_240 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_240",species))

Means_240 <- colMeans(metals_240[c(5,18:39)], na.rm = TRUE)
final_means_240 <- as.data.frame(t(Means_240))
View(final_means_240)

#262
metals_262 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_262",species))

Means_262 <- colMeans(metals_262[c(5,18:39)], na.rm = TRUE)
final_means_262 <- as.data.frame(t(Means_262))
View(final_means_262)

#Now time to get something useful from all this work


#doesnt work
merged_means_metals <- list(final_means_020, final_means_022, final_means_027, final_means_093, final_means_094, final_means_176, final_means_185, final_means_191, final_means_203, final_means_237, final_means_240, final_means_262) %>% reduce(merge, by = "Population")
View(merged_means_metals) 

#Why is nothing working


#pca of 20
amf_020 <- read_excel("Test_cultivated_AMF.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_020",species))

data(Means_020, amf_020)

pca <- prcomp(X,center = TRUE,scale. = TRUE)
summary(pca)





  


#Working with combined data sheet
library(readr)
CombinedCoreData <- read_csv("CombinedCoreData.csv")
View(CombinedCoreData)
glimpse(CombinedCoreData)


#simple scatter plots, they are missing SAMs 93, 94, and 262... I need to work with some more variables
ggplot(CombinedCoreData, aes(x = `Mean Colonization Rate (LS Mean)`, y = `Abovemass_salt`)) +
  geom_point()

#correlation tests, missing SAMs 93, 94, 262
cor.test(~ `Mean Colonization Rate (LS Mean)` + `Abovemass_salt`, method = "pearson", data = CombinedCoreData)
#Not likely correlated


#correlation?
merged_metals_amf %>% select (`Mean Colonization Rate (LS Mean)`,As:Ni) %>% correlation() %>% summary(
) %>% plot()

#goodone
merged_metals_amf %>% select (As:Ni,`Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
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



