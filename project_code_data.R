



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


library(readxl)
Data12S1 <- read_excel("Data12S1.xlsx")
View(Data12S1)

###didnt work for me? the sam names are different one has underscore
merged_metals_amf <- merge.data.frame(cultivated_log10_As_Cd_Ni, Test_cultivated_AMF, by = "species" , all = TRUE)
View(merged_metals_amf)

###worked for me
merged_metals_amf <- merge(cultivated_log10_As_Cd_Ni, Data12S1, by = "species" , all = TRUE)
View(merged_metals_amf)

###goodone red blue cor

merged_metals_amf %>% select (As:Ni, `Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
) %>% plot()

#cor
merged_metals_amf %>% 
  select (`Mean Colonization Rate (LS Mean)`,As:Ni) %>% cor(use = "pair")

glimpse(merged_metals_amf)

#simple scatter plots #### added ablines
ggplot(merged_metals_amf, aes(x = `Mean Colonization Rate (LS Mean)`, y = `As`)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE) 


ggplot(merged_metals_amf, aes(x = `Mean Colonization Rate (LS Mean)`, y = `Cd`)) +
  geom_point()+ geom_smooth(method = "lm", se = TRUE) 

ggplot(merged_metals_amf, aes(x = `Mean Colonization Rate (LS Mean)`, y = `Ni`)) +
  geom_point()+ geom_smooth(method = "lm", se = TRUE) 

#correlation tests
cor.test(~ `Mean Colonization Rate (LS Mean)` + `As`, method = "pearson", data = merged_metals_amf)
#Not likely to be correlated

cor.test(~ `Mean Colonization Rate (LS Mean)` + `Cd`, method = "pearson", data = merged_metals_amf)
#Stronger correlation, negatively correlated

cor.test(~ `Mean Colonization Rate (LS Mean)` + `Ni`, method = "pearson", data = merged_metals_amf)
#Not significantly correlated

#get the cor
merged_metals_amf %>% 
  select (`Mean Colonization Rate (LS Mean)`,As:Ni) %>% cor(use = "pair")

#Looking at wild_metal_dropped, going to cut out the non-core and take the mean of the columns
#nothing works and it makes me sad

#just the core 12 metals
test_metal_dropped <- read_excel("test_metal_dropped.xlsx")
View(test_metal_dropped)


#not working ### same
mean_metals <- test_metal_dropped %>% group_by(species) %>% summarise(across(cols = 18:39, fns = mean))
view(mean_metals)


#SAM_20
metals_020 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_020",species))


Means_020 <- colMeans(metals_020[c(5,18:39)], na.rm = TRUE)
final_means_020 <- as.data.frame(t(Means_020))
View(final_means_020)




Means_020 <- colMeans(metals_020[c( 18:39)], na.rm = TRUE)
final_means_020 <- as.data.frame(t(Means_020))
View(final_means_020)

clipr::write_clip(final_means_020)
clipr::write_clip(final_means_)


#22
metals_022 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_022",species))

Means_022 <- colMeans(metals_022[c(5,18:39)], na.rm = TRUE)
final_means_022 <- as.data.frame(t(Means_022))
View(final_means_022)

write.excel022 <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(final_means_022)

clipr::write_clip(final_means_022)



#27
metals_027 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_027",species))

Means_027 <- colMeans(metals_027[c(5,18:39)], na.rm = TRUE)
final_means_027 <- as.data.frame(t(Means_027))
View(final_means_027)

clipr::write_clip(final_means_027)


#93
metals_093 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_093",species))

Means_093 <- colMeans(metals_093[c(5,18:39)], na.rm = TRUE)
final_means_093 <- as.data.frame(t(Means_093))
View(final_means_093)

clipr::write_clip(final_means_093)

#SAM 94
metals_094 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_094",species))

Means_094 <- colMeans(metals_094[c(5,18:39)], na.rm = TRUE)
final_means_094 <- as.data.frame(t(Means_094))
View(final_means_094)

clipr::write_clip(final_means_094)

#176
metals_176 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_176",species))

Means_176 <- colMeans(metals_176[c(5,18:39)], na.rm = TRUE)
final_means_176 <- as.data.frame(t(Means_176))
View(final_means_176)

clipr::write_clip(final_means_176)


#185
metals_185 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_185",species))

Means_185 <- colMeans(metals_185[c(5,18:39)], na.rm = TRUE)
final_means_185 <- as.data.frame(t(Means_185))
View(final_means_185)


clipr::write_clip(final_means_185)

#191
metals_191 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_191",species))

Means_191 <- colMeans(metals_191[c(5,18:39)], na.rm = TRUE)
final_means_191 <- as.data.frame(t(Means_191))
View(final_means_191)


clipr::write_clip(final_means_191)

#203
metals_203 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_203",species))

Means_203 <- colMeans(metals_203[c(5,18:39)], na.rm = TRUE)
final_means_203 <- as.data.frame(t(Means_203))
View(final_means_203)


clipr::write_clip(final_means_203)

#237
metals_237 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_237",species))

Means_237 <- colMeans(metals_237[c(5,18:39)], na.rm = TRUE)
final_means_237 <- as.data.frame(t(Means_237))
View(final_means_237)

clipr::write_clip(final_means_237)



#240
metals_240 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_240",species))

Means_240 <- colMeans(metals_240[c(5,18:39)], na.rm = TRUE)
final_means_240 <- as.data.frame(t(Means_240))
View(final_means_240)


clipr::write_clip(final_means_240)



#262
metals_262 <- read_excel("test_metal_dropped.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_262",species))

Means_262 <- colMeans(metals_262[c(5,18:39)], na.rm = TRUE)
final_means_262 <- as.data.frame(t(Means_262))
View(final_means_262)


clipr::write_clip(final_means_262)


merged_means_metals <- read_excel("copying the indiv meansgood.xlsx")
View(merged_means_metals)  



#Now time to get something useful from all this work
merged_means_metals <- data.frame(final_means_020, final_means_022, final_means_027, final_means_093, final_means_094, final_means_176, final_means_185, final_means_191, final_means_203, final_means_237, final_means_240, final_means_262) %>% reduce(merge, by = "Population")
View(merged_means_metals) 


#doesnt work
merged_means_metals <- list(final_means_020, final_means_022, final_means_027, final_means_093, final_means_094, final_means_176, final_means_185, final_means_191, final_means_203, final_means_237, final_means_240, final_means_262) %>% reduce(merge, by = "Population")
View(merged_means_metals) 

#Why is nothing working

###########merging all the frames

****************************************yay *************************
merged_metals_amf_andmeans <- merge.data.frame(merged_means_metals, merged_metals_amf, by = "species" , all = TRUE)
View(merged_metals_amf_andmeans)


#pca of 20
amf_020 <- read_excel("Test_cultivated_AMF.xlsx") %>% as.data.frame(.) %>% filter(grepl("SAM_020",species))

data(Means_020, amf_020)

pca <- prcomp(X,center = TRUE,scale. = TRUE)
summary(pca)


pca <- prcomp(,center = TRUE,scale. = TRUE)
summary(pca)

metal_means_pca <- prcomp(merged_means_metals[,c(2:23)], center = TRUE,scale. = TRUE)
summary(metal_means_pca)

library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(metal_means_pca)

ggbiplot(metal_means_pca, labels=rownames(metal_means_pca))



pcaofall <- prcomp(,center = TRUE,scale. = TRUE)
summary(pcaofall)

metal_means_pca_andamf <- prcomp(merged_metals_amf_andmeans[,c(2:29)], center = TRUE,scale. = TRUE)
summary(metal_means_pca_andamf)

library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(metal_means_pca_andamf)


merged_means_metals

merged_metals_amf_andmeans %>% select (As:Ni, `Mean Colonization Rate (LS Mean)`) %>% correlation() %>% summary(
) %>% plot()


modelamfbymetals <- lm(`Mean Colonization Rate (LS Mean)` ~ As+Ni+ Cd, data = merged_metals_amf)
summary(modelamfbymetals)

```````from lab 7 


autoplot(metal_means_pca,loadings = TRUE,loadings.label = TRUE,
         x = 1, # which PC to plot for x-axis
         y = 2) + # which PC to plot for y-axis
  geom_mark_ellipse(mapping = aes(fill = As),alpha=.3,expand = unit(0.5,'mm')) + 
  geom_point(mapping = aes(color = As),size = 3) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  theme_classic()

# 3D plot of selected variables
scatter3d(x = sim_plant$defense,y = sim_plant$herbivory,z = sim_plant$fitness,
          point.col = cbPalette[as.factor(sim_plant$TYPE)],
          surface.alpha=.00,
          residuals = FALSE,
          sphere.size=1.5,
          fogtype="none",
          xlab = "Trait x",
          ylab = "Trait y",
          zlab = "Trait z")
