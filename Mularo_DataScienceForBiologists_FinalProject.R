# Urbanization on Native and Invasive Herp Spatial Partitioning ----------------
# Andrew Mularo 
# Data Science for Biologists Final Project 



## Load Libraries 

library(tidyverse)

## Set working directory 

setwd("C://Users/andre/OneDrive/Documents/2022_DataScienceforBiologists/Final_Project")

## Load dataset 

herp_occurence<- read_tsv("./0105536-220831081235567/0105536-220831081235567.csv")
head(herp_occurence)
glimpse(herp_occurence)
dim(herp_occurence)

## Here, I am filtering out all occurences where lat and long are missing, 
## as well as restricting all of the species to Florida, 
## and am restricting the years for between 2010 and 2016
herp_occurrence_filtered<- herp_occurence %>% 
  select(gbifID, class, order, family, genus, species, stateProvince, decimalLatitude, decimalLongitude, day, month, year) %>% 
  drop_na(decimalLatitude, decimalLongitude) %>% 
  filter(stateProvince == "Florida") %>% 
  filter(year >= 2010, year <= 2016) 

dim(herp_occurrence_filtered)


## Writing in new file 
write.csv(herp_occurrence_filtered, "FloridaMuseum_Occurence_filtered.csv")


### I am now discovering what species have more than 20 occurences 
## so that there is a robust enough sample size for each individual
herp_occurence_count<- herp_occurrence_filtered %>% 
  group_by(species) %>% 
  tally() %>% 
  filter(n >= 20) %>% 
  drop_na(species) 


dim(herp_occurence_count)

## I am now filtering out the species in the lat/long dataset 
## that have fewer than 20 occurence datapoints
herp_occurrence_filtered<- 
  herp_occurrence_filtered %>% 
  inner_join(herp_occurence_count)

dim(herp_occurrence_filtered)

### Now, I will move over to arcGIS Pro to obtain all variables


#### Uploading new datasets from arcGIS 

## This contains the canopy cover for all occurence data
canopycover<- read_csv("FloridaHeprs_CanopyCover.csv")

## This contains the impervious surface measurements for all occurence data 
impervioussurface<- read_csv("FloridaHerps_Impervious.csv")

## This dataset contains the reference to connect arcGIS OBJECTID with gbifID
dissolved_buffer<- read_csv("FloridaHerp_Dissolved_Reference.csv")


### Inner joing the dissolved buffer dataset with the filtered herp dataset 

florida_herps_arcGIS<- inner_join(herp_occurrence_filtered, dissolved_buffer)

florida_herp_impervious<- inner_join(florida_herps_arcGIS, impervioussurface)

florida_herp_canopy<- inner_join(florida_herps_arcGIS, canopycover)


# General Figure for Impervious surfaces

## Subsetting for amphibians only 

florida_herp_impervious_amphibian<- florida_herp_impervious %>% 
  filter(class == "Amphibia")

amphibian_plot<- ggplot(florida_herp_impervious_amphibian)+
  aes(x=MEAN, y = species)+
  geom_boxplot()+
  xlab("Mean Percent Impervious Surface")+
  ylab("Species")+
  ggtitle("Impervious Surface Occupied by Species")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16))

amphibian_plot

