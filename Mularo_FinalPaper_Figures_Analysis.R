# Urbanization on Native and Invasive Herp Spatial Partitioning ----------------
# Andrew Mularo 
# Data Science for Biologists Final Project 


## Presentation Figures 



## Load libraries 

library(tidyverse)
library(ggpubr)
library(RColorBrewer)

setwd("C://Users/andre/OneDrive/Documents/2022_DataScienceforBiologists/Final_Project/new_data")

## Checking color blind friendly palettes

display.brewer.all(colorblindFriendly = T)

## Will use Dark 2 for all figures 

## Load datasets 

## All data available at https://github.com/andrewmularo/2022_Invasive_Herps

herp_occurence<- read_tsv("./0105536-220831081235567/0105536-220831081235567.csv")
head(herp_occurence)
glimpse(herp_occurence)
dim(herp_occurence)

## Here, I am filtering out all occurences where lat and long are missing, 
## as well as restricting all of the species to Florida
herp_occurrence_filtered<- herp_occurence %>% 
  select(gbifID, genus, species, stateProvince, decimalLatitude, decimalLongitude, day, month, year) %>% 
  drop_na(decimalLatitude, decimalLongitude) %>% 
  filter(year >= 2010, year <= 2016) %>% 
  filter(stateProvince == "Florida")

dim(herp_occurrence_filtered)




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

## Now, I will put the herp occurence filtered dataset into arcGIS 


herp_occurrence_filtered<- read.csv("FloridaMuseum_Occurence_filtered.csv")

## This contains the impervious surface measurements for all occurence data 
impervioussurface<- read_csv("FloridaHerps_Impervious.csv")

## This dataset contains the reference to connect arcGIS OBJECTID with gbifID
dissolved_buffer<- read_csv("FloridaHerp_Dissolved_Reference.csv")

## Precipitation Data 2016, denoted each variable by month

Prec1<- read.csv("Prec_1_FIXED.csv")
Prec1<- rename(Prec1, Mean_Jan= MEAN, Count_Jan = COUNT, Area_Jan = AREA, Min_Jan=MIN, Max_Jan=MAX, Range_Jan=RANGE, STD_Jan=STD, Sum_Jan=SUM)
Prec2<- read.csv("Prec_2_FIXED.csv")
Prec2<- rename(Prec2, Mean_Feb= MEAN,  Count_Feb = COUNT, Area_Feb = AREA, Min_Feb=MIN, Max_Feb=MAX, Range_Feb=RANGE, STD_Feb=STD, Sum_Feb=SUM)
Prec3<- read.csv("Prec_3_FIXED.csv")
Prec3<- rename(Prec3, Mean_Mar= MEAN,  Count_Mar = COUNT, Area_Mar = AREA, Min_Mar=MIN, Max_Mar=MAX, Range_Mar=RANGE, STD_Mar=STD, Sum_Mar=SUM)
Prec4<- read.csv("Prec_4_FIXED.csv")
Prec4<- rename(Prec4, Mean_April= MEAN,  Count_April = COUNT, Area_April = AREA, Min_April=MIN, Max_April=MAX, Range_April=RANGE, STD_April=STD, Sum_April=SUM)
Prec5<- read.csv("Prec_5_FIXED.csv")
Prec5<- rename(Prec5, Mean_May= MEAN,  Count_May = COUNT, Area_May = AREA, Min_May=MIN, Max_May=MAX, Range_May=RANGE, STD_May=STD, Sum_May=SUM)
Prec6<- read.csv("Prec_6_FIXED.csv")
Prec6<- rename(Prec6, Mean_June= MEAN,  Count_June = COUNT, Area_June = AREA, Min_June=MIN, Max_June=MAX, Range_June=RANGE, STD_June=STD, Sum_June=SUM)
Prec7<- read.csv("Prec_7_FIXED.csv")
Prec7<- rename(Prec7, Mean_July= MEAN,  Count_July = COUNT, Area_July = AREA, Min_July=MIN, Max_July=MAX, Range_July=RANGE, STD_July=STD, Sum_July=SUM)
Prec8<- read.csv("Prec_8_FIXED.csv")
Prec8<- rename(Prec8, Mean_Aug= MEAN,  Count_Aug = COUNT, Area_Aug = AREA, Min_Aug=MIN, Max_Aug=MAX, Range_Aug=RANGE, STD_Aug=STD, Sum_Aug=SUM)
Prec9<- read.csv("Prec_9_FIXED.csv")
Prec9<- rename(Prec9, Mean_Sep= MEAN,  Count_Sep = COUNT, Area_Sep = AREA, Min_Sep=MIN, Max_Sep=MAX, Range_Sep=RANGE, STD_Sep=STD, Sum_Sep=SUM)
Prec10<- read.csv("Prec_10_FIXED.csv")
Prec10<- rename(Prec10, Mean_Oct= MEAN,  Count_Oct = COUNT, Area_Oct = AREA, Min_Oct=MIN, Max_Oct=MAX, Range_Oct=RANGE, STD_Oct=STD, Sum_Oct=SUM)
Prec11<- read.csv("Prec_11_FIXED.csv")
Prec11<- rename(Prec11, Mean_Nov= MEAN,  Count_Nov = COUNT, Area_Nov = AREA, Min_Nov=MIN, Max_Nov=MAX, Range_Nov=RANGE, STD_Nov=STD, Sum_Nov=SUM)
Prec12<- read.csv("Prec_12_FIXED.csv")
Prec12<- rename(Prec12, Mean_Dec= MEAN,  Count_Dec = COUNT, Area_Dec = AREA, Min_Dec=MIN, Max_Dec=MAX, Range_Dec=RANGE, STD_Dec=STD, Sum_Dec=SUM)

## Merge all precipitation data together 
preca<- inner_join(Prec1, Prec2)
precb<- inner_join(preca, Prec3)
precc<- inner_join(precb, Prec4)
precd<- inner_join(precc, Prec5)
prece<- inner_join(precd, Prec6)
precf<- inner_join(prece, Prec7)
precg<- inner_join(precf, Prec8)
prech<- inner_join(precg, Prec9)
precj<- inner_join(prech, Prec10)
precl<- inner_join(precj, Prec11)
precm<- inner_join(precl, Prec12)

## Calcuated the mean annual precipitation across all 12 months
Precipitation<- precm %>% mutate(mean = (Mean_Jan+Mean_Feb+Mean_Mar+Mean_April+Mean_May+Mean_June+Mean_July+Mean_Aug+Mean_Sep+Mean_Oct+Mean_Nov+Mean_Dec)/12)



### Maximum Temperature 2016, denoted each variable by month

Tmax1<- read.csv("Tmax_1_FIXED.csv")
Tmax1<- rename(Tmax1, Mean_Jan= MEAN, Count_Jan = COUNT, Area_Jan = AREA, Min_Jan=MIN, Max_Jan=MAX, Range_Jan=RANGE, STD_Jan=STD, Sum_Jan=SUM)
Tmax2<- read.csv("Tmax_2_FIXED.csv")
Tmax2<- rename(Tmax2, Mean_Feb= MEAN,  Count_Feb = COUNT, Area_Feb = AREA, Min_Feb=MIN, Max_Feb=MAX, Range_Feb=RANGE, STD_Feb=STD, Sum_Feb=SUM)
Tmax3<- read.csv("Tmax_3_FIXED.csv")
Tmax3<- rename(Tmax3, Mean_Mar= MEAN,  Count_Mar = COUNT, Area_Mar = AREA, Min_Mar=MIN, Max_Mar=MAX, Range_Mar=RANGE, STD_Mar=STD, Sum_Mar=SUM)
Tmax4<- read.csv("Tmax_4_FIXED.csv")
Tmax4<- rename(Tmax4, Mean_April= MEAN,  Count_April = COUNT, Area_April = AREA, Min_April=MIN, Max_April=MAX, Range_April=RANGE, STD_April=STD, Sum_April=SUM)
Tmax5<- read.csv("Tmax_5_FIXED.csv")
Tmax5<- rename(Tmax5, Mean_May= MEAN,  Count_May = COUNT, Area_May = AREA, Min_May=MIN, Max_May=MAX, Range_May=RANGE, STD_May=STD, Sum_May=SUM)
Tmax6<- read.csv("Tmax_6_FIXED.csv")
Tmax6<- rename(Tmax6, Mean_June= MEAN,  Count_June = COUNT, Area_June = AREA, Min_June=MIN, Max_June=MAX, Range_June=RANGE, STD_June=STD, Sum_June=SUM)
Tmax7<- read.csv("Tmax_7_FIXED.csv")
Tmax7<- rename(Tmax7, Mean_July= MEAN,  Count_July = COUNT, Area_July = AREA, Min_July=MIN, Max_July=MAX, Range_July=RANGE, STD_July=STD, Sum_July=SUM)
Tmax8<- read.csv("Tmax_8_FIXED.csv")
Tmax8<- rename(Tmax8, Mean_Aug= MEAN,  Count_Aug = COUNT, Area_Aug = AREA, Min_Aug=MIN, Max_Aug=MAX, Range_Aug=RANGE, STD_Aug=STD, Sum_Aug=SUM)
Tmax9<- read.csv("Tmax_9_FIXED.csv")
Tmax9<- rename(Tmax9, Mean_Sep= MEAN,  Count_Sep = COUNT, Area_Sep = AREA, Min_Sep=MIN, Max_Sep=MAX, Range_Sep=RANGE, STD_Sep=STD, Sum_Sep=SUM)
Tmax10<- read.csv("Tmax_10_FIXED.csv")
Tmax10<- rename(Tmax10, Mean_Oct= MEAN,  Count_Oct = COUNT, Area_Oct = AREA, Min_Oct=MIN, Max_Oct=MAX, Range_Oct=RANGE, STD_Oct=STD, Sum_Oct=SUM)
Tmax11<- read.csv("Tmax_11_FIXED.csv")
Tmax11<- rename(Tmax11, Mean_Nov= MEAN,  Count_Nov = COUNT, Area_Nov = AREA, Min_Nov=MIN, Max_Nov=MAX, Range_Nov=RANGE, STD_Nov=STD, Sum_Nov=SUM)
Tmax12<- read.csv("Tmax_12_FIXED.csv")
Tmax12<- rename(Tmax12, Mean_Dec= MEAN,  Count_Dec = COUNT, Area_Dec = AREA, Min_Dec=MIN, Max_Dec=MAX, Range_Dec=RANGE, STD_Dec=STD, Sum_Dec=SUM)

## Merge all max temp data together 
Tmaxa<- inner_join(Tmax1, Tmax2)
Tmaxb<- inner_join(Tmaxa, Tmax3)
Tmaxc<- inner_join(Tmaxb, Tmax4)
Tmaxd<- inner_join(Tmaxc, Tmax5)
Tmaxe<- inner_join(Tmaxd, Tmax6)
Tmaxf<- inner_join(Tmaxe, Tmax7)
Tmaxg<- inner_join(Tmaxf, Tmax8)
Tmaxh<- inner_join(Tmaxg, Tmax9)
Tmaxj<- inner_join(Tmaxh, Tmax10)
Tmaxl<- inner_join(Tmaxj, Tmax11)
Tmaxm<- inner_join(Tmaxl, Tmax12)

# Calculated the mean annual maximum temperature across all 12 months 
Max_Temp<- Tmaxm %>% mutate(mean = (Mean_Jan+Mean_Feb+Mean_Mar+Mean_April+Mean_May+Mean_June+Mean_July+Mean_Aug+Mean_Sep+Mean_Oct+Mean_Nov+Mean_Dec)/12)



### Minimum Temperature 2016, denoted each variable by month
Tmin1<- read.csv("Tmin_1_FIXED.csv")
Tmin1<- rename(Tmin1, Mean_Jan= MEAN, Count_Jan = COUNT, Area_Jan = AREA, Min_Jan=MIN, Max_Jan=MAX, Range_Jan=RANGE, STD_Jan=STD, Sum_Jan=SUM)
Tmin2<- read.csv("Tmin_2_FIXED.csv")
Tmin2<- rename(Tmin2, Mean_Feb= MEAN,  Count_Feb = COUNT, Area_Feb = AREA, Min_Feb=MIN, Max_Feb=MAX, Range_Feb=RANGE, STD_Feb=STD, Sum_Feb=SUM)
Tmin3<- read.csv("Tmin_3_FIXED.csv")
Tmin3<- rename(Tmin3, Mean_Mar= MEAN,  Count_Mar = COUNT, Area_Mar = AREA, Min_Mar=MIN, Max_Mar=MAX, Range_Mar=RANGE, STD_Mar=STD, Sum_Mar=SUM)
Tmin4<- read.csv("Tmin_4_FIXED.csv")
Tmin4<- rename(Tmin4, Mean_April= MEAN,  Count_April = COUNT, Area_April = AREA, Min_April=MIN, Max_April=MAX, Range_April=RANGE, STD_April=STD, Sum_April=SUM)
Tmin5<- read.csv("Tmin_5_FIXED.csv")
Tmin5<- rename(Tmin5, Mean_May= MEAN,  Count_May = COUNT, Area_May = AREA, Min_May=MIN, Max_May=MAX, Range_May=RANGE, STD_May=STD, Sum_May=SUM)
Tmin6<- read.csv("Tmin_6_FIXED.csv")
Tmin6<- rename(Tmin6, Mean_June= MEAN,  Count_June = COUNT, Area_June = AREA, Min_June=MIN, Max_June=MAX, Range_June=RANGE, STD_June=STD, Sum_June=SUM)
Tmin7<- read.csv("Tmin_7_FIXED.csv")
Tmin7<- rename(Tmin7, Mean_July= MEAN,  Count_July = COUNT, Area_July = AREA, Min_July=MIN, Max_July=MAX, Range_July=RANGE, STD_July=STD, Sum_July=SUM)
Tmin8<- read.csv("Tmin_8_FIXED.csv")
Tmin8<- rename(Tmin8, Mean_Aug= MEAN,  Count_Aug = COUNT, Area_Aug = AREA, Min_Aug=MIN, Max_Aug=MAX, Range_Aug=RANGE, STD_Aug=STD, Sum_Aug=SUM)
Tmin9<- read.csv("Tmin_9_FIXED.csv")
Tmin9<- rename(Tmin9, Mean_Sep= MEAN,  Count_Sep = COUNT, Area_Sep = AREA, Min_Sep=MIN, Max_Sep=MAX, Range_Sep=RANGE, STD_Sep=STD, Sum_Sep=SUM)
Tmin10<- read.csv("Tmin_10_FIXED.csv")
Tmin10<- rename(Tmin10, Mean_Oct= MEAN,  Count_Oct = COUNT, Area_Oct = AREA, Min_Oct=MIN, Max_Oct=MAX, Range_Oct=RANGE, STD_Oct=STD, Sum_Oct=SUM)
Tmin11<- read.csv("Tmin_11_FIXED.csv")
Tmin11<- rename(Tmin11, Mean_Nov= MEAN,  Count_Nov = COUNT, Area_Nov = AREA, Min_Nov=MIN, Max_Nov=MAX, Range_Nov=RANGE, STD_Nov=STD, Sum_Nov=SUM)
Tmin12<- read.csv("Tmin_12_FIXED.csv")
Tmin12<- rename(Tmin12, Mean_Dec= MEAN,  Count_Dec = COUNT, Area_Dec = AREA, Min_Dec=MIN, Max_Dec=MAX, Range_Dec=RANGE, STD_Dec=STD, Sum_Dec=SUM)

## Join all Tmin data together 
Tmina<- inner_join(Tmin1, Tmin2)
Tminb<- inner_join(Tmina, Tmin3)
Tminc<- inner_join(Tminb, Tmin4)
Tmind<- inner_join(Tminc, Tmin5)
Tmine<- inner_join(Tmind, Tmin6)
Tminf<- inner_join(Tmine, Tmin7)
Tming<- inner_join(Tminf, Tmin8)
Tminh<- inner_join(Tming, Tmin9)
Tminj<- inner_join(Tminh, Tmin10)
Tminl<- inner_join(Tminj, Tmin11)
Tminm<- inner_join(Tminl, Tmin12)

## Calculate the annual mean minumum temperature across all 12 months 
Min_Temp<- Tminm %>% mutate(mean = (Mean_Jan+Mean_Feb+Mean_Mar+Mean_April+Mean_May+Mean_June+Mean_July+Mean_Aug+Mean_Sep+Mean_Oct+Mean_Nov+Mean_Dec)/12)



## Here, I am assigning the species as either invasive or native

invasive<- herp_occurrence_filtered %>% 
  filter(species %in% c("Agama picticauda",
         "Ameiva ameiva",
         "Anolis distichus",
         "Basiliscus vittatus",
         "Boa constrictor",
         "Caiman crocodilus",
         "Centrochelys sulcata",
         "Chamaeleo calyptratus",
         "Ctenosaura similis",
         "Eleutherodactylus planirostris",
         "Furcifer oustaleti",
         "Gekko gecko",
         "Hemidactylus mabouia",
         "Hemidactylus turcicus",
         "Iguana iguana",
         "Indotyphlops braminus",
         "Leiocephalus carinatus",
         "Osteopilus septentrionalis",
         "Phelsuma grandis",
         "Python bivittatus",
         "Python regius",
         "Python sebae",
         "Rhinella marina",
         "Salvator merianae",
         "Varanus niloticus")
  ) %>% 
  mutate(Status = "Invasive")


native<- herp_occurrence_filtered %>% 
  filter(species %in% c("Agkistrodon piscivorus",
                        "Alligator mississippiensis",
                        "Anaxyrus terrestris",
                        "Anolis carolinensis",
                        "Coluber constrictor",
                        "Crotalus adamanteus",
                        "Diadophis punctatus",
                        "Dryophytes cinereus",
                        "Kinosternon baurii",
                        "Liodytes pygaea",
                        "Lithobates sphenocephalus",
                        "Macrochelys temminckii",
                        "Masticophis flagellum",
                        "Micrurus fulvius",
                        "Nerodia clarkii",
                        "Nerodia fasciata",
                        "Notophthalmus viridescens",
                        "Pantherophis alleghaniensis",
                        "Pantherophis guttatus",
                        "Pseudacris feriarum",
                        "Pseudacris nigrita",
                        "Pseudemys concinna",
                        "Rhineura floridana",
                        "Sistrurus miliarius",
                        "Tantilla relicta",
                        "Thamnophis saurita",
                        "Trachemys scripta")
         ) %>% 
  mutate(Status = "Native")


herp_occurrence_filtered<- rbind(invasive, native)


### Inner joing the dissolved buffer dataset with the filtered herp dataset 

florida_herps_arcGIS<- inner_join(herp_occurrence_filtered, dissolved_buffer)


##Inner-joining all of the variables 
florida_herp_impervious<- inner_join(florida_herps_arcGIS, impervioussurface)
dim(florida_herp_impervious)

florida_herp_precipitation<- inner_join(florida_herps_arcGIS, Precipitation)
dim(florida_herp_precipitation)

florida_herp_maxtemp<- inner_join(florida_herps_arcGIS, Max_Temp)
dim(florida_herp_maxtemp)

florida_herp_mintemp<- inner_join(florida_herps_arcGIS, Min_Temp)
dim(florida_herp_mintemp)

#### Figures and analysis for Impervious surfaces

## Subsetting for amphibians only 


florida_herp_impervious_amphibian<- florida_herp_impervious %>% 
  filter(class == "Amphibia") %>% 
  arrange(Status)
dim(florida_herp_impervious_amphibian)

# By status
amphibian_status_impervious<- ggplot(florida_herp_impervious_amphibian)+
  aes(x=MEAN, y = Status, fill = Status)+
  geom_boxplot()+
  xlab("Mean Percent Impervious Surface")+
  ylab("Status")+
  ggtitle("Amphibia")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(14),
        legend.text = element_text(15),
        legend.position = "none")

amphibian_status_impervious


## By species 
amphibian_impervious_plot<- ggplot(florida_herp_impervious_amphibian)+
  aes(x=MEAN, y=reorder(species, MEAN), fill = Status)+
  geom_boxplot()+
  xlab("Mean Percent Impervious Surface")+
  ylab("Status")+
  ggtitle("Amphibia")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 15))
amphibian_impervious_plot

## Subsetting for squamata
florida_herp_impervious_squamata<- florida_herp_impervious %>% 
  filter(order == "Squamata")
dim(florida_herp_impervious_squamata)

# By status 
squamata_status_impervious<- ggplot(florida_herp_impervious_squamata)+
  aes(x=MEAN, y = Status, fill = Status)+
  geom_boxplot()+
  xlab("Mean Percent Impervious Surface")+
  ylab("Status")+
  ggtitle("Squamata")+ 
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(14),
        legend.text = element_text(15),
        legend.position = "none")

squamata_status_impervious


## By species 
squamata_impervious_plot<- ggplot(florida_herp_impervious_squamata)+
  aes(x=MEAN, y=reorder(species, MEAN), fill = Status)+
  geom_boxplot()+
  xlab("Mean Percent Impervious Surface")+
  ylab("Species")+
  ggtitle("Squamata")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 15))

squamata_impervious_plot



#### Figures and analysis for Precipitation 

## Subsetting for amphibians only 

florida_herp_precipitation_amphibian<- florida_herp_precipitation %>% 
  filter(class == "Amphibia") %>% 
  arrange(Status)
dim(florida_herp_precipitation_amphibian)


# By status 
amphibian_status_precipitation<- ggplot(florida_herp_precipitation_amphibian)+
  aes(x=mean, y = Status, fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Precipitation (mm)")+
  ylab("Status")+
  ggtitle("Amphibia")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(14),
        legend.text = element_text(15),
        legend.position = "none")

amphibian_status_precipitation


# By species 
amphibian_precipitation_plot<- ggplot(florida_herp_precipitation_amphibian)+
  aes(x=mean, y=reorder(species, mean), fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Precipitation (mm)")+
  ylab("Species")+
  ggtitle("Amphibia")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 15))

amphibian_precipitation_plot

## Subsetting for squamata
florida_herp_precipitation_squamata<- florida_herp_precipitation %>% 
  filter(order == "Squamata")
dim(florida_herp_precipitation_squamata)

# By status 
squamata_status_precipitation<- ggplot(florida_herp_precipitation_squamata)+
  aes(x=mean, y = Status, fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Precipitation (mm)")+
  ylab("Status")+
  ggtitle("Squamata")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(14),
        legend.text = element_text(15),
        legend.position = "none")

squamata_status_precipitation

## By species 
squamata_precipitation_plot<- ggplot(florida_herp_precipitation_squamata)+
  aes(x=mean, y=reorder(species, mean), fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Precipitation (mm)")+
  ylab("Species")+
  ggtitle("Squamata")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 15))

squamata_precipitation_plot



#### Figures and analysis for Maximum Temperature 

## Subsetting for amphibians only 

florida_herp_maxtemp_amphibian<- florida_herp_maxtemp %>% 
  filter(class == "Amphibia") %>% 
  arrange(Status)


# By status 
amphibian_status_maxtemp<- ggplot(florida_herp_maxtemp_amphibian)+
  aes(x=mean, y = Status, fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Max Temperature (C)")+
  ylab("Status")+
  ggtitle("Amphibia")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 18), 
        legend.title = element_text(14),
        legend.text = element_text(15),
        legend.position = "none")

amphibian_status_maxtemp


# By species 
amphibian_maxtemp_plot<- ggplot(florida_herp_maxtemp_amphibian)+
  aes(x=mean, y=reorder(species, mean), fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Max Temperature (C)")+
  ylab("Species")+
  ggtitle("Amphibia")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 15))
amphibian_maxtemp_plot

## Subsetting for squamata
florida_herp_maxtemp_squamata<- florida_herp_maxtemp %>% 
  filter(order == "Squamata")


# By status 
squamata_status_maxtemp<- ggplot(florida_herp_maxtemp_squamata)+
  aes(x=mean, y = Status, fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Max Temperature (C)")+
  ylab("Status")+
  ggtitle("Squamata")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(14),
        legend.text = element_text(15),
        legend.position = "none")

squamata_status_maxtemp


## By species 
squamata_maxtemp_plot<- ggplot(florida_herp_maxtemp_squamata)+
  aes(x=mean, y=reorder(species, mean), fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Max Temperature (C)")+
  ylab("Species")+
  ggtitle("Squamata")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 15))

squamata_maxtemp_plot



#### Figures and analysis for Minimum Temperature 

## Subsetting for amphibians only 

florida_herp_mintemp_amphibian<- florida_herp_mintemp %>% 
  filter(class == "Amphibia") %>% 
  arrange(Status)



# By status 
amphibian_status_mintemp<- ggplot(florida_herp_mintemp_amphibian)+
  aes(x=mean, y = Status, fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Min Temperature (C)")+
  ylab("Status")+
  ggtitle("Amphibia")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(14),
        legend.text = element_text(15),
        legend.position = "none")

amphibian_status_mintemp



## By species 
amphibian_mintemp_plot<- ggplot(florida_herp_mintemp_amphibian)+
  aes(x=mean, y = reorder(species, mean), fill = Status,)+
  geom_boxplot()+
  xlab("Mean Annual Min Temperature (C)")+
  ylab("Species")+
  ggtitle("Amphibia")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 15))

amphibian_mintemp_plot

## Subsetting for squamata
florida_herp_mintemp_squamata<- florida_herp_mintemp %>% 
  filter(order == "Squamata")



# By status 
squamata_status_mintemp<- ggplot(florida_herp_mintemp_squamata)+
  aes(x=mean, y = Status, fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Min Temperature (C)")+
  ylab("Status")+
  ggtitle("Squamata")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(14),
        legend.text = element_text(15),
        legend.position = "none")

squamata_status_mintemp


# By species 
squamata_mintemp_plot<- ggplot(florida_herp_mintemp_squamata)+
  aes(x=mean, y=reorder(species, mean), fill = Status)+
  geom_boxplot()+
  xlab("Mean Annual Min  Temperature (C)")+
  ylab("Species")+
  ggtitle("Squamata")+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 15))

squamata_mintemp_plot

## Statistical tests 


## Impervious Surfaces 

# Normality 
shapiro.test(florida_herp_impervious_amphibian$MEAN)
shapiro.test(florida_herp_impervious_squamata$MEAN)

## Wilcoxon tests 
t_amphibian_impervious<- wilcox.test(data = florida_herp_impervious_amphibian, MEAN ~ Status)
t_amphibian_impervious
summary(t_amphibian_impervious)

t_squamata_impervious<- wilcox.test(data = florida_herp_impervious_squamata, MEAN ~ Status)
t_squamata_impervious
summary(t_squamata_impervious)

## Precipitation 

# Normality 
shapiro.test(florida_herp_precipitation_amphibian$mean)
shapiro.test(florida_herp_precipitation_squamata$mean)

# Wilcoxon test 
t_amphibian_precipitation<- wilcox.test(data = florida_herp_precipitation_amphibian, mean ~ Status)
t_amphibian_precipitation
summary(t_amphibian_precipitation)

t_squamata_precipitation<- wilcox.test(data = florida_herp_precipitation_squamata, mean ~ Status)
t_squamata_precipitation
summary(t_squamata_precipitation)

## Maximum Temperature 

# Normality 
shapiro.test(florida_herp_maxtemp_amphibian$mean)
shapiro.test(florida_herp_maxtemp_squamata$mean)

# Wilcoxon test 
t_amphibian_maxtemp<- wilcox.test(data = florida_herp_maxtemp_amphibian, mean ~ Status)
t_amphibian_maxtemp
summary(t_amphibian_maxtemp)

t_squamata_maxtemp<- wilcox.test(data = florida_herp_maxtemp_squamata, mean ~ Status)
t_squamata_maxtemp
summary(t_squamata_maxtemp)


## Minimum Temperature 

# Normality 
shapiro.test(florida_herp_mintemp_amphibian$mean)
shapiro.test(florida_herp_mintemp_squamata$mean)

# Wilcoxon test 
t_amphibian_mintemp<- wilcox.test(data = florida_herp_mintemp_amphibian, mean ~ Status)
t_amphibian_mintemp
summary(t_amphibian_mintemp)

t_squamata_mintemp<- wilcox.test(data = florida_herp_mintemp_squamata, mean ~ Status)
t_squamata_mintemp
summary(t_squamata_mintemp)

## Final Figures 

Figure1<- ggarrange(amphibian_status_impervious, squamata_status_impervious)
Figure2<- ggarrange(amphibian_impervious_plot, squamata_impervious_plot)
Figure3<- ggarrange(amphibian_status_precipitation, squamata_status_precipitation)
Figure4<- ggarrange(amphibian_precipitation_plot, squamata_precipitation_plot)
Figure5<- ggarrange(amphibian_status_maxtemp, squamata_status_maxtemp)
Figure6<- ggarrange(amphibian_maxtemp_plot, squamata_maxtemp_plot)
Figure7<- ggarrange(amphibian_status_mintemp, squamata_status_mintemp)
Figure8<- ggarrange(amphibian_mintemp_plot, squamata_mintemp_plot)


ggsave(filename = "./Figure1.png", plot = Figure1, width = 10, height = 10, dpi = 300)
ggsave(filename = "./Figure2.png", plot = Figure2, width = 20, height = 10, dpi = 300)
ggsave(filename = "./Figure3.png", plot = Figure3, width = 10, height = 10, dpi = 300)
ggsave(filename = "./Figure4.png", plot = Figure4, width = 20, height = 10, dpi = 300)
ggsave(filename = "./Figure5.png", plot = Figure5, width = 10, height = 10, dpi = 300)
ggsave(filename = "./Figure6.png", plot = Figure6, width = 20, height = 10, dpi = 300)
ggsave(filename = "./Figure7.png", plot = Figure7, width = 10, height = 10, dpi = 300)
ggsave(filename = "./Figure8.png", plot = Figure8, width = 20, height = 10, dpi = 300)



