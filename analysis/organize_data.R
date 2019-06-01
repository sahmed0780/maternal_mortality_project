#' ---
#' title: "organize_data.R"
#' author: ""
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory. 

#source in any useful functions
source("useful_functions.R")
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
install.packages("ggalt")
library(ggalt)
install.packages("ggrepel")
library(ggrepel)

# calculate MMR by region and year ----------------------------------------

mat_death <- read_tsv("input/mcd_wonder.txt",
                      n_max=204)

mat_death <- aggregate(Deaths~`Census Region`+Race+Year, 
                       sum, data=mat_death)
mat_death <- subset(mat_death, (Race=="White" | Race=="Black or African American") & Year>=2007)

births <- read_tsv("input/natality_wonder.txt",
                   n_max=197)
births <- aggregate(Births~`Census Region`+`Mother's Bridged Race`+Year, sum, data=births)
colnames(births)[2] <- "Race"

births <- subset(births, Race=="White" | Race=="Black or African American")

mat_mort <- merge(mat_death, births, by=c("Year", "Census Region"))
mat_mort$mmr <- mat_mort$Deaths/mat_mort$Births

temp1 <- subset(mat_mort, Race.x=="Black or African American",
                select=c("Census Region","Year","mmr"))
colnames(temp1)[3] <- "mmr_black"
colnames(temp1)[1] <- "Census_Region"

temp2 <- subset(mat_mort, Race.x=="White",
                select=c("Census Region","Year","mmr"))
test=filter(temp1, Census_Region== "Census Region 1: Northeast")
mean_black_mmr=c(mean(filter(temp1, Census_Region== "Census Region 1: Northeast")$mmr), 
                 mean(filter(temp1, Census_Region== "Census Region 2: Midwest")$mmr),
                 mean(filter(temp1, Census_Region== "Census Region 3: South")$mmr),
                 mean(filter(temp1, Census_Region== "Census Region 4: West")$mmr))

colnames(temp2)[3] <- "mmr_white"
colnames(temp2)[1] <- "Census_Region"
test=filter(temp2, Census_Region== "Census Region 1: Northeast")
mean_white_mmr=c(mean(filter(temp1, Census_Region== "Census Region 1: Northeast")$mmr), 
                 mean(filter(temp1, Census_Region== "Census Region 2: Midwest")$mmr),
                 mean(filter(temp1, Census_Region== "Census Region 3: South")$mmr),
                 mean(filter(temp1, Census_Region== "Census Region 4: West")$mmr))
barplot(mean_white_mmr)

mat_mort <- merge(temp1, temp2, by=c("Census_Region","Year"))
head(mat_mort)
table(mat_mort)
summary(mat_mort)
mat_mort$mmr_race <- mat_mort$mmr_black/mat_mort$mmr_white
table(mat_mort$mmr_race)
barplot(mat_mort$mmr_race)

mat_mort <- subset(mat_mort, Year>2007,
                   select=c("Census_region", "Year", "mmr_black", "mmr_white", "mmr_race"))



# Read in IPUMS Data ------------------------------------------------------

ipums <- read_fwf("input/usa_00006.dat.gz",
                  col_positions = fwf_positions(start = c(1,7, 9,20,23,27),
                                                end   = c(4,8,18,22,23,27),
                                                col_names = c("Year","Census_Region","perwt","age","race","hcovany")),
                  col_types = cols(.default = "i"), 
                                  progress = TRUE)

#subset to women of childbearing age, white and black and region identified
ipums <- subset(ipums, age>=18 & age<45 & race<3 & Census_Region<91 & Year>2007)

ipums$race <- factor(ipums$race,
                     levels=1:2,
                     labels=c("White","Black"))

ipums$health_insurance <- ipums$hcovany==2

ipums$region_big <- ifelse(ipums$Census_Region<21, "Census Region 1: Northeast",
                           ifelse(ipums$Census_Region<31, "Census Region 2: Midwest",
                                  ifelse(ipums$Census_Region<41, "Census Region 3: South", "Census Region 4: West")))
table(ipums$Census_Region, ipums$region_big, exclude=NULL)

ipums$health_insurance_weight <- (ipums$health_insurance * ipums$perwt)

ipums_temp1 <- aggregate(health_insurance_weight~region_big+Year, data=ipums, sum)
ipums_temp2 <- aggregate(perwt~region_big+Year, data=ipums, sum)
head(ipums_temp1)
head(ipums_temp2)
summary(ipums_temp1)
summary(ipums_temp2)

ipums_temp1$health_coverage <- 100*ipums_temp1$health_insurance_weight/ipums_temp2$perwt

ipums_agg <- ipums_temp1[,-3]
colnames(ipums_agg) <- c("Census_Region","Year","health_coverage")
head(ipums_agg)
mat_mort <- merge(mat_mort, ipums_agg, by=c("Census_Region","Year"))
head(mat_mort)
summary(mat_mort)

# Read in NHIS Data ------------------------------------------------------



ipums <- read_fwf("input/nhis_00007.dat.gz",
                  
                  col_positions = fwf_positions(start = c(1,5,7,19,28,30,31,34,35,36),
                                                
                                                end   = c(4,8,18,27,29,30,33,34,35,36),
                                                
                                                col_names = c("year","region","perwt","samplewt","age","sex","race","usualpl", "routcare","ybarcare")),
                  
                  col_types = cols(.default = "i"), #ensure that all variables are read in as integers
                  
                  progress = TRUE)



