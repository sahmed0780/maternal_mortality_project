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

mat_mort <- merge(mat_death, births, by=c("Year", "Census Region", "Race"))
mat_mort$mmr <- mat_mort$Deaths/mat_mort$Births

temp1 <- subset(mat_mort, Race=="Black or African American",
                select=c("Census Region","Year","mmr"))
colnames(temp1) <- c("region","year","mmr_black")

temp2 <- subset(mat_mort, Race=="White",
                select=c("Census Region","Year","mmr"))
colnames(temp2) <- c("region","year","mmr_white")

mat_mort <- merge(temp1, temp2, by=c("region","year"))
mat_mort$mmr_race <- mat_mort$mmr_black/mat_mort$mmr_white

mat_mort <- subset(mat_mort, year>2007,
                   select=c("region", "year", "mmr_black", "mmr_white", "mmr_race"))
summary(mat_mort)
tapply(mat_mort$mmr_race, mat_mort$region, mean)


# Read in IPUMS Data ------------------------------------------------------

ipums <- read_fwf("input/usa_00006.dat.gz",
                  col_positions = fwf_positions(start = c(1,7, 9,20,23,27),
                                                end   = c(4,8,18,22,23,27),
                                                col_names = c("year","region","perwt","age","race","hcovany")),
                  col_types = cols(.default = "i"), 
                                  progress = TRUE)

#subset to women of childbearing age, white and black and region identified
ipums <- subset(ipums, age>=18 & age<45 & race<3 & region<91 & year>2007)

ipums$race <- factor(ipums$race,
                     levels=1:2,
                     labels=c("White","Black"))

ipums$health_insurance <- ipums$hcovany==2


ipums$region_big <- ifelse(ipums$region<21, "Census Region 1: Northeast",
                           ifelse(ipums$region<31, "Census Region 2: Midwest",
                                  ifelse(ipums$region<41, "Census Region 3: South", "Census Region 4: West")))
table(ipums$region, ipums$region_big, exclude=NULL)
ipums$region <- ipums$region_big

ipums$health_insurance_weight <- (ipums$health_insurance * ipums$perwt)

temp1 <- aggregate(health_insurance_weight~region+year+race, data=ipums, sum)
temp2 <- aggregate(perwt~region+year+race, data=ipums, sum)

insurance <- merge(temp1, temp2, by=c("region","year","race"))
insurance$health_coverage <- 100*insurance$health_insurance_weight/insurance$perwt

insurance <- merge(subset(insurance, race=="Black", select=c("region","year","health_coverage")), 
                   subset(insurance, race=="White", select=c("region","year","health_coverage")),
                                      by=c("region","year"))
colnames(insurance) <- c("region","year","health_coverage_black","health_coverage_white")



insurance$diff_coverage <- insurance$health_coverage_white-insurance$health_coverage_black


mat_mort <- merge(mat_mort, insurance, by=c("region","year"))
save(mat_mort, file="output/analytical_data.RData")
