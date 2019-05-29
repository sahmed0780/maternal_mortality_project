#' ---
#' title: "organize_data.R"
#' author: ""
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory. 

#source in any useful functions
source("useful_functions.R")
library(readr)


# calculate MMR by region and year ----------------------------------------

mat_death <- read_tsv("input/mcd_wonder.txt",
                      n_max=204)

mat_death <- aggregate(Deaths~`Census Region Code`+Race+Year, 
                       sum, data=mat_death)
mat_death <- subset(mat_death, (Race=="White" | Race=="Black or African American") & Year>=2007)

births <- read_tsv("input/natality_wonder.txt",
                   n_max=197)

births <- aggregate(Births~`Census Region Code`+`Mother's Bridged Race`+Year, sum, data=births)
colnames(births)[2] <- "Race"
births <- subset(births, Race=="White" | Race=="Black or African American")

mat_mort <- merge(mat_death, births)
mat_mort$mmr <- mat_mort$Deaths/mat_mort$Births
colnames(mat_mort) <- c("region","race","year","deaths","births","mmr")
temp1 <- subset(mat_mort, race=="Black or African American",
                select=c("region","year","mmr"))
colnames(temp1)[3] <- "mmr_black"
temp2 <- subset(mat_mort, race=="White",
                select=c("region","year","mmr"))
colnames(temp2)[3] <- "mmr_white"
mat_mort <- merge(temp1, temp2, by=c("region","year"))
mat_mort$mmr_race <- mat_mort$mmr_black/mat_mort$mmr_white

#regions
#1 - NE
#2 - MW
#3 - South
#4 - West

mat_mort$region <- ifelse(mat_mort$region=="CENS-R1","Northeast",
                          ifelse(mat_mort$region=="CENS-R2","Midwest",
                                 ifelse(mat_mort$region=="CENS-R3","South","West")))
mat_mort <- subset(mat_mort, year>2007,
                   select=c("region","year","mmr_race"))

# Read in IPUMS Data ------------------------------------------------------

ipums <- read_fwf("input/usa_00006.dat.gz",
                  col_positions = fwf_positions(start = c(1,7, 9,20,23,27),
                                                end   = c(4,8,18,22,23,27),
                                                col_names = c("year","region","perwt","age","race","hcovany")),
                  col_types = cols(.default = "i"), #ensure that all variables are read in as integers
                  progress = TRUE)

#subset to women of childbearing age, white and black and region identified
ipums <- subset(ipums, age>=18 & age<45 & race<3 & region<91 & year>2007)

ipums$race <- factor(ipums$race,
                     levels=1:2,
                     labels=c("White","Black"))

ipums$health_insurance <- ipums$hcovany==2

ipums$region_big <- ifelse(ipums$region<21, "Northeast",
                           ifelse(ipums$region<31, "Midwest",
                                  ifelse(ipums$region<41, "South", "West")))
table(ipums$region, ipums$region_big, exclude=NULL)

ipums$health_insurance_weight <- (ipums$health_insurance * ipums$perwt)

temp <- aggregate(health_insurance_weight~region_big+year, data=ipums, sum)
temp2 <- aggregate(perwt~region_big+year, data=ipums, sum)

temp$health_coverage <- 100*temp$health_insurance_weight/temp2$perwt

ipums_agg <- temp[,-3]
colnames(ipums_agg) <- c("region","year","health_coverage")

mat_mort <- merge(mat_mort, ipums_agg, by=c("region","year"))
