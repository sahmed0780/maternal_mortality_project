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
