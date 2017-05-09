library(dplyr)
library(datapkg)
library(gtools)

##################################################################
#
# Processing Script for Commuting-and-Residence
# Created by Jenna Daly
# On 05/02/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
all_csvs <- dir(path_to_raw_data, recursive=T, pattern = "od") 

#only read in 2014 data
com_res_data <- read.csv(paste0(path_to_raw_data, "/", all_csvs[2]), stringsAsFactors=F, header=T )
com_res_data <-format(com_res_data, scientific = FALSE) 
com_res_data <- com_res_data[,c(1:3)]
colnames(com_res_data) <- c("Workplace FIPS", "Residence FIPS", "Value")
get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(all_csvs[2])), "")))
com_res_data$Year <- get_year  


xwalk <- read.csv(paste0(path_to_raw_data, "/", "ct_xwalk.csv"), stringsAsFactors=F, header=T )
xwalk$tabblk2010 <-format(xwalk$tabblk2010, scientific = FALSE) 
xwalk_large_fips <- xwalk[,c(1,18)]
xwalk_small_fips <- xwalk[,c(17,18)]

#remove duplicates from small fips
xwalk_small_fips <- xwalk_small_fips[!duplicated(xwalk_small_fips), ]
#remove CT
xwalk_small_fips <- xwalk_small_fips[xwalk_small_fips$ctycsub!=9999999999, ]

#merge in FIPS for both residence and Workplace
com_res_towns <- merge(xwalk_large_fips, com_res_data, by.x = "tabblk2010", by.y = "Workplace FIPS")
names(com_res_towns)[names(com_res_towns)=="tabblk2010"] <- "Workplace FIPS"
names(com_res_towns)[names(com_res_towns)=="ctycsubname"] <- "Workplace Town"

com_res_towns <- merge(xwalk_large_fips, com_res_towns, by.x = "tabblk2010", by.y = "Residence FIPS")
names(com_res_towns)[names(com_res_towns)=="tabblk2010"] <- "Residence FIPS"
names(com_res_towns)[names(com_res_towns)=="ctycsubname"] <- "Residence Town"

#add up commuters for each town
com_res_towns$"Value" <- as.numeric(com_res_towns$"Value")
com_res_total <- aggregate(`Value` ~ `Residence Town` + `Workplace Town` + `Year`, com_res_towns, sum)

#remove extra rows that had county fips
com_res_total <- com_res_total[com_res_total$`Residence Town` != "",]

#check to make sure work and residence columns have all towns, and all the same towns
u1 <- unique(com_res_total$`Residence Town`)
u2 <- unique(com_res_total$`Workplace Town`)
u1_sort <- mixedsort(u1)
u2_sort <- mixedsort(u2)
identical(u1_sort, u2_sort)

#create a backfill (so every town has corresponding work and residence, will create some town pairs with 0 commuters)
work_towns <- u1_sort
residence_towns <- u1_sort

backfill_towns <- expand.grid(
  `Workplace Town` = work_towns,
  `Residence Town` = residence_towns 
)

complete_com_res_total <- merge(com_res_total, backfill_towns, by = c("Workplace Town", "Residence Town"), all=T)

#Add back in small FIPS
com_res_total_with_fips <- merge(complete_com_res_total, xwalk_small_fips, by.x = "Workplace Town", by.y = "ctycsubname")
names(com_res_total_with_fips)[names(com_res_total_with_fips)=="ctycsub"] <- "Workplace FIPS"

com_res_total_with_fips <- merge(com_res_total_with_fips, xwalk_small_fips, by.x = "Residence Town", by.y = "ctycsubname")
names(com_res_total_with_fips)[names(com_res_total_with_fips)=="ctycsub"] <- "Residence FIPS"

#set Year
com_res_total_with_fips$Year <- "2014"

#remove county name from town columns
com_res_total_with_fips$`Workplace Town` <- sub(" town.*", "", com_res_total_with_fips$`Workplace Town`)
com_res_total_with_fips$`Residence Town` <- sub(" town.*", "", com_res_total_with_fips$`Residence Town`)

#set NA's to zero (designates no commuters)
com_res_total_with_fips[is.na(com_res_total_with_fips)] <- 0

#set Measure Type
com_res_total_with_fips$`Measure Type` <- "Number"

#set Variable
com_res_total_with_fips$`Variable` <- "Commuters"

#Arrange and order columns
com_res_total_with_fips <- arrange(com_res_total_with_fips, `Residence Town`, desc(Value)) %>% 
  select(`Residence Town`, `Residence FIPS`, `Workplace Town`, `Workplace FIPS`, `Year`, `Variable`, `Measure Type`, `Value`)

#Write CSV
write.table(
  com_res_total_with_fips,
  file.path(getwd(), "data", "commuting_and_residence_2014.csv"),
  sep = ",",
  row.names = F
)
