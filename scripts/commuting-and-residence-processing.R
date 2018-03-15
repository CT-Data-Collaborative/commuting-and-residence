library(dplyr)
library(datapkg)
library(gtools)

##################################################################
#
# Processing Script for Commuting-and-Residence
# Created by Jenna Daly
# On 05/02/2017
# Update 07/14/2017 Jenna Daly
#  - Capture out of state commuting with neighbor state aux files
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
all_csvs <- dir(path_to_raw_data, recursive=T, pattern = "od") 
options(scipen=9999)

xwalk_files <- dir(path_to_raw_data, recursive=T, pattern = "xwalk.csv$") 

all_states_xwalk <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(xwalk_files)) {
  xwalk_file <- read.csv(paste0(path_to_raw_data, "/", xwalk_files[i]), stringsAsFactors=F, header=T )
  all_states_xwalk <- rbind(all_states_xwalk, xwalk_file)
}
rm(xwalk_file)

xwalk_large_fips <- all_states_xwalk[,c(1,18)]

xwalk_large_fips <- xwalk_large_fips[xwalk_large_fips$ctycsubname != "",]

#Separate out into 3 sections, and merge by FIPS separately
##1) in-state CT only (CT-main) (live in CT, work in CT)
################################################################################################################################
in_state_files <- grep("main", all_csvs, value=T) #live and work in this state

complete_CT_file <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(in_state_files)) {
  CT_file <- read.csv(paste0(path_to_raw_data, "/", in_state_files[i]), stringsAsFactors=F, header=T )
  CT_file <- CT_file[,c(1:3)]
  colnames(CT_file) <- c("Workplace FIPS", "Residence FIPS", "Value")
  get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(in_state_files[i])), "")))
  CT_file$Year <- get_year 
  complete_CT_file <- rbind(complete_CT_file, CT_file)
}
rm(CT_file)

#Bring in ct-xwalk
CT_xwalk <- read.csv(paste0(path_to_raw_data, "/", xwalk_files[1]), stringsAsFactors=F, header=T )

#Separate out FIPS codes
xwalk_large_fips_CT <- CT_xwalk[,c(1,18)]

#Merge in Workplace, then Residence by large FIPS
CT_fips <- merge(complete_CT_file, xwalk_large_fips_CT, by.x = "Workplace FIPS", by.y = "tabblk2010", all.x=T)
names(CT_fips)[names(CT_fips)=="ctycsubname"] <- "Workplace"

CT_fips <- merge(CT_fips, xwalk_large_fips_CT, by.x = "Residence FIPS", by.y = "tabblk2010", all.x=T)
names(CT_fips)[names(CT_fips)=="ctycsubname"] <- "Residence"

#add up commuters for each town
CT_fips$"Value" <- as.numeric(CT_fips$"Value")
CT_fips_total <- aggregate(`Value` ~ `Residence` + `Workplace` + `Year`, CT_fips, sum)

#remove extra rows that had county fips
CT_fips_total <- CT_fips_total[CT_fips_total$`Residence` != "",]
CT_fips_total$`Residence State` <- "CT"
CT_fips_total$`Workplace State` <- "CT"

#res <- unique(CT_fips_total$Residence)
#work <- unique(CT_fips_total$Workplace)

##2) to CT (CT-aux)
########################################################################################################
out_state_files <- grep("aux", all_csvs, value=T) #live in this state, work in another state
ct_in_files <- grep("ct", out_state_files, value=T) #live in CT, work in another state

complete_CT_in <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(ct_in_files)) {
  CT_in <- read.csv(paste0(path_to_raw_data, "/", ct_in_files[i]), stringsAsFactors=F, header=T )
  CT_in <- CT_in[,c(1:3)]
  colnames(CT_in) <- c("Workplace FIPS", "Residence FIPS", "Value")
  get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(ct_in_files[i])), "")))
  CT_in$Year <- get_year
  complete_CT_in <- rbind(complete_CT_in, CT_in)
}

#Only grab residences from NY, RI, and MA
complete_CT_in <- complete_CT_in[substr(complete_CT_in$`Residence FIPS`, 1, 2) %in% c(25, 36, 44),]

#merge in FIPS for both residence and Workplace
complete_CT_in_fips <- merge(xwalk_large_fips_CT, complete_CT_in, by.x = "tabblk2010", by.y = "Workplace FIPS")
names(complete_CT_in_fips)[names(complete_CT_in_fips)=="tabblk2010"] <- "Workplace FIPS"
names(complete_CT_in_fips)[names(complete_CT_in_fips)=="ctycsubname"] <- "Workplace"

complete_CT_in_fips2 <- merge(xwalk_large_fips, complete_CT_in_fips, by.x = "tabblk2010", by.y = "Residence FIPS")
names(complete_CT_in_fips2)[names(complete_CT_in_fips2)=="tabblk2010"] <- "Residence FIPS"
names(complete_CT_in_fips2)[names(complete_CT_in_fips2)=="ctycsubname"] <- "Residence"


#remove extra rows that had county fips
complete_CT_in_fips2$`Residence State` <- NA
complete_CT_in_fips2$`Residence State`[which(substr(complete_CT_in_fips2$`Residence FIPS`, 1, 2) == "25")] <- "MA"
complete_CT_in_fips2$`Residence State`[which(substr(complete_CT_in_fips2$`Residence FIPS`, 1, 2) == "36")] <- "NY"
complete_CT_in_fips2$`Residence State`[which(substr(complete_CT_in_fips2$`Residence FIPS`, 1, 2) == "44")] <- "RI"
complete_CT_in_fips2$`Workplace State` <- "CT"

#add up commuters for each town
complete_CT_in_fips2$"Value" <- as.numeric(complete_CT_in_fips2$"Value")
CT_in_fips_total <- aggregate(`Value`  ~ `Residence` + `Workplace` + `Year` + `Residence State` + `Workplace State`, complete_CT_in_fips2, sum)

##3) from CT (NY, RI, MA -aux)
#################################################################################################################
for (i in 1:length(out_state_files)) {
  out_state_file <- read.csv(paste0(path_to_raw_data, "/", out_state_files[i]), stringsAsFactors=F, header=T )
  out_state_file <- out_state_file[,c(1:3)]
  colnames(out_state_file) <- c("Workplace FIPS", "Residence FIPS", "Value")
  out_state_file$`Workplace FIPS` <- as.character(out_state_file$`Workplace FIPS`)
  out_state_file$`Residence FIPS` <- as.character(out_state_file$`Residence FIPS`)
  get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(out_state_files[i])), "")))
  out_state_file$Year <- get_year
  get_state <- toupper(substr(out_state_files[i], 1, 2))
  out_state_file$`Workplace State` <- get_state
  assign(paste0(get_state, "_", get_year), out_state_file)
}

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
yrs <- c("2014", "2015")
all_out_states <- grep(paste(yrs, collapse="|"), dfs, value=TRUE)

neighbors <- c("NY", "MA", "RI")
all_non_CT_states <- grep(paste(neighbors, collapse="|"), all_out_states, value=TRUE)

CT_out <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(all_non_CT_states)) {
  current_out_state <- get(all_non_CT_states[i])
  #filter out all non-CT residences
  #Confirm long fips has same number of characters (add leading 0 if not)
  current_out_state$`Residence FIPS` <- sprintf("%015s", current_out_state$`Residence FIPS`)
  current_out_state <- current_out_state[substr(current_out_state$`Residence FIPS`, 1, 2) == " 9",]
  current_out_state$`Residence State` <- "CT"
  CT_out <- rbind(CT_out, current_out_state)
}

#merge in FIPS for both residence and Workplace
CT_out_fips <- merge(xwalk_large_fips, CT_out, by.x = "tabblk2010", by.y = "Workplace FIPS")
names(CT_out_fips)[names(CT_out_fips)=="tabblk2010"] <- "Workplace FIPS"
names(CT_out_fips)[names(CT_out_fips)=="ctycsubname"] <- "Workplace"

CT_out_fips$`Residence FIPS` <- trimws(CT_out_fips$`Residence FIPS`)

CT_out_fips2 <- merge(xwalk_large_fips_CT, CT_out_fips, by.x = "tabblk2010", by.y = "Residence FIPS")
names(CT_out_fips2)[names(CT_out_fips2)=="tabblk2010"] <- "Residence FIPS"
names(CT_out_fips2)[names(CT_out_fips2)=="ctycsubname"] <- "Residence"

#remove extra rows that had county fips
CT_out_fips2 <- CT_out_fips2[CT_out_fips2$`Residence` != "",]

#add up commuters for each town
CT_out_fips2$"Value" <- as.numeric(CT_out_fips2$"Value")
CT_out_fips_total <- aggregate(`Value` ~ `Residence` + `Workplace` + `Year` + `Residence State` + `Workplace State`, CT_out_fips2, sum)

#Merge back in small FIPS
#######################################################################################################################################
xwalk_small_fips <- all_states_xwalk[,c(17,18)]
xwalk_small_fips_CT <- CT_xwalk[,c(17,18)]

#remove duplicates from small fips
xwalk_small_fips <- unique(xwalk_small_fips)
xwalk_small_fips_CT <- unique(xwalk_small_fips_CT)

#remove CT
xwalk_small_fips <- xwalk_small_fips[xwalk_small_fips$ctycsub!=9999999999, ]
xwalk_small_fips_CT <- xwalk_small_fips_CT[xwalk_small_fips_CT$ctycsub!=9999999999, ]

#remove duplicates from small fips
# xwalk_small_fips <- xwalk_small_fips[!duplicated(xwalk_small_fips), ]
# #remove CT
# xwalk_small_fips <- xwalk_small_fips[xwalk_small_fips$ctycsub!=9999999999, ]

CT_in_fips_total2 <- merge(CT_in_fips_total, xwalk_small_fips, by.x = "Workplace", by.y = "ctycsubname")
names(CT_in_fips_total2)[names(CT_in_fips_total2)=="ctycsub"] <- "Workplace FIPS"
CT_in_fips_total2 <- merge(CT_in_fips_total2, xwalk_small_fips, by.x = "Residence", by.y = "ctycsubname")
names(CT_in_fips_total2)[names(CT_in_fips_total2)=="ctycsub"] <- "Residence FIPS"

CT_out_fips_total2 <- merge(CT_out_fips_total, xwalk_small_fips, by.x = "Workplace", by.y = "ctycsubname")
names(CT_out_fips_total2)[names(CT_out_fips_total2)=="ctycsub"] <- "Workplace FIPS"
CT_out_fips_total2 <- merge(CT_out_fips_total2, xwalk_small_fips, by.x = "Residence", by.y = "ctycsubname")
names(CT_out_fips_total2)[names(CT_out_fips_total2)=="ctycsub"] <- "Residence FIPS"

CT_fips_total2 <- merge(CT_fips_total, xwalk_small_fips, by.x = "Workplace", by.y = "ctycsubname")
names(CT_fips_total2)[names(CT_fips_total2)=="ctycsub"] <- "Workplace FIPS"
CT_fips_total2 <- merge(CT_fips_total2, xwalk_small_fips, by.x = "Residence", by.y = "ctycsubname")
names(CT_fips_total2)[names(CT_fips_total2)=="ctycsub"] <- "Residence FIPS"

complete_commuter_data <- rbind(CT_in_fips_total2, CT_fips_total2, CT_out_fips_total2)

#cleanup town names
complete_commuter_data$`Workplace` <- sub(" town.*", "", complete_commuter_data$`Workplace`)
complete_commuter_data$`Residence` <- sub(" town.*", "", complete_commuter_data$`Residence`)

complete_commuter_data$`Workplace` <- sub(" city.*", "", complete_commuter_data$`Workplace`)
complete_commuter_data$`Residence` <- sub(" city.*", "", complete_commuter_data$`Residence`)

complete_commuter_data$`Workplace` <- sub(" borough.*", "", complete_commuter_data$`Workplace`)
complete_commuter_data$`Residence` <- sub(" borough.*", "", complete_commuter_data$`Residence`)

complete_commuter_data$`Workplace` <- sub(" Town.*", "", complete_commuter_data$`Workplace`)
complete_commuter_data$`Residence` <- sub(" Town.*", "", complete_commuter_data$`Residence`)

complete_commuter_data$`Workplace` <- sub(" \\(.*", "", complete_commuter_data$`Workplace`)
complete_commuter_data$`Residence` <- sub(" \\(.*", "", complete_commuter_data$`Residence`)

#set Measure Type
complete_commuter_data$`Measure Type` <- "Number"

#set Variable
complete_commuter_data$`Variable` <- "Commuters"

#Select and arrange columns
complete_commuter_data <- complete_commuter_data %>% 
  select(Residence, `Residence State`, `Residence FIPS`, Workplace, `Workplace State`, `Workplace FIPS`, Year, Variable, `Measure Type`, Value) %>% 
  arrange(Residence, desc(Value))  

#Write CSV
write.table(
  complete_commuter_data,
  file.path(getwd(), "data", "commuting_and_residence_2015.csv"),
  sep = ",",
  row.names = F
)
