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

#find latest year (update this value when data become avail)
latest_year <- "2014"
latest_csvs <- grep(latest_year, all_csvs, value=T)

xwalk_files <- dir(path_to_raw_data, recursive=T, pattern = "xwalk.csv$") 

all_states_xwalk <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(xwalk_files)) {
  xwalk_file <- read.csv(paste0(path_to_raw_data, "/", xwalk_files[i]), stringsAsFactors=F, header=T )
  all_states_xwalk <- rbind(all_states_xwalk, xwalk_file)
}

all_states_xwalk$tabblk2010 <- format(all_states_xwalk$tabblk2010, scientific = FALSE) 

xwalk_large_fips <- all_states_xwalk[,c(1,18)]
xwalk_large_fips <- xwalk_large_fips[xwalk_large_fips$ctycsubname != "",]


#Separate out into 3 sections, and merge by FIPS separately
##1) in-state CT only (CT-main)
################################################################################################################################
in_state_files <- grep("main", latest_csvs, value=T) #live and work in this state

CT_file <- read.csv(paste0(path_to_raw_data, "/", in_state_files[1]), stringsAsFactors=F, header=T )
CT_file <-format(CT_file, scientific = FALSE)
CT_file <- CT_file[,c(1:3)]
colnames(CT_file) <- c("Workplace FIPS", "Residence FIPS", "Value")
get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(in_state_files[1])), "")))
CT_file$Year <- get_year

#Bring in ct-xwalk
CT_xwalk <- read.csv(paste0(path_to_raw_data, "/", xwalk_files[1]), stringsAsFactors=F, header=T )
CT_xwalk$tabblk2010 <- format(CT_xwalk$tabblk2010, scientific = FALSE) 

#Separate out FIPS codes
xwalk_large_fips_CT <- CT_xwalk[,c(1,18)]

#Merge in Workplace, then Residence by large FIPS
CT_fips <- merge(CT_file, xwalk_large_fips_CT, by.x = "Workplace FIPS", by.y = "tabblk2010", all.x=T)
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
out_state_files <- grep("aux", latest_csvs, value=T) #live in this state, work in another state

CT_in <- read.csv(paste0(path_to_raw_data, "/", out_state_files[1]), stringsAsFactors=F, header=T )
CT_in <-format(CT_in, scientific = FALSE)
CT_in <- CT_in[,c(1:3)]
colnames(CT_in) <- c("Workplace FIPS", "Residence FIPS", "Value")
get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(out_state_files[1])), "")))
CT_in$Year <- get_year

CT_in <- CT_in[substr(CT_in$`Residence FIPS`, 1, 2) == "25" |
                 substr(CT_in$`Residence FIPS`, 1, 2) == "36" |
                 substr(CT_in$`Residence FIPS`, 1, 2) == "44",]

#merge in FIPS for both residence and Workplace
CT_in_fips <- merge(xwalk_large_fips_CT, CT_in, by.x = "tabblk2010", by.y = "Workplace FIPS")
names(CT_in_fips)[names(CT_in_fips)=="tabblk2010"] <- "Workplace FIPS"
names(CT_in_fips)[names(CT_in_fips)=="ctycsubname"] <- "Workplace"

CT_in_fips2 <- merge(xwalk_large_fips, CT_in_fips, by.x = "tabblk2010", by.y = "Residence FIPS")
names(CT_in_fips2)[names(CT_in_fips2)=="tabblk2010"] <- "Residence FIPS"
names(CT_in_fips2)[names(CT_in_fips2)=="ctycsubname"] <- "Residence"


#remove extra rows that had county fips
CT_in_fips2$`Residence State` <- NA
CT_in_fips2$`Residence State`[which(substr(CT_in_fips2$`Residence FIPS`, 1, 2) == "25")] <- "MA"
CT_in_fips2$`Residence State`[which(substr(CT_in_fips2$`Residence FIPS`, 1, 2) == "36")] <- "NY"
CT_in_fips2$`Residence State`[which(substr(CT_in_fips2$`Residence FIPS`, 1, 2) == "44")] <- "RI"
CT_in_fips2$`Workplace State` <- "CT"

#add up commuters for each town
CT_in_fips2$"Value" <- as.numeric(CT_in_fips2$"Value")
CT_in_fips_total <- aggregate(`Value` ~ `Residence` + `Workplace` + `Year` + `Residence State` + `Workplace State`, CT_in_fips2, sum)

##3) from CT (NY, RI, MA -aux)
#################################################################################################################
for (i in 1:length(out_state_files)) {
  out_state_file <- read.csv(paste0(path_to_raw_data, "/", out_state_files[i]), stringsAsFactors=F, header=T )
  out_state_file <-format(out_state_file, scientific = FALSE)
  out_state_file <- out_state_file[,c(1:3)]
  colnames(out_state_file) <- c("Workplace FIPS", "Residence FIPS", "Value")
  out_state_file$`Workplace FIPS` <- as.character(out_state_file$`Workplace FIPS`)
  out_state_file$`Residence FIPS` <- as.character(out_state_file$`Residence FIPS`)
  get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(out_state_files[i])), "")))
  out_state_file$Year <- get_year
  get_state <- toupper(substr(out_state_files[i], 1, 2))
  out_state_file$`Workplace State` <- get_state
  #all_out_states <- rbind(all_out_states, out_state_file)
  assign(paste0(get_state, "_", get_year), out_state_file)
}

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
all_out_states <- grep("2014$", dfs, value=T)

neighbors <- c("NY", "MA", "RI")
all_non_CT_states <- unique(grep(paste(neighbors, collapse="|"), all_out_states, value=TRUE))

CT_out <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(all_non_CT_states)) {
  current_out_state <- get(all_non_CT_states[i])
  #filter out all non-CT residences
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


#add up commuters for each town
CT_out_fips2$"Value" <- as.numeric(CT_out_fips2$"Value")
CT_out_fips_total <- aggregate(`Value` ~ `Residence` + `Workplace` + `Year` + `Residence State` + `Workplace State`, CT_out_fips2, sum)

rm(CT_2014, MA_2014, NY_2014, RI_2014, CT_file, CT_fips, CT_in, CT_in_fips, CT_in_fips2, 
   CT_out, CT_out_fips, CT_out_fips2, current_out_state, out_state_file, xwalk_file)

#Merge back in small FIPS
#######################################################################################################################################
xwalk_small_fips <- all_states_xwalk[,c(17,18)]
xwalk_small_fips_CT <- CT_xwalk[,c(17,18)]

#remove duplicates from small fips
xwalk_small_fips <- xwalk_small_fips[!duplicated(xwalk_small_fips), ]
xwalk_small_fips_CT <- xwalk_small_fips_CT[!duplicated(xwalk_small_fips_CT), ]
#remove CT
xwalk_small_fips <- xwalk_small_fips[xwalk_small_fips$ctycsub!=9999999999, ]
xwalk_small_fips_CT <- xwalk_small_fips_CT[xwalk_small_fips_CT$ctycsub!=9999999999, ]

#remove duplicates from small fips
xwalk_small_fips <- xwalk_small_fips[!duplicated(xwalk_small_fips), ]
#remove CT
xwalk_small_fips <- xwalk_small_fips[xwalk_small_fips$ctycsub!=9999999999, ]


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

#Arrange and order columns
complete_commuter_data <- arrange(complete_commuter_data, `Residence`, desc(Value)) %>% 
  select(`Residence`, `Residence State`, `Residence FIPS`, `Workplace`, `Workplace State`, `Workplace FIPS`, `Year`, `Variable`, `Measure Type`, `Value`)

#Write CSV
write.table(
  complete_commuter_data,
  file.path(getwd(), "data", "commuting_and_residence_2014.csv"),
  sep = ",",
  row.names = F
)
