################################################################################
#############          NES - LTER Cruise           #############################
#############            ELOG BONGO                #############################
## by: Alexandra Cabanelas 
## created: JUN-2024
################################################################################
# getting bongo and ring net information from R2R elog  
# cleaning up data and merging all elog data together
# using Rest API

## new API 
#https://github.com/WHOIGit/nes-lter-api-2/wiki

## old API
#https://www.rvdata.us/
#https://github.com/WHOIGit/nes-lter-ims/wiki/Using-REST-API-to-access-NES-LTER-data

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

file_urls <- c(
  'https://nes-lter-data.whoi.edu/api/events/en608.csv',
  'https://nes-lter-data.whoi.edu/api/events/en617.csv',
  'https://nes-lter-data.whoi.edu/api/events/en627.csv',
  'https://nes-lter-data.whoi.edu/api/events/en644.csv',
  'https://nes-lter-data.whoi.edu/api/events/en649.csv',
  'https://nes-lter-data.whoi.edu/api/events/en655.csv',
  'https://nes-lter-data.whoi.edu/api/events/en657.csv',
  'https://nes-lter-data.whoi.edu/api/events/en661.csv',
  'https://nes-lter-data.whoi.edu/api/events/en668.csv',
  'https://nes-lter-data.whoi.edu/api/events/at46.csv',
  'https://nes-lter-data.whoi.edu/api/events/en687.csv',
  'https://nes-lter-data.whoi.edu/api/events/en695.csv',
  'https://nes-lter-data.whoi.edu/api/events/hrs2303.csv',
  'https://nes-lter-data.whoi.edu/api/events/en706.csv',
  'https://nes-lter-data.whoi.edu/api/events/en712.csv',
  'https://nes-lter-data.whoi.edu/api/events/en715.csv',
  #'https://nes-lter-data.whoi.edu/api/events/en720.csv', #not available yet 
  'https://nes-lter-data.whoi.edu/api/events/ar77.csv',
  'https://nes-lter-data.whoi.edu/api/events/ar32.csv',
  'https://nes-lter-data.whoi.edu/api/events/ar38.csv',
  'https://nes-lter-data.whoi.edu/api/events/ar61b.csv', #ring net only
  #'https://nes-lter-data.whoi.edu/api/events/ar66b.csv', #ring net only; not available
  'https://nes-lter-data.whoi.edu/api/events/ar28b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/events/ar31a.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/events/ar34b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/events/ar39b.csv' #ring net only
  #'https://nes-lter-data.whoi.edu/api/events/ar63.csv' #not available
)

# OOI? no bongo, no ring :
# AR34A, AR22, AR24a, AR24c, AR28A, AR31C, AR39A, AR44, AR48A, AR48B

read_and_add_cruise <- function(url) {
  cruise_name <- toupper(gsub(".csv", "", basename(url)))
  data <- read.csv(url)
  
  #not all cruises have equal columns...some have 9, others 15, most have 10 cols
  # desired columns
  desired_columns <- c("Message.ID", "dateTime8601", "Instrument", 
                       "Action", "Station", "Cast", "Latitude", 
                       "Longitude", "Comment")
  
  # Add missing columns with NA values
  missing_columns <- setdiff(desired_columns, colnames(data))
  data[, missing_columns] <- NA
  
  # Remove extra columns
  extra_columns <- setdiff(colnames(data), desired_columns)
  data <- data[, !colnames(data) %in% extra_columns]
  
  # Add Cruise column
  data$cruise <- cruise_name
  return(data)
}

cruiseDat <- lapply(file_urls, read_and_add_cruise)

listviewer::jsonedit(cruiseDat)
#purrr::map(cruiseDat, 2) 

combined_data <- do.call(rbind, cruiseDat)

AR63 <- read.csv("R2R_ELOG_AR63.csv", header = T) %>%
  mutate(cruise = toupper(Cruise)) %>% #change col name and make cruise name lowercase
  select(Message.ID, dateTime8601, Instrument, Action, Station, Cast,
         Latitude, Longitude, Comment, cruise)

AR66B <- read.csv("R2R_ELOG_AR66B.csv", header = T) %>%
  mutate(cruise = toupper(Cruise)) %>% #change col name and make cruise name lowercase
  select(Message.ID, dateTime8601, Instrument, Action, Station, Cast,
         Latitude, Longitude, Comment, cruise)

EN720 <- read.csv("R2R_ELOG_en720.csv", header = T) %>%
  mutate(cruise = toupper(Cruise)) %>% #change col name and make cruise name lowercase
  select(Message.ID, dateTime8601, Instrument, Action, Station, Cast,
         Latitude, Longitude, Comment, cruise)

combined_data <- rbind(combined_data, AR63, AR66B, EN720)

## ------------------------------------------ ##
#            Data Wrangling -----
## ------------------------------------------ ##

## ---- There are inconsistencies in Instrument names
unique(combined_data$Instrument)

zoop_tows <- combined_data %>%
  filter(Instrument == "Bongo Net" | Instrument == "Bongo" | 
           Instrument == "Ring Net" | Instrument == "RingNet")
unique(zoop_tows$Instrument)

# Function to standardize Instrument names
standardize_instrument <- function(instrument) {
  # convert the instrument name to lowercase to ensure case-insensitive matching
  instrument <- tolower(instrument) 
  instrument <- gsub("\\s+", " ", instrument)  # Remove extra spaces
  # if the instrument contains "bongo" or "ring", 
  # change to "Bongo Net" or "Ring Net" respectively.
  if (grepl("bongo", instrument)) {  
    instrument <- "Bongo Net"
  } else if (grepl("ring", instrument)) {
    instrument <- "Ring Net"
  }
  return(instrument)
}

# Apply the function to the Instrument column
zoop_tows$Instrument <- sapply(zoop_tows$Instrument, standardize_instrument)
unique(zoop_tows$Instrument)

## ---- There are inconsistencies in Station names
unique(zoop_tows$Station)
# Remove the "LTER" in station name and replace with "L"
zoop_tows$Station <- gsub("^LTER(\\d+)$", "L\\1", zoop_tows$Station)
# Convert "MVCO" to uppercase
zoop_tows$Station <- gsub("MvCO", "MVCO", zoop_tows$Station)
# Add "L" to the station names that are missing it (except "u11c" and "MVCO")
zoop_tows$Station[!grepl("^L", zoop_tows$Station) & zoop_tows$Station != "u11c" & zoop_tows$Station != "MVCO" & zoop_tows$Station != ""] <- paste0("L", zoop_tows$Station[!grepl("^L", zoop_tows$Station) & zoop_tows$Station != "u11c" & zoop_tows$Station != "MVCO" & zoop_tows$Station != ""])
# Remove leading zeros after "L" in the station names (except for "MVCO" and "u11c")
zoop_tows$Station[grepl("^L\\d{2}$", zoop_tows$Station) & zoop_tows$Station != "MVCO" & zoop_tows$Station != "u11c"] <- gsub("^L0+(\\d+)$", "L\\1", zoop_tows$Station[grepl("^L\\d{2}$", zoop_tows$Station) & zoop_tows$Station != "MVCO" & zoop_tows$Station != "u11c"])

unique(zoop_tows$Station)

## ---- There are inconsistencies in Cast names
unique(zoop_tows$Cast)
# Add "R" or "B" to the cast names that are missing it based on the "Instrument" column
zoop_tows$Cast <- ifelse(zoop_tows$Cast != "" & grepl("Bongo Net", zoop_tows$Instrument) & substr(zoop_tows$Cast, 1, 1) != "B", paste0("B", zoop_tows$Cast), zoop_tows$Cast)
zoop_tows$Cast <- ifelse(zoop_tows$Cast != "" & grepl("Ring Net", zoop_tows$Instrument) & substr(zoop_tows$Cast, 1, 1) != "R", paste0("R", zoop_tows$Cast), zoop_tows$Cast)
# Remove leading zeros after "B" or "R" in the cast names (ignore missing vals)
zoop_tows$Cast <- ifelse(zoop_tows$Cast != "", gsub("^(B|R)0*([1-9]\\d*)[a-zA-Z]*$", "\\1\\2", zoop_tows$Cast), zoop_tows$Cast)
# Correct the typo "BL16" to "B16"
zoop_tows$Cast <- gsub("^BL", "B", zoop_tows$Cast)
unique(zoop_tows$Cast)

## ---- There are missing/empty cells -> change all these to NAs
# Replace empty cells with NA
zoop_tows[zoop_tows == ""] <- NA

## ---- Add some date columns 

#some entries have a "T" between date and time
zoop_tows$dateTime8601 <- gsub("T", " ", zoop_tows$dateTime8601) 

zoop_tows$date <- format(as.POSIXct(zoop_tows$dateTime8601, 
                                    format = "%Y-%m-%d %H:%M:%S+00:00", 
                                    tz="UTC"), "%Y-%m-%d")
zoop_tows$time <- format(as.POSIXct(zoop_tows$dateTime8601,
                                    format = "%Y-%m-%d %H:%M:%S+00:00", 
                                    tz="UTC"), "%H:%M:%S")

zoop_tows <- tidyr::separate(zoop_tows, date, c('Year', 'Month', 'Day'), 
                             sep = '[-]', remove = FALSE)

#write.csv(zoop_tows, "output/all_eventLogs_26SEP2024.csv")
