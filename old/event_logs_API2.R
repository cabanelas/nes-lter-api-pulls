################################################################################
#############          NES - LTER Cruises          #############################
#############            ELOG BONGO                #############################
## by: Alexandra Cabanelas 
## created: JUN-2024; updated: NOV-2025
################################################################################
# getting bongo and ring net information from R2R elog  
# cleaning up data and merging all elog data together
# using Rest API

## new API 2
#https://github.com/WHOIGit/nes-lter-api-2/wiki
#https://nes-lter-api.whoi.edu/api/docs#/
#https://nes-lter-api.whoi.edu/cruise/ar77/track/

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
base_url <- "https://nes-lter-api.whoi.edu/api/"

## --- Cruise list -----
cruise_list <- read_csv(paste0(base_url, "ctd/cruises/get/all"))
cruise_ids <- toupper(cruise_list$name)

## --- event log -----
# Reader function
read_event_cruise <- function(cruise_id) {
  url <- paste0(base_url, "events/get/", cruise_id)
  message("Reading: ", cruise_id)
  
  tryCatch({
    data <- read_csv(url, show_col_types = FALSE)
    
    desired_columns <- c("Message ID", "dateTime8601", "Instrument", 
                         "Action", "Station", "Cast", "Latitude", 
                         "Longitude", "Comment")
    
    # Add missing columns
    missing <- setdiff(desired_columns, names(data))
    data[missing] <- NA
    
    # Keep only desired columns
    data <- data[intersect(names(data), desired_columns)]
    
    # Harmonize column types
    data <- mutate(data,
                   Cast = as.character(Cast),
                   Station = as.character(Station),
                   Latitude = as.numeric(Latitude),
                   Longitude = as.numeric(Longitude)
    )
    
    # Add cruise ID
    data$cruise <- cruise_id
    return(data)
  }, error = function(e) {
    warning("Failed to read: ", cruise_id)
    return(NULL)
  })
}

# Read and combine all cruises
cruiseDat <- lapply(cruise_ids, read_event_cruise)
combined_data <- bind_rows(Filter(Negate(is.null), cruiseDat))
print(cruise_ids[which(sapply(cruiseDat, is.null))]) #AR16-AR95

glimpse(combined_data)
#listviewer::jsonedit(combined_data);purrr::map(combined_data, 2)# optional interactive view

# AR66B (ring net only) not available in restapi
AR66B <- read_csv("raw/R2R_ELOG_AR66B.csv") %>%
  mutate(cruise = toupper(Cruise)) %>% #change col name and make cruise name lowercase
  select("Message ID", dateTime8601, Instrument, Action, Station, Cast,
         Latitude, Longitude, Comment, cruise)

combined_data <- rbind(combined_data, AR66B) 
rm(AR66B)

# keep bongo/ring entries only
# no bongo, no ring : AR22,AR24A,AR24B,AR24C,AR28A,AR34A,AR31B,AR31C,AR39A,AR44,
#AR48A,AR48B,AR61A,AR70B,AR75,AR78,AR82A,AR87A
zoop_tows <- combined_data %>%
  filter(Instrument %in% c("Bongo Net", "Bongo", "Ring Net", "RingNet"),
         cruise != "AR87B")

unique(zoop_tows$cruise) #31 cruises total
#AE2426,AR32,AR38,AR63,AR77,AR88,AT46,EN608,EN617,EN627,EN44,EN649,EN655,
#EN657,EN661,EN668,EN687,EN695,EN706,EN712,EN715,EN720,EN727,HRS2303

#ring net only:AR28B,AR31A,AR34B,AR39B,AR61B, 
#EN685 (this was URI cruise summer 2022; 35 ring net casts) 

## ------------------------------------------ ##
#            Tidy -----
## ------------------------------------------ ##
#zoop_tows <- zoop_tows %>% 
#  rename(Message_ID = `Message ID`)

unique(zoop_tows$Instrument)

## --- Tidy Station names -----
unique(zoop_tows$Station)

zoop_tows <- zoop_tows %>%
  mutate(Station = Station %>%
           # Replace "LTER" in station name with "L"
           gsub("^LTER(\\d+)$", "L\\1", .) %>% # Replace "LTER1" with "L1"
           # Ensure MVCO is uppercase
           gsub("MvCO", "MVCO", .) %>%  
           # Add "L" to the station names (except "u11c" and "MVCO")
           { ifelse(!grepl("^L", .) & . != "u11c" & . != "MVCO" & . != "", 
                    paste0("L", .), .) } %>%  # Add "L" if missing
           { ifelse(grepl("^L\\d{2}$", .) & . != "MVCO" & . != "u11c", 
                    gsub("^L0+(\\d+)$", "L\\1", .), .) }  # Remove leading zeros
  )

unique(zoop_tows$Station)

## --- Tidy Cast names -----
unique(zoop_tows$Cast)

zoop_tows <- zoop_tows %>%
  mutate(Cast = Cast %>%
           # Add "B" for Bongo Nets
           { ifelse(. != "" & grepl("Bongo Net", 
                                    Instrument) & substr(., 1, 1) != "B", 
                    paste0("B", .), .) } %>%
           
           # Add "R" for Ring Nets 
           { ifelse(. != "" & grepl("Ring Net", 
                                    Instrument) & substr(., 1, 1) != "R", 
                    paste0("R", .), .) } %>%
           
           # Remove leading zeros after B or R (B01 -> B1)
           { ifelse(. != "", gsub("^(B|R)0*([1-9]\\d*)[a-zA-Z]*$", 
                                  "\\1\\2", .), .) } %>%
           
           # Fix typo BL16 -> B16
           gsub("^BL", "B", .) %>%
           gsub("BTest", "Test", .)
  )

unique(zoop_tows$Cast)

## --- Add some date columns -----

zoop_tows$date <- format(as.POSIXct(zoop_tows$dateTime8601, 
                                    format = "%Y-%m-%d %H:%M:%S+00:00", 
                                    tz="UTC"), "%Y-%m-%d")
zoop_tows$time <- format(as.POSIXct(zoop_tows$dateTime8601,
                                    format = "%Y-%m-%d %H:%M:%S+00:00", 
                                    tz="UTC"), "%H:%M:%S")

zoop_tows <- tidyr::separate(zoop_tows, date, c('year', 'month', 'day'), 
                             sep = '[-]', remove = FALSE)
zoop_tows <- janitor::clean_names(zoop_tows)

#write.csv(zoop_tows, "output/all_eventLogs_12NOV2025.csv")