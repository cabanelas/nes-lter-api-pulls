################################################################################
## Script:  01_elog_pull.R
## Author:  Alexandra C. Cabanelas
##
## Purpose: Pull bongo/ring net event logs from NES-LTER API v2
##          and combine with manually downloaded R2R elogs
##
## API2 (new):
##    https://github.com/WHOIGit/nes-lter-api-2/wiki
##    https://nes-lter-api.whoi.edu/api/docs#/
##    https://nes-lter-api.whoi.edu/cruise/ar77/track/
##
## API1 (old):
##    https://www.rvdata.us/
##    https://github.com/WHOIGit/nes-lter-ims/wiki/Using-REST-API-to-access-NES-LTER-data
##
## Outputs (data/raw/):
##   - elog_zoop_tows_[datecreated].csv
################################################################################

library(tidyverse)
library(janitor)
library(here)
library(httr)

BASE_URL <- "https://nes-lter-api.whoi.edu/api"  

## ------------------------------------------ ##
#   1. Get cruise list
## ------------------------------------------ ##
cruises_df <- read_csv(paste0(BASE_URL, "/ctd/cruises/all.csv"),
                       show_col_types = FALSE)

cruise_ids <- toupper(cruises_df$name)  # col is called 'name' per notebook

## ------------------------------------------ ##
#   2. Pull event logs via API
## ------------------------------------------ ##
# Reader function
read_event_cruise <- function(cruise_id) {
  url <- paste0(BASE_URL, "/events/", tolower(cruise_id), ".csv")
  message("Reading: ", cruise_id)
  
  tryCatch({
    data <- read_csv(url, show_col_types = FALSE)
    
    desired_columns <- c("Message ID", "dateTime8601", "Instrument",
                         "Action", "Station", "Cast",
                         "Latitude", "Longitude", "Comment")
    # Add missing columns
    missing <- setdiff(desired_columns, names(data))
    data[missing] <- NA
    # Keep only desired columns
    data <- data[, intersect(desired_columns, names(data))]
    # Harmonize column types
    data |>
      mutate(
        Cast      = as.character(Cast),
        Station   = as.character(Station),
        Latitude  = as.numeric(Latitude),
        Longitude = as.numeric(Longitude),
        # Add cruise ID
        cruise    = cruise_id
      )
    
  }, error = function(e) {
    warning("Failed: ", cruise_id, " — ", conditionMessage(e))
    return(NULL)
  })
}

# Read and combine all cruises
cruise_dat <- lapply(cruise_ids, read_event_cruise)
failed     <- cruise_ids[sapply(cruise_dat, is.null)]
print(cruise_ids[which(sapply(cruise_dat, is.null))])

if (length(failed) > 0) message("Failed cruises: ", 
                                paste(failed, collapse = ", "))

walk(failed, function(cruise_id) {
  url    <- paste0(BASE_URL, "/events/", tolower(cruise_id), ".csv")
  status <- GET(url)$status_code
  message(cruise_id, ": HTTP ", status)
})
# 404 = cruise exists in the system but no event log

succeeded <- cruise_ids[!sapply(cruise_dat, is.null)]
message("Succeeded (", length(succeeded), "): ", paste(succeeded, collapse = ", "))

combined_data <- bind_rows(Filter(Negate(is.null), cruise_dat)) 

message("Most recent cruise in API: ", cruises_df$name[which.max(as.Date(cruises_df$start_time))])

## ------------------------------------------ ##
#   3. Manually downloaded R2R elogs
## ------------------------------------------ ##
# for cruises not in API: AR66B
## ring net only: AR61B, AR66B, AR28B, AR31A, AR34B, AR39B

read_r2r <- function(filename, cruise_id) {
  desired <- c("Message ID", "dateTime8601", "Instrument",
               "Action", "Station", "Cast",
               "Latitude", "Longitude", "Comment")
  
  read_csv(here("data", "raw", filename), show_col_types = FALSE) |>
    mutate(cruise    = cruise_id,
           Cast      = as.character(Cast),
           Station   = as.character(Station),
           Latitude  = as.numeric(Latitude),
           Longitude = as.numeric(Longitude)) |>
    select(any_of(c(desired, "cruise")))
}

r2r_manual <- bind_rows(
  read_r2r("R2R_ELOG_AR66B.csv", "AR66B")#,
  # read_r2r("R2R_ELOG_en720.csv", "EN720")
)

# should be TRUE
n_distinct(combined_data$cruise) == length(succeeded) + n_distinct(r2r_manual$cruise)

# names(combined_data); names(r2r_manual)
combined_data <- bind_rows(combined_data, r2r_manual) 
rm(r2r_manual)

unique(combined_data$cruise)

## ------------------------------------------ ##
#   4. Filter to bongo/ring net only
## ------------------------------------------ ##
# keep bongo/ring entries only
# no bongo, no ring : AR22,AR24A,AR24B,AR24C,AR28A,AR34A,AR31B,AR31C,AR39A,AR44,
#AR48A,AR48B,AR61A,AR70B,AR75,AR78,AR82A,AR87A

zoop_tows <- combined_data |>
  filter(Instrument %in% c("Bongo Net", "Bongo", "Ring Net", "RingNet")) |>
  filter(cruise != "AR87B") # OOI cruise; https://www.rvdata.us/search/cruise/AR87B

unique(zoop_tows$Instrument) # seems like the discrepancies might have been fixed
unique(zoop_tows$Station)
unique(zoop_tows$Cast)

zoop_tows <- zoop_tows |>
  mutate(
    Instrument = case_when(
      str_detect(tolower(Instrument), "bongo") ~ "Bongo Net",
      str_detect(tolower(Instrument), "ring")  ~ "Ring Net",
      TRUE ~ Instrument
    ),
    # Replace "LTER" in station name with "L"
    Station = Station |>
      str_replace("^LTER(\\d+)$", "L\\1") |> # Replace "LTER1" with "L1"
      # Ensure MVCO is uppercase
      str_replace(regex("mvco", ignore_case = TRUE), "MVCO") |>
      # Add "L" to the station names (except "u11c" and "MVCO")
      (\(x) ifelse(!str_starts(x, "L") & x != "u11c" & # Add "L" if missing
                     x != "MVCO" & !is.na(x), paste0("L", x), x))() |>
      str_replace("^L0+(\\d+)$", "L\\1"), # Remove leading zeros
    Cast = Cast |>
      # Add "B" for Bongo Nets
      (\(x) ifelse(!is.na(x) & str_detect(Instrument, "Bongo") &
                     !str_starts(x, "B"), paste0("B", x), x))() |>
      # Add "R" for Ring Nets
      (\(x) ifelse(!is.na(x) & str_detect(Instrument, "Ring") &
                     !str_starts(x, "R"), paste0("R", x), x))() |>
      # Remove leading zeros after B or R (B01 -> B1)
      str_replace("^(B|R)0*([1-9]\\d*)[a-zA-Z]*$", "\\1\\2") |>
      # Fix typo BL16 -> B16
      str_replace("^BL", "B") |>
      str_replace("BTest", "Test"),
    across(where(is.character), ~ na_if(.x, ""))
  )

unique(zoop_tows$Instrument)
unique(zoop_tows$Action)
unique(zoop_tows$Station)
unique(zoop_tows$Cast)

zoop_tows |>
  group_by(cruise) |>
  summarise(first_date = min(dateTime8601, na.rm = TRUE)) |>
  arrange(first_date) |>
  pull(cruise) |>
  (\(x) message(length(x), " cruises: ", paste(x, collapse = ", ")))()

## --- Ring Net only cruises --- 
zoop_tows |>
  group_by(cruise) |>
  summarise(has_bongo = any(Instrument == "Bongo Net"),
            has_ring  = any(Instrument == "Ring Net"),
            first_date = min(dateTime8601, na.rm = TRUE)) |>
  filter(has_ring & !has_bongo) |>
  arrange(first_date) |>
  pull(cruise) |>
  (\(x) message(length(x), " ring-net-only cruises: ", paste(x, 
                                                             collapse = ", ")))()
#EN685 (this was URI cruise summer 2022; 35 ring net casts) 

## ------------------------------------------ ##
#   5. Add date columns
## ------------------------------------------ ##

zoop_tows <- zoop_tows |>
  mutate(
    date  = as.Date(dateTime8601),
    year  = year(dateTime8601),
    month = month(dateTime8601),
    day   = day(dateTime8601),
    time  = format(dateTime8601, "%H:%M:%S")
  ) |>
  clean_names()

## ------------------------------------------ ##
#   6. Quick checks
## ------------------------------------------ ##
cat("Cruises in output:\n"); print(unique(zoop_tows$cruise))
cat("Instruments:\n");       print(unique(zoop_tows$instrument))
cat("Rows: ", nrow(zoop_tows), "\n")

(m1 <- ggplot(zoop_tows |> filter(!is.na(latitude)), 
       aes(x = longitude, y = latitude)) +
  borders("world", fill = "gray85", color = "white") +
  geom_point(aes(color = cruise), size = 1.5, alpha = 0.7, show.legend = FALSE) +
  coord_quickmap(xlim = c(-85, -55), ylim = c(30, 55)) +
  theme_bw() +
  labs(title = "all cruises")
)

m1 + coord_quickmap(xlim = c(-73, -69), ylim = c(38.5, 43))

## ------------------------------------------ ##
#   7. Save
## ------------------------------------------ ##
write_csv(zoop_tows, here("data", "processed",
                          paste0("elog_zoop_tows_", Sys.Date(), ".csv")))
