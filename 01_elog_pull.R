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
## Inputs (data/raw/):
##   - nes-lter-zooplankton-tow-metadata-v2.csv (EDI inventory knb-lter-nes.24.2)
##    https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.24.2
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
message("Succeeded (", length(succeeded), "): ", 
        paste(succeeded, collapse = ", "))

combined_data <- bind_rows(Filter(Negate(is.null), cruise_dat)) 

message("Most recent cruise in API: ", 
        cruises_df$name[which.max(as.Date(cruises_df$start_time))])

## ------------------------------------------ ##
#   3. Manually downloaded R2R elogs
## ------------------------------------------ ##
# for cruises not in API: AR66B

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

# names(combined_data); names(r2r_manual)
combined_data <- bind_rows(combined_data, r2r_manual) 
# should be TRUE
n_distinct(combined_data$cruise) == length(succeeded) + n_distinct(r2r_manual$cruise)

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
      # For Bongo Net: ensure starts with B
      (\(x) ifelse(!is.na(x) & str_detect(Instrument, "Bongo") &
                     !str_starts(x, "B"), paste0("B", x), x))() |>
      # For Ring Net: replace B prefix with R (handles AR99 case)
      (\(x) ifelse(!is.na(x) & str_detect(Instrument, "Ring") &
                     str_starts(x, "B"), paste0("R", str_sub(x, 2)), x))() |>
      # For Ring Net: add R if no prefix at all
      (\(x) ifelse(!is.na(x) & str_detect(Instrument, "Ring") &
                     !str_starts(x, "R") & !str_starts(x, "B"),
                   paste0("R", x), x))() |>
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
#   5. FIX elog entry errors 
## ------------------------------------------ ##
zoop_tows <- zoop_tows |> clean_names() |> rename(datetime8601 = date_time8601)

# print duplicate entries
zoop_tows |>
  filter(action %in% c("deploy", "recover")) |>
  count(cruise, station, cast, action) |>
  filter(n > 1) 

## --- handling duplicate entries ---
# --- fix actions ---
zoop_tows <- zoop_tows |>
  mutate(action = case_when(
    # AR34B L11 R10 = second entry (2019-04-18 06:14:00) is a recover, not deploy
    cruise == "AR34B" & station == "L11" & cast == "R10" &
    action == "deploy" &
    datetime8601 == max(datetime8601[cruise == "AR34B" &
                                     station == "L11" &
                                     cast == "R10"]) ~ "recover",
    # AR77 L2 B2 — first deploy was aborted; change earlier deploy to abort
    cruise == "AR77" & station == "L2" & cast == "B2" &
    action == "deploy" &
    datetime8601 == min(datetime8601[cruise == "AR77" &
                                     station == "L2" &
                                     cast == "B2" &
                                     action == "deploy"]) ~ "abort",
    # EN617 flowmeter calibrations: change Action to other via message_id
    # L11     B25 = flowmeter calibation
    # MVCO    B35 = flowmeter calibation == 2018-07-25 02:10:18
    cruise == "EN617" & station == "L11" & cast == "B25" ~ "other",
    cruise == "EN617" & station == "MVCO" & cast == "B35" ~ "other",
    # EN627 L9 cast NA; tow stopped; block malfunction 
    cruise == "EN627" & station == "L9" & is.na(cast) ~ "other",
    # EN685 cast R1 station NA == was a test
    cruise == "EN685" & is.na(station) & cast == "R1" ~ "other",
    TRUE ~ action
  ))

# --- remove rows ---
## 13 rows should be deleted
zoop_tows <- zoop_tows |>
  filter(
    # AR31A L6 R1 has 2 deploy; remove 1 ; remove the later one (keep earlier one)
    # the second one was really close in timestamp
    !(cruise == "AR31A" & station == "L6" & cast == "R1" &
      action == "deploy" &
      datetime8601 == max(datetime8601[cruise == "AR31A" &
                                       station == "L6" &
                                       cast == "R1" &
                                       action == "deploy"])),
    # EN644 L9 B12 — all failed (2x hit bottom) 
    # L9 B18 was the successful one (3rd try) = 2019-08-23 16:15:02 
    !(cruise == "EN644" & station == "L9" & cast == "B12"),
    # EN627 L2 — cast NA, nets hit bottom, no sample
    !(cruise == "EN627" & station == "L2" & is.na(cast)),
    # EN655 L9 B15 — hit bottom, no sample, remove deploy and recover
    !(cruise == "EN655" & station == "L9" & cast == "B15"),
    # EN712 L6 B5 — no sample
    !(cruise == "EN712" & station == "L6" & cast == "B5"),
    # empty entry? i think this may be a typo; no missing stations this cruise
    !(cruise == "AR38" & is.na(station) & is.na(cast)),
    # remove == cast == Test 
    !(cast == "Test" & !is.na(cast))
  )

## check NAs
walk(c("instrument", "action", "station", "cast"), function(col) {
  rows <- zoop_tows |> filter(is.na(.data[[col]]))
  if (nrow(rows) > 0) {
    cat("\n--- NA in", col, "---\n")
    print(rows |> select(cruise, station, cast, action, instrument, datetime8601))
  }
}) #ideally if any NAs, the action == "other" 

## ------------------------------------------ ##
#   6. Patch coordinates and timestamps from meta
## ------------------------------------------ ##
## --- EDI zooplankton inventory package --- 
# knb-lter-nes.24.2
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.24.2
meta <- read_csv(file.path("data",
                           "raw",
                           "nes-lter-zooplankton-tow-metadata-v2.csv"))

meta_patch <- meta |>
  mutate(cast = paste0("B", cast)) |>
  select(cruise, station, cast, latitude_start, longitude_start,
         latitude_end, longitude_end, datetime_UTC_start, datetime_UTC_end)

## --- check if any duplicates (should return 0) ---
zoop_tows |>
  filter(action %in% c("deploy", "recover")) |>
  count(cruise, station, cast, action) |>
  filter(n > 1) |>
  distinct(cruise, station, cast)

cat("NA latitude: ",  sum(is.na(zoop_tows$latitude)),  "\n")
cat("NA longitude:", sum(is.na(zoop_tows$longitude)), "\n")
cat("NA datetime8601:", sum(is.na(zoop_tows$datetime8601)), "\n")

## --- discrepancies between coordinates in meta and elog ---
zoop_tows |>
  left_join(meta_patch, by = c("cruise", "station", "cast")) |>
  filter(action %in% c("deploy", "recover")) |>
  mutate(
    meta_lat = if_else(action == "deploy", latitude_start,  latitude_end),
    meta_lon = if_else(action == "deploy", longitude_start, longitude_end)
  ) |>
  filter(!is.na(meta_lat)) |>
  filter(abs(round(latitude,  2) - round(meta_lat, 2)) > 0.02 |
           abs(round(longitude, 2) - round(meta_lon, 2)) > 0.02) |>
  select(cruise, station, cast, action,
         latitude, meta_lat,
         longitude, meta_lon) |>
  arrange(cruise, station, cast)
## AR38 had issues with the GPS; need to fix 

## --- fix AR38 L6 coordinates ---
# these are also wrong in the meta doc
# patching using the CTD coordinates in the elog
ar38_l6 <- combined_data |>
  filter(cruise == "AR38", Station == "L6", Cast == "8",
         Instrument == "CTD911", Action == "deploy") 

zoop_tows <- zoop_tows |>
  mutate(
    latitude  = if_else(cruise == "AR38" & station == "L6" & cast == "B8",
                        ar38_l6$Latitude,  latitude),
    longitude = if_else(cruise == "AR38" & station == "L6" & cast == "B8",
                        ar38_l6$Longitude, longitude)
  )

## --- fix AR34B L10 coordinates ---
ar34b_l10 <- combined_data |>
  filter(cruise == "AR34B", Cast == "21",
         Instrument == "CTD911", Action == "recover") |>
  slice(1)

zoop_tows <- zoop_tows |>
  mutate(
    latitude  = if_else(cruise == "AR34B" & station == "L10",
                        ar34b_l10$Latitude,  latitude),
    longitude = if_else(cruise == "AR34B" & station == "L10",
                        ar34b_l10$Longitude, longitude)
  )

## --- discrepancies between timestamps in meta and elog ---
zoop_tows |>
  left_join(meta_patch, by = c("cruise", "station", "cast")) |>
  filter(action %in% c("deploy", "recover")) |>
  mutate(
    meta_dt   = if_else(action == "deploy", datetime_UTC_start, datetime_UTC_end),
    diff_mins = as.numeric(difftime(datetime8601, meta_dt, units = "mins"))
  ) |>
  filter(!is.na(meta_dt)) |>
  filter(abs(diff_mins) > 2) |>
  select(cruise, station, cast, action,
         datetime8601, meta_dt, diff_mins) |>
  arrange(cruise, station, cast)

## --- fill missing coordinates and fix timestamps --- 
zoop_tows <- zoop_tows |>
  left_join(meta_patch, by = c("cruise", "station", "cast")) |>
  mutate(
    # fill missing coordinates from meta (deploy -> start, recover -> end)
    latitude = case_when(
      is.na(latitude) & action == "deploy"  & !is.na(latitude_start)  ~ latitude_start,
      is.na(latitude) & action == "recover" & !is.na(latitude_end)    ~ latitude_end,
      TRUE ~ latitude
    ),
    longitude = case_when(
      is.na(longitude) & action == "deploy"  & !is.na(longitude_start) ~ longitude_start,
      is.na(longitude) & action == "recover" & !is.na(longitude_end)   ~ longitude_end,
      TRUE ~ longitude
    ),
    # fix timestamps where mismatch > 2 mins
    datetime8601 = case_when(
      action == "deploy"  & !is.na(datetime_UTC_start) &
        abs(as.numeric(difftime(datetime8601, datetime_UTC_start,
                                units = "mins"))) > 2 ~ datetime_UTC_start,
      action == "recover" & !is.na(datetime_UTC_end) &
        abs(as.numeric(difftime(datetime8601, datetime_UTC_end,
                                units = "mins"))) > 2 ~ datetime_UTC_end,
      TRUE ~ datetime8601
    )
  ) |>
  select(-latitude_start, -longitude_start,
         -latitude_end, -longitude_end,
         -datetime_UTC_start, -datetime_UTC_end)

# quick check 
cat("remaining NA latitude: ",  sum(is.na(zoop_tows$latitude)),  "\n")
cat("remaining NA longitude:", sum(is.na(zoop_tows$longitude)), "\n")
# EN627 L1 B3 = this one is a strange sample; they hit bottom; no bongo sample
# kept but they kept the size fraction samples from the ring net 
# this is not in the bongo meta file (L1 B44 is)

## ------------------------------------------ ##
#   7. Add date columns
## ------------------------------------------ ##

zoop_tows <- zoop_tows |>
  mutate(
    date  = as.Date(datetime8601),
    year  = year(datetime8601),
    month = month(datetime8601),
    day   = day(datetime8601),
    time  = format(datetime8601, "%H:%M:%S")
  ) 

## ------------------------------------------ ##
#   Quick checks
## ------------------------------------------ ##
cat("Cruises in output:\n"); print(unique(zoop_tows$cruise))
cat("Instruments:\n");       print(unique(zoop_tows$instrument))
cat("Rows: ", nrow(zoop_tows), "\n")

(m1 <- ggplot(zoop_tows |> filter(!is.na(latitude)), 
       aes(x = longitude, y = latitude)) +
  borders("world", fill = "gray85", color = "white") +
  geom_point(aes(color = cruise), size = 1.5, alpha = 0.7, show.legend = FALSE) +
  coord_quickmap(xlim = c(-85, -55), ylim = c(30, 55)) +
  theme_bw() 
)

m1 + coord_quickmap(xlim = c(-73, -69), ylim = c(38.5, 43))

## ------------------------------------------ ##
#   Save
## ------------------------------------------ ##
most_recent <- zoop_tows |>
  arrange(desc(datetime8601)) |>
  slice(1) |>
  pull(cruise)

write_csv(zoop_tows, here("data", "processed",
                          paste0("elog_zoop_tows_thru", most_recent, 
                                 "_", Sys.Date(), ".csv")))

# write_csv(zoop_tows, here("data", "processed",
#                           paste0("elog_zoop_tows_", Sys.Date(), ".csv")))
