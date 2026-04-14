# nes-lter-api-pulls

Pulls and cleans zooplankton tow event log data from the NES-LTER API v2 
and manually downloaded R2R elogs.

## Scripts
- `01_elog_pull.R` — pulls bongo/ring net event logs, cleans station/cast 
  names, fixes known coordinate and timestamp errors, and saves output

## Data sources
- NES-LTER API v2: https://nes-lter-api.whoi.edu/api/docs
- EDI zooplankton tow metadata (knb-lter-nes.24.2): 
  https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.24.2
- Manual R2R elogs for cruises not yet in API (AR66B)

## Output
`data/processed/elog_zoop_tows_thru[CRUISE]_[DATE].csv`

## Notes
- Cruises with no event log in API return HTTP 404 and are skipped
- Known coordinate fixes: AR38 L6 B8, AR34B L10 (GPS errors)
- Known timestamp fixes applied via meta comparison (>2 min threshold)
- Duplicate/failed tow entries documented in script comments