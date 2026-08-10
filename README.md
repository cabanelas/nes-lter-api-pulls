# nes-lter-api-pulls

Pulls and cleans zooplankton tow event log data from the NES-LTER API v2 
and manually downloaded R2R elogs.

## Usage
Requires R with: `tidyverse`, `here`, `lubridate`, `glue`.
Open the `.Rproj` and run `01_elog_pull.R`. Output is written to `data/processed/`.

## Scripts
- `01_elog_pull.R` — pulls bongo/ring net event logs, cleans station/cast 
  names, fixes known coordinate and timestamp errors, and saves output

## Data sources
- NES-LTER API v2: https://nes-lter-api.whoi.edu/api/docs
- EDI zooplankton tow metadata (knb-lter-nes.24.2, downloaded 07-NOV-2025): 
  https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.24.2
- Manual R2R elogs for cruises not yet in API (AR66B)

## Output
`data/processed/elog_zoop_tows_thru[CRUISE]_[DATE].csv`

**One row per action (deploy/recover), NOT one row per tow.** A single tow
appears as (up to) two rows — a `deploy` and a `recover`. To get one row per
tow, pivot on `action`.

Columns:
| column | description |
|---|---|
| `message_id` | R2R event-log message ID |
| `datetime8601` | event timestamp (UTC, POSIXct) — curated (see Notes) |
| `instrument` | `Bongo Net` or `Ring Net` |
| `action` | `deploy`, `recover`, `other`, or `abort` (see below) |
| `station` | station name (e.g. `L1`, `MVCO`) |
| `cast` | cast label with gear prefix (e.g. `B1` bongo, `R1` ring) |
| `latitude`, `longitude` | decimal degrees |
| `comment` | free-text elog comment |
| `cruise` | cruise ID |
| `date`, `year`, `month`, `day`, `time` | parsed from `datetime8601` for convenience |

### `action` vocabulary
Only `deploy` and `recover` are real sampling events. `other` = calibrations,
tests, and non-tow entries; `abort` = failed/aborted tows (e.g. redeployed due
to gear issues). Filter to `deploy`/`recover` for analysis.

## Notes
- Cruises with no event log in API return HTTP 404 and are skipped
- Known coordinate fixes: AR38 L6 B8, AR34B L10 (GPS errors)
- Known timestamp fixes applied via meta comparison (>2 min threshold)
- Duplicate/failed tow entries documented in script comments