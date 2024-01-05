# Load historical SNAP retailer data
## Can be downloaded here: https://www.fns.usda.gov/snap/retailer-locator
orig_snap = read.csv(file = "raw_SNAP.csv")

# Subset to Forsyth & bordering counties
## Define border counties
border_counties = c("DAVIDSON", "DAVIE",  "FORSYTH", "GUILFORD", "RANDOLPH", "ROCKINGHAM",
                    "STOKES", "SURRY", "YADKIN")
## Define border counties of border counties 
border_border_counties = c("ALLEGHANY", "WILKES", "IREDELL", "ROWAN", "ALAMANCE", "CASWELL")
## Subset to SNAP data in the above counties
forsyth_border_snap = orig_snap |> 
  dplyr::filter(State == "NC", County %in% c(border_counties, border_border_counties))

# Function to clean/re-format string variables 
## (1) Delete white space before/after elements of names and addresses 
## (2) Make all strings upper case
clean_string_var = function(string_var) {
  stringr::str_trim(toupper(string_var))
}

# Apply function to clean/reformat all string variables 
forsyth_border_snap = forsyth_border_snap |> 
  dplyr::mutate_if(is.character, clean_string_var)

# Subset to "healthy foods" store types 
## Define healthy store types
healthy_types = c("FARMERS' MARKET", "LARGE GROCERY STORE", 
                  "MEDIUM GROCERY STORE", "SMALL GROCERY STORE", 
                  "BAKERY SPECIALTY", "FRUITS/VEG SPECIALTY", 
                  "MEAT/POULTRY SPECIALTY", "SEAFOOD SPECIALTY", 
                  "SUPERMARKET", "SUPER STORE")
## Subset to SNAP data from healthy store types
forsyth_border_snap = forsyth_border_snap |> 
  dplyr::filter(Store.Type %in% healthy_types)

# Transform date columns to date type 
forsyth_border_snap = forsyth_border_snap |> 
  dplyr::mutate(
    Authorization.Date = as.Date(Authorization.Date, "%m/%d/%Y"),
    End.Date = as.Date(End.Date, "%m/%d/%Y")
  )

# Create indicators of latest authorization for each address and still authorized (i.e., no end date)
forsyth_border_snap = forsyth_border_snap |> 
  dplyr::group_by(Street.Number, Street.Name, Additional.Address, City, State, Zip.Code) |> 
  dplyr::mutate(
    Latest.Authorization = max(Authorization.Date),
    Is.Latest.Authorization = Authorization.Date == Latest.Authorization,
    Still.Authorized = is.na(End.Date))

# Exclude rows that were not a store's latest authorization 
forsyth_border_snap = forsyth_border_snap |> 
  dplyr::filter(Is.Latest.Authorization) |> 
  dplyr::select(-Is.Latest.Authorization)
nrow(forsyth_border_snap) ## 1434 rows
nrow(unique(forsyth_border_snap[, c("Street.Number", "Street.Name", "Additional.Address",
                                    "City", "State", "Zip.Code")])) ## but only 1433 unique addresses 

# Filter to duplicate address for manual review
forsyth_border_snap |> 
  dplyr::group_by(Street.Number, Street.Name, Additional.Address, City, State, Zip.Code) |> 
  dplyr::arrange(Street.Number, Street.Name, Additional.Address, City, State, Zip.Code) |>
  dplyr::mutate(address_count = dplyr::n()) |>
  dplyr::filter(address_count > 1) |> 
  data.frame()

## Unclear why duplicates exist - address looks the same for both rows
## Keep the latest authorization (Arlington Meat Processing)
forsyth_border_snap = forsyth_border_snap |> 
  dplyr::filter(Record.ID != 456609)
nrow(forsyth_border_snap) ## 1433 rows
nrow(unique(forsyth_border_snap[, c("Street.Number", "Street.Name", "Additional.Address",
                                    "City", "State", "Zip.Code")])) ## and now 1433 unique addresses 

# Exclude addresses that were not authorized in 2022 
forsyth_border_snap = forsyth_border_snap |> 
  dplyr::filter(!(!Still.Authorized & as.numeric(format(End.Date,'%Y')) < 2022)) 
nrow(forsyth_border_snap) ## 525 rows

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("Forsyth-Food-Access/data/set_api_keys.R")
# Re-geocode using ggmap::geocode()
healthy = forsyth_border_snap |> 
  dplyr::filter(Store.Type %in% healthy_types) |> 
  dplyr::mutate(Address = paste(Street.Number, Street.Name, Additional.Address, City, State, Zip.Code)) |>
  ggmap::mutate_geocode(location = Address)
# Warning messages from geocoding: 
# 1: "207 AVON ST MOCK..." not uniquely geocoded, using "207 avon st, mocksville, nc 27028, usa" 
## The address used matches the address given 
# 2: "2948 N GLENN AVE ..." not uniquely geocoded, using "2948 n glenn ave, winston-salem, nc 27105, usa" 
## The address used matches the address given 
# 3: "313 SALISBURY AVE..." not uniquely geocoded, using "salisbury ave, cambridge, on n1s, canada" 
## Double checked in address for "FOOD LION 547" in Google Maps manually. Should be: 313 Salisbury Ave, Spencer, NC 28159
healthy$Street.Name[healthy$Store.Name == "FOOD LION 547"] = "SALISBURY AVE"
healthy$Address[healthy$Store.Name == "FOOD LION 547"] = with(healthy[healthy$Store.Name == "FOOD LION 547", ], 
                                                              paste(Street.Number, Street.Name, Additional.Address, City, State, Zip.Code))
## Geocode this one again
healthy[healthy$Store.Name == "FOOD LION 547", c("Latitude", "Longitude")] = ggmap::geocode(location = healthy$Address[healthy$Store.Name == "FOOD LION 547"])

# Save final dataset 
healthy |> 
  write.csv("healthy_foods_stores_2022.csv", 
            row.names = F)
