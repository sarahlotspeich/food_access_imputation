# Load historical SNAP retailer data
## Can be downloaded here: https://www.fns.usda.gov/snap/retailer-locator
orig_snap = read.csv(file = "~/Dropbox (Wake Forest University)/14 - GRANTS/CEES-DistanceToFood-May2023/Forsyth-Food-Access/data/grocery_stores/raw_SNAP.csv")

# Subset to Piedmont Triad & bordering counties
## Define Piedmont Triad counties
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")

## Define bordering counties
border_counties = c("PERSON", "ORANGE", "CHATHAM", 
                    "MOORE", "STANLY", "CABARRUS", 
                    "ROWAN", "IREDELL", "WILKES", 
                    "ALLEGHANY", "RICHMOND", "ANSON")

## Subset to SNAP data in the above counties
piedmont_border_snap = orig_snap |> 
  dplyr::filter(State == "NC", 
                County %in% c(piedmont_triad, border_counties))

# Function to clean/re-format string variables 
## (1) Delete white space before/after elements of names and addresses 
## (2) Make all strings upper case
clean_string_var = function(string_var) {
  stringr::str_trim(toupper(string_var))
}

# Apply function to clean/reformat all string variables 
piedmont_border_snap = piedmont_border_snap |> 
  dplyr::mutate_if(is.character, clean_string_var)

# Subset to "healthy foods" store types 
## Define healthy store types
healthy_types = c("FARMERS' MARKET", "LARGE GROCERY STORE", 
                  "MEDIUM GROCERY STORE", "SMALL GROCERY STORE", 
                  "BAKERY SPECIALTY", "FRUITS/VEG SPECIALTY", 
                  "MEAT/POULTRY SPECIALTY", "SEAFOOD SPECIALTY", 
                  "SUPERMARKET", "SUPER STORE")
## Subset to SNAP data from healthy store types
piedmont_border_snap = piedmont_border_snap |> 
  dplyr::filter(Store.Type %in% healthy_types)

# Transform date columns to date type 
piedmont_border_snap = piedmont_border_snap |> 
  dplyr::mutate(
    Authorization.Date = as.Date(Authorization.Date, "%m/%d/%Y"),
    End.Date = as.Date(End.Date, "%m/%d/%Y")
  )

# Create indicators of latest authorization for each address and still authorized (i.e., no end date)
piedmont_border_snap = piedmont_border_snap |> 
  dplyr::group_by(Street.Number, Street.Name, Additional.Address, City, State, Zip.Code) |> 
  dplyr::mutate(
    Latest.Authorization = max(Authorization.Date),
    Is.Latest.Authorization = Authorization.Date == Latest.Authorization,
    Still.Authorized = is.na(End.Date))

# Exclude rows that were not a store's latest authorization 
piedmont_border_snap = piedmont_border_snap |> 
  dplyr::filter(Is.Latest.Authorization) |> 
  dplyr::select(-Is.Latest.Authorization)

# Exclude addresses that were not authorized in 2022 
piedmont_border_snap = piedmont_border_snap |> 
  dplyr::filter(!(!Still.Authorized & as.numeric(format(End.Date,'%Y')) < 2022)) 
nrow(piedmont_border_snap) ## 701 rows
nrow(unique(piedmont_border_snap[, c("Street.Number", "Street.Name", "Additional.Address",
                                    "City", "State", "Zip.Code")])) ## All unique addresses 

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("~/Documents/food/set_api_keys.R")
# Re-geocode using ggmap::geocode()
healthy = piedmont_border_snap |> 
  dplyr::filter(Store.Type %in% healthy_types) |> 
  dplyr::mutate(Address = paste(Street.Number, Street.Name, Additional.Address, City, State, Zip.Code)) |>
  ggmap::mutate_geocode(location = Address)

# Warning messages:
# 1: "4055 HARRISBURG D..." not uniquely geocoded, using "4055 harrisburg dr, harrisburg, nc 28075, usa" 
## The address used matches the address given 
# 2: "207 AVON ST MOCK..." not uniquely geocoded, using "207 avon st, mocksville, nc 27028, usa" 
## The address used matches the address given 
# 3: "646 RIVER HWY MO..." not uniquely geocoded, using "646 river hwy, mooresville, nc 28117, usa" 
## The address used matches the address given 
# 4: "10205 US 15 501 H..." not uniquely geocoded, using "10205 us-15 #37, southern pines, nc 28387, usa" 
## The address used matches the address given
# 5: "400 MILL CREEK RD..." not uniquely geocoded, using "400 mill creek rd, carthage, nc 28327, usa" 
## The address used matches the address given
# 6: "3419 US HIGHWAY 7..." not uniquely geocoded, using "us hwy 70 w, efland, nc 27243, usa" 
## The address used is less precise than the address given
### Double checked in address for "EFLAND SUPERMARKET" in Google Maps manually. 
### Should be: 3419 US-70, Efland, NC 27243
healthy$Street.Name[healthy$Store.Name == "EFLAND SUPERMARKET"] = "US-70"
healthy$Address[healthy$Store.Name == "EFLAND SUPERMARKET"] = with(healthy[healthy$Store.Name == "EFLAND SUPERMARKET", ], 
                                                              paste(Street.Number, Street.Name, Additional.Address, City, State, Zip.Code))
### Geocode this one again
healthy[healthy$Store.Name == "EFLAND SUPERMARKET", c("Longitude", "Latitude")] = ggmap::geocode(location = healthy$Address[healthy$Store.Name == "EFLAND SUPERMARKET"])
# 7: "120 US HIGHWAY 70..." not uniquely geocoded, using "120 us-70, hillsborough, nc 27278, usa" 
## The address used matches the address given
# 8: "3707 US HIGHWAY 2..." not uniquely geocoded, using "3707 us-220, asheboro, nc 27205, usa" 
## The address used matches the address given
# 9: "1226 E DIXIE DR ..." not uniquely geocoded, using "1226 e dixie dr, asheboro, nc 27203, usa" 
## The address used matches the address given
# 10: "6370 NC HWY 66 K..." not uniquely geocoded, using "6370 nc-66, king, nc 27021, usa" 
## The address used matches the address given

# Save final dataset 
healthy |> 
  write.csv("~/Documents/food/piedmont-triad-data/healthy_foods_stores_2022.csv", 
            row.names = F)
