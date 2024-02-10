################################################################################
# SETUP ########################################################################
################################################################################
# Define health outcomes of interest
health_indicators = c("DIABETES", "OBESITY", "BPHIGH", "CHD") 

# Define counties in Piedmont Triad
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")

# Read in raw PLACES data
health = read.csv(file = "~/Dropbox (Wake Forest University)/14 - GRANTS/CEES-DistanceToFood-May2023/Forsyth-Food-Access/data/health/PLACES-USA.csv") |> 
  dplyr::filter(StateAbbr == "NC", ## subset to NC
                toupper(CountyName) %in% piedmont_triad, ## subset to study area
                MeasureId %in% health_indicators ## subset to outcomes of interest
                ) |> 
  dplyr::mutate(TractFIPS = as.numeric(LocationName), ## make FIPS numeric
                crude_prev = Data_Value / 100, ## calculate crude prevalence (as %)
                POP = TotalPopulation, ## define population variable
                CASES = ceiling(crude_prev * POP) ## rescale from crude prevalence --> cases per POP
                ) |> 
  dplyr::select(TractFIPS, CountyName, POP, MeasureId, CASES) |> ## keep key variables
  tidyr::spread(key = "MeasureId", value = "CASES") ## transform from long (row per outcome) to wide (col per outcome)

# Save data 
health |> 
  write.csv("~/Documents/food/piedmont-triad-data/disease_prevalences_2022.csv", 
            row.names = FALSE)
