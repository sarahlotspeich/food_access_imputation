################################################################################
# SETUP ########################################################################
################################################################################
# Define health outcomes of interest
health_indicators = c("DIABETES", "OBESITY", "BPHIGH", "CHD") 

# Define counties in Forsyth & border
forsyth_border = c("Forsyth", "Davidson", "Davie", "Yadkin", "Surry", "Stokes", 
                   "Rockingham", "Guilford", "Randolph")

# Read in raw PLACES data
health = read.csv(file = "PLACES-USA.csv") |> 
  dplyr::filter(StateAbbr == "NC", ## subset to NC
                CountyName %in% forsyth_border, ## subset to study area
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
  write.csv("disease_prevalences_2022.csv", 
            row.names = FALSE)
