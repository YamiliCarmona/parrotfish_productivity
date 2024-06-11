# Load libraries --------------------------------------------------------

library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(ggplot2)     # For data visualization
library(vegan)       # For ecological community analysis

# Load data ------------------------------------------------------------

sspdata <- readRDS("data/fish_datagr_prod-by-species-herbivoros2.RDS") 

# Summarize for each transect -----------------------------------------

data_prod_brut <- sspdata |> 
  filter(Species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  # Sum for each transect
  group_by(Year, Region, Reef, Depth2, Transect) %>%
  mutate(
    Biom = sum(Biom) / Area,    # Biomass (kg ha^−1)
    Prod = sum(Prod) / Area,    # Production (g d^−1 ha^−1)
    Productivity = (Prod / Biom) * 100) %>%
  ungroup() |> 
  # Mean for each site
  group_by(Year, Region, Reef, Transect, Species) %>%
  mutate(
    Biom = mean(Biom),
    Prod = mean(Prod),
    Productivity = mean(Productivity)) %>% 
  ungroup() %>%
  # Transforming data
  mutate(
    log10ProdB = Productivity,          # % per day
    log10Biom = log10(Biom + 1),        # (g m -2)
    log10Prod = log10(Prod + 1),        # (g m-2 d-1)
    Latitude = as.numeric(as.character(Latitude)),
    Longitude = as.numeric(as.character(Longitude)))

# Print summary statistics ---------------------------------------------

print(paste("Minimum biomass:", min(data_prod_brut$Biom)))
print(paste("Maximum biomass:", max(data_prod_brut$Biom)))
print(paste("Minimum production:", min(data_prod_brut$Prod)))
print(paste("Maximum production:", max(data_prod_brut$Prod)))
print(paste("Minimum turnover:", min(data_prod_brut$Productivity)))
print(paste("Maximum turnover:", max(data_prod_brut$Productivity)))

# Data management -------------------------------------------------------

biom75 = quantile(data_prod_brut$log10Biom, 0.95)
biom25 = quantile(data_prod_brut$log10Biom, 0.25)
prod75 = quantile(data_prod_brut$log10ProdB, 0.75)
prod25 = quantile(data_prod_brut$log10ProdB, 0.25)
max_biom = max(data_prod_brut$log10Biom)

# Diving data into 3 classes for each biomass/productivity relationship

management = data_prod_brut %>% 
  mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25, "deadzone",
                        ifelse(log10ProdB > prod75, "partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75, "pristine", "transition")))) %>%
  mutate(Class = as.factor(Class))

range(management$Productivity)
range(management$Biom)
range(management$Prod)

unique(management$Class)

# Clean data ------------------------------------------------------------

sites <- data_prod_brut |> 
  janitor::clean_names()

fish <- sites |> 
  filter(label == "PEC") |> 
  filter(year >= 2010) |> 
  filter(reef %in% c("CANTIL_MEDIO", "CASITAS", "ISLOTE_CABO_PULMO", "LOBERA_CABO_PULMO", "MORROS_CABO_PULMO",
                     "BURROS", "SAN_DIEGO_ABNEGADO", "REFUGIO_MORENA", "PUNTA_BOTELLA", "ISLOTE_AGUA_VERDE",
                     "ESPIRITU_SANTO_PARTIDA_NORESTE", "ESPIRITU_SANTO_ISLOTES_NORTE", "ESPIRITU_SANTO_BALLENA", 
                     "ESPIRITU_SANTO_PAILEBOTE", "ESPIRITU_SANTO_PUNTA_LOBOS",
                     "CARMEN_ABISMO", "CORONADO_LAJAS", "CORONADO_MONO", "DANZANTE_BIZNAGA", "MONSERRAT_PUNTA_SURESTE"
  )) |> 
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |> 
  mutate(
    a_ord = as.numeric(a),
    b_pen = as.numeric(b),
    quantity = as.numeric(quantity),
    size = as.numeric(size),
    area = as.numeric(area),
    month = as.numeric(month),
    biomass = (quantity * a_ord * (size^b_pen)) / (area * 100) # Formula to calculate biomass (ton/ha)
  ) |> 
  mutate(biomass = as.numeric(biomass))

str(fish)

# Process community data -----------------------------------------------

community_data <- fish |> 
  group_by(year, mpa, protection_level, region, reef, habitat, transect, depth2, species) |>
  summarise(
    abundance = sum(quantity, na.rm = TRUE),
    richness = n_distinct(species),
    biomass = sum(biomass, na.rm = TRUE),
    prod = mean(prod), 
    productivity = mean(productivity),
    size = mean(size, na.rm = TRUE)) |> 
  group_by(year, region, reef, transect, species) |>
  summarise(
    abundance = mean(abundance),
    richness = sum(richness),
    avg_biomass = mean(biomass, na.rm = TRUE),
    prod = mean(prod), 
    productivity = mean(productivity),
    size = mean(size))

unique(community_data$reef)

