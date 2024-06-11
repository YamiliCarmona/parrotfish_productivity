# Load libraries --------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
ltem <- readRDS("data/ltem_historic_integrada_2024-05-24.RDS") 

regions <- c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")

# Filter by regions of interest
filtered_ltem <- ltem %>%
  filter(year >= 2010) %>% 
  filter(region %in% regions) %>%
  filter(reef %in% c("CANTIL_MEDIO", "CASITAS", "ISLOTE_CABO_PULMO", "LOBERA_CABO_PULMO", "MORROS_CABO_PULMO", #"SAN_JOSE_ANIMAS_NORTE", 
                     "BURROS", "SAN_DIEGO_ABNEGADO", "REFUGIO_MORENA", "PUNTA_BOTELLA", "ISLOTE_AGUA_VERDE",
                     "ESPIRITU_SANTO_PARTIDA_NORESTE", "ESPIRITU_SANTO_ISLOTES_NORTE", "ESPIRITU_SANTO_BALLENA", "ESPIRITU_SANTO_PAILEBOTE", "ESPIRITU_SANTO_PUNTA_LOBOS",
                     "CARMEN_ABISMO", "CORONADO_LAJAS", "CORONADO_MONO", "DANZANTE_BIZNAGA", "MONSERRAT_PUNTA_SURESTE"
  ))
# filter(family == "Scaridae")
# filter(family %in% c("Carcharhinidae", "Ginglymostomatidae", "Sphyrnidae"))  

unique(filtered_ltem$protection)

# Define a new column for protection classification
filtered_ltem <- filtered_ltem %>%
  mutate(protection = case_when(
    protection_status == "cabo pulmo" & protection_level == "Prohibited" ~ "Fully Protected",
    protection_status == "cabo pulmo" & protection_level == "Open Area" ~ "Not Protected",
    protection_status == "sin proteccion" & protection_level == "Open Area" ~ "Not Protected",
    protection_status == "area protegida" & protection_level == "Allowed" ~ "Lightly Protected",
    protection_status == "area protegida" & protection_level == "Open Area" ~ "Not Protected",
    protection_status == "area protegida" & protection_level == "Prohibited" ~ "Lightly Protected",
    protection_status == "cabo pulmo" & protection_level == "Allowed" ~ "Lightly Protected",
    protection_status == "refugio pesquero" & protection_level == "Open Area" ~ "Fishing Refuge",
    TRUE ~ "" 
  ))

site_summary <- filtered_ltem %>%
  group_by(region, reef, depth2) %>%
  summarise(total_years = n_distinct(year), .groups = 'drop')

threshold_years <- 6

consistent_sites <- site_summary %>%
  filter(total_years >= threshold_years)

filtered_ltem <- filtered_ltem %>%
  inner_join(consistent_sites, by = c("region", "reef", "depth2"))

# Select relevant columns
site_characteristics <- filtered_ltem %>%
  # filter(region == "Corredor") %>% 
  select(year, region, reef, depth, depth2, protection, total_years) %>%
  distinct()

# Standardize the number of representative sites by region, year, and depth
standardize_sites <- function(data, max_sites = 5) {
  data %>%
    group_by(region, year, depth2, protection) %>%
    sample_n(min(n(), max_sites)) %>%
    ungroup()
}

# Apply the function to standardize the sites
standardized_sites <- standardize_sites(site_characteristics)

sites <- standardized_sites %>% 
  distinct(region, reef, depth2, total_years)

# Filter the original data to keep only the standardized sites
final_data <- filtered_ltem %>%
  inner_join(standardized_sites, by = c("year", "region", "reef", "depth2", "protection"))

# Check the number of sites by region, depth, and protection
site_counts <- final_data %>%
  group_by(region, year, depth2, protection) %>%
  summarise(num_sites = n_distinct(reef), .groups = 'drop')

print(site_counts)

# Count the number of sites by year and region
site_counts_by_year_region <- final_data %>%
  group_by(region, year) %>%
  summarise(num_sites = n_distinct(reef), .groups = 'drop')

print(site_counts_by_year_region)

# Save the file with saveRDS
saveRDS(final_data, "data/standardized_ltem_sites_5_by_region.RDS")
# saveRDS(final_data, "data/standardized_ltem_sites.RDS")
