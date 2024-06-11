# Load libraries
library(dplyr)
library(janitor)
library(readr)

# Read and clean the functional groups data
func_groups <- read.csv("data/cluster_to_create_traits.csv") %>% 
  janitor::clean_names() 

# Read and clean the ltem data
ltem <- readRDS("data/ltem_historic_updated_modified_2024-04-23.RDS") %>% 
  janitor::clean_names() %>% 
  filter(!(reef == "SAN_JOSE_ANIMAS_PINACULOS" & year == 2014 & species == "Hoplopagrus guentherii"))

# Merge the datasets
merged_data <- left_join(ltem, func_groups, by = "species")

# Read fish productivity data
fishdata <- readRDS("data/fish_productivity_data.RDS")

# str(fishdata) is used to view the structure of the data
str(fishdata)

# Get distinct species and commercial status
fish <- fishdata %>% 
  distinct(species, commercial)

# Get distinct reefs and protection status
reef <- fishdata %>% 
  distinct(reef, protection)

# Clean and prepare merged data
data <- merged_data %>% 
  janitor::clean_names() 

str(data)

data <- data %>%
  select(-region_y) %>%
  rename(region = region_x)

# Add species commercial status to the data
data <- merge(data, fish, by = "species", all.x = TRUE)

# Add reef protection status to the data
data <- merge(data, reef, by = "reef", all.x = TRUE)

# Check the structure of the final data
str(data)

# Save the result
# saveRDS(data, "data/fish_datagr_prod-by-species-herbivoros.RDS")

# Filter and prepare data for visualization
ver <- data %>% 
  filter(label == "PEC") %>% 
  distinct(species, trophic_level_x, trophic_level_y)

data <- data %>%
  select(-trophic_level_y) %>%
  rename(trophic_level = trophic_level_x)

str(data)

# Save the integrated historical data
# saveRDS(data, "data/ltem_historic_integrada_2024-05-24.RDS")

# SST merge
ltem <- readRDS("data/ltem_historic_integrada_2024-05-24.RDS")
sst_data <- readRDS("data/sst_for_reefs.RDS")

# Define a function to convert to CamelCase
toCamelCase <- function(x) {
  words <- unlist(strsplit(x, "_"))
  capitalized_words <- paste0(toupper(substring(words, 1, 1)), substring(words, 2))
  camelCase <- paste(capitalized_words, collapse = "")
  return(camelCase)
}

# Apply the function to column names
ltem <- ltem %>%
  rename_with(~ sapply(.x, toCamelCase)) %>% 
  rename(
    IDSpecies = IdSpecies,
    A_ord = AOrd,
    B_pen = BPen,
    MaxSizeTL = MaxSizeTl,
    IDReef = IdReef
  )

# Check the column names
colnames(ltem)

# Filter and prepare fish data
fish <- ltem %>%
  group_by(Reef) %>%
  mutate(tot_year = n_distinct(Year)) %>%
  ungroup()

fish <- fish %>% 
  filter(Label == "PEC") %>% 
  filter(tot_year > 6, Year >= 2010) %>% 
  filter(Region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>% 
  filter(Family %in% c("Acanthuridae", "Girellidae", "Kyphosidae", "Pomacentridae", "Scaridae")) %>% 
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen = as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size = as.numeric(Size),
    Area = as.numeric(Area),
    Month = as.numeric(Month),
    Biomass = (Quantity * A_ord * (Size^B_pen)) / (Area * 100) # Formula to calculate biomass (ton/ha)
  ) %>% 
  mutate(Biomass = as.numeric(Biomass))

unique(fish$Species)

str(fish)

# Prepare SST data and merge with fish data
sst_mean <- sst_data %>%
  rename(Year = year, sstmean = sst_avg)

# Merge data based on "Reef" and "Year" columns
newsites <- left_join(fish, sst_mean, by = c("Reef", "Year"))

colnames(newsites)

newsites <- newsites %>%
  select(-Region.y) %>%
  rename(Region = Region.x)

# Save the final result
# saveRDS(newsites, "data/fishdata_sstmean_maxsize_by_year_herbivoros.RDS")


