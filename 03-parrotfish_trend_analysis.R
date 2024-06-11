# Load libraries --------------------------------------------------------
library(ggplot2)
library(dplyr)
library(janitor)
library(tidyverse)

# Load data  ----------
ltem <- readRDS("data/ltem_historic_integrada_2024-05-24.RDS")

fish <- final_data %>% 
  filter(label == "PEC", region == "Corredor") %>% 
  mutate(
    a_ord = as.numeric(a_ord),
    b_pen = as.numeric(b_pen),
    quantity = as.numeric(quantity),
    size = as.numeric(size),
    area = as.numeric(area),
    month = as.numeric(month),
    biomass = (quantity * a_ord * (size^b_pen)) / (area * 100) # Formula to calculate biomass (ton/ha)
  ) %>% 
  mutate(biomass = as.numeric(biomass))

# Fish size trends ----------
fish <- fish %>%
  group_by(reef) %>%
  mutate(tot_year = n_distinct(year)) %>%
  ungroup()

# Step 2: Create the table with necessary filters and calculations
datatable <- fish %>%
  filter(label == "PEC") %>%
  # filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>%
  # filter(tot_year > 6, year >= 2010) %>%
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) %>%
  group_by(year, region, reef, depth2, transect, species) %>%
  summarise(size = mean(size, na.rm = TRUE), .groups = 'drop') %>%
  collect()

fish %>%
  ggplot(aes(x = year, y = quantity)) +
  geom_point() +
  # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  # facet_grid( ~ region)
  facet_grid(region ~ species)

datatable %>% 
  ggplot(aes(x = year, y = size)) +
  geom_point() +
  labs(
    title = " ",
    x = "Year",
    y = "Size"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave("herb/size_herbivoros_plot.png", width = 12, height = 8, dpi=1000)

# Plotting with corrected column name
datatable %>% 
  group_by(year, species, region) %>%
  summarise(size = mean(size), .groups = 'drop') %>%
  ggplot(aes(x = year, y = size)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(
    title = " ",
    x = "Year",
    y =  "Size"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave("herb/size_herbivoros_smooth.png", width = 12, height = 8, dpi=1000)

herbivoro <- fish %>%
  filter(family %in% c("Acanthuridae", "Girellidae", "Kyphosidae", "Pomacentridae", "Scaridae"))

unique(herbivoro$species)

# Group data by year and species, and calculate the average size of fish for each combination
size_trends_species <- fish %>%
  filter(commercial == "yes") %>% 
  group_by(year, species) %>%
  summarize(avg_size = mean(size, na.rm = TRUE))

# Create trend plot with facets by species
ggplot(size_trends_species, aes(x = year, y = avg_size, group = species)) +
  geom_line() +
  geom_point() +
  labs(
    title = " ",
    x = "Year",
    y = "Average Fish Size"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# Save the plot
# ggsave("data/size_trends_species.png", width = 12, height = 8, dpi = 300)

ggplot(size_trends_species, aes(x = year, y = avg_size, group = species)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression lines
  labs(
    title = " ",
    x = "Year",
    y = "Average Size"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend

# Biomass  ------------
annual_biomass <- fish %>%
  filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>%
  filter(tot_year > 6, year >= 2010) %>%
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) %>%
  group_by(year, region, reef, depth2, transect, species) %>%  # Group the data
  summarise(biomass = sum(biomass, na.rm = TRUE)) %>% 
  group_by(year, region, species) %>% 
  summarise(avg_biomass = mean(biomass, na.rm = TRUE))

ggplot(annual_biomass, aes(x = year, y = avg_biomass)) +
  geom_line() +
  geom_point() +
  labs(
    title = " ",
    x = "Year",
    y = "Average Biomass (ton/ha)"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave("herb/biomass_herbivoros.png", width = 12, height = 8, dpi=1000)

(p <- fish %>% 
    filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>%
    filter(tot_year > 6, year >= 2010) %>%
    filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                          "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
    mutate(year = factor(year),  # Factorize year
           region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) %>% 
    group_by(year, region, island, reef, transect, depth2, species) %>%  # Group the data
    summarise(biomass = sum(biomass, na.rm = TRUE)) %>%  # Sum biomass
    group_by(year, region, species) %>%  # Group by year, region, and species
    summarise(biomass = mean(biomass, na.rm = TRUE)) %>%  # Calculate average biomass
    ggplot(aes(x = year , y = biomass, fill = species)) +  # Create the plot
    geom_bar(position = "stack", stat = "identity", col = "black") +  # Add bars
    # geom_point() +
    scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e", "#f4a582", "#92c5de"), guide = "none") +  # Customize colors
    # scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"), guide = "none") +  # Customize colors
    # facet_wrap(~region) +  # Divide by region
    facet_grid(region ~ species) +
    scale_x_discrete(breaks = c("2010",  "2013", "2016", "2019", "2022")) +
    labs(x = "Year", y = "Biomass (ton/ha)", fill = "Species") +  # Labels
    theme_classic())

# ggsave("herb/biomass_herbivoros_histo.png", width = 12, height = 8, dpi=1000)

# Increase or decrease significantly? ------------

# Data visualization
ggplot(datatable, aes(x = year, y = size)) +
  geom_point() +
  labs(title = " ",
       x = "Year",
       y = "Size") +
  theme_minimal()

# Time trend analysis
model <- lm(size ~ year, data = datatable)

# Model summary
summary(model)

# Hypothesis test on the slope
# Null hypothesis: No significant change in size over time
# Alternative hypothesis: Significant change in size over time
coef_test <- coef(summary(model))["year", "Pr(>|t|)"]
p_value <- coef_test

# Print the p-value of the hypothesis test
print(p_value)

library(broom)

# Fit linear models for each species
size_trends_models <- datatable %>%
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>% 
  group_by(species) %>%
  do(model = lm(size ~ year, data = .))

# Extract model coefficients (slopes) and their significance
model_coefficients <- size_trends_models %>%
  tidy(model) %>%
  filter(term == "year")

# View coefficients and their p-values
print(model_coefficients)

# View coefficients and their p-values
model_coefficients %>%
  select(species, estimate, std.error, statistic, p.value)

# Filter species with significant slopes
significant_trends <- model_coefficients %>%
  filter(p.value < 0.05) %>%
  mutate(trend = ifelse(estimate > 0, "Increases", "Decreases"))

# View results
significant_trends

