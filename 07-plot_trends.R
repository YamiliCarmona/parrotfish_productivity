
# Sizes ---------

# Plotting mean size over years for each region and species
community_data |> 
  group_by(year, region, species) %>%
  summarise(size = mean(size), .groups = 'drop') |>
  ggplot(aes(x = year, y = size)) +
  geom_line()+
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

# Plotting mean size over years for each region and species with colored lines
community_data |> 
  group_by(year, region, species) |>
  summarise(size = mean(size), .groups = 'drop') |>
  ggplot( aes(x = year, y = size)) +
  geom_line(aes(color = species), alpha = 0.3) +  # Thin lines for random trends
  geom_smooth(aes(color = species), method = "loess", span = 0.75, se = FALSE, size = 1.2) +  # Local fit with adjusted span
  geom_point(aes(color = species), alpha = 0.6) +  # Colored points for raw data
  labs(x = "Year", y = "size", title = " ") +
  theme_bw() +  # Plot style
  theme(axis.text.x = element_text(color = "red")) #+  # X-axis text color
# theme(legend.position = "none")

# Abundance --------------

# Calculate logarithm of abundance
community_data$log_abundance <- log(community_data$abundance)

# Create logarithmic abundance plot over time
community_data |> 
  ggplot( aes(x = year, y = abundance)) +
  geom_line(aes(color = species), alpha = 0.3) +  # Thin lines for random trends
  geom_smooth(aes(color = species), method = "loess", span = 0.75, se = FALSE, size = 1.2) +  # Local fit with adjusted span
  geom_point(aes(color = species), alpha = 0.6) +  # Colored points for raw data
  labs(x = "Year", y = "Abundance", title = " ") +
  theme_bw() +  # Plot style
  theme(axis.text.x = element_text(color = "red")) #+  # X-axis text color
# theme(legend.position = "none")

# Richness --------------

# Calculate logarithm of richness
community_data$log_richness <- log(community_data$richness)

# Create logarithmic richness plot over time
community_data |> 
  group_by(year, region, species) %>%
  summarise(richness = mean(richness), .groups = 'drop') |>
  ggplot( aes(x = year, y = richness)) +
  geom_line(aes(color = species), alpha = 0.3) +  # Thin lines for random trends
  geom_smooth(aes(color = species), method = "loess", span = 0.75, se = FALSE, size = 1.2) +  # Local fit with adjusted span
  geom_point(aes(color = species), alpha = 0.6) +  # Colored points for raw data
  labs(x = "Year", y = "Richness", title = " ") +
  theme_bw() +  # Plot style
  theme(axis.text.x = element_text(color = "red"))

# Biomass -----------------

# Calculate logarithm of biomass
community_data$log_avg_biomass <- log(community_data$avg_biomass)

# Create logarithmic biomass plot over time
community_data |> 
  group_by(year, region, species) %>%
  summarise(avg_biomass = mean(avg_biomass), .groups = 'drop') |>
  ggplot( aes(x = year, y = avg_biomass)) +
  geom_line(aes(color = species), alpha = 0.3) +  # Thin lines for random trends
  geom_smooth(aes(color = species), method = "loess", span = 0.75, se = FALSE, size = 1.2) +  # Local fit with adjusted span
  geom_point(aes(color = species), alpha = 0.6) +  # Colored points for raw data
  labs(x = "Year", y = "Biomass (ton/ha)", title = " ") +
  theme_bw() +  # Plot style
  theme(axis.text.x = element_text(color = "red"))# +  # X-axis text color
# theme(legend.position = "none")

# Biomass historic -----------------

# Creating a histogram of biomass for each species
(p <- fish |> 
   mutate(year = factor(year),  # Factorizing year
          region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |> 
   group_by(year, region, island, reef, transect, depth2, species) |>  # Grouping data
   summarise(biomass = sum(biomass, na.rm = TRUE)) |>  # Summing biomass
   group_by(year, region, species) |>  # Grouping by year, region, and species
   summarise(biomass = mean(biomass, na.rm = TRUE)) |>  # Calculating biomass mean
   ggplot(aes(x = year , y = biomass, fill = species)) +  # Creating plot
   geom_bar(position = "stack", stat = "identity", col = "black") +  # Adding bars
   scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e", "#f4a582", "#92c5de"),  guide = "none") +  # Customizing colors
   facet_grid(region ~ species) +
   scale_x_discrete(breaks = c("2010",  "2013", "2016", "2019", "2022")) +
   labs(x = "Year", y = "Biomass (ton/ha)", fill = "Species") +  # Labels
   theme_classic())


# Productivity -----------

# Maximum value for y-axis
y_max <- max(community_data$prod, na.rm = TRUE)

# Plotting productivity over years for each species
community_data |> 
  group_by(year,  species) %>%
  summarise(prod = mean(prod), .groups = 'drop') |>
  ggplot(aes(x = year, y = prod, color = species)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "plasma") +  # Adjusting colors
  theme_minimal() +
  facet_wrap(~ species, scales = "free_y", ncol = 2) +  # Free scale on Y-axis for each facet
  ylim(0, y_max) +  # Setting the same upper limit on the Y-axis for all plots
  labs(x = "Year", y = "Productivity (g d^−1 ha^−1)", title = " ") +
  theme(legend.position = " ") 


# Plotting productivity over years for each species with smoothed lines
community_data |> 
  group_by(year, species) %>%
  summarise(prod = mean(prod), .groups = 'drop') |>
  ggplot(aes(x = year, y = prod, color = species)) +
  geom_line(color = "black", alpha = 0.3) +  # Thin lines for random trends
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +  # Thick lines for median trends
  geom_point(alpha = 0.6) +  # Colored points for raw data
  scale_y_log10() +  # Logarithmic scale on the Y-axis
  labs(x = "Year", y = "Productivity (g d^−1 ha^−1)", title = " ") +
  theme_bw() +  # Plot style
  theme(axis.text.x = element_text(color = c("red", "green", "yellow", "blue"))) #+  # X-axis text color based on stress events
# facet_grid(region ~ species) +
# theme(legend.position = "none")

# Turnover -----------

# Plotting turnover over years for each region and species
community_data |> 
  group_by(year, region, species) %>%
  summarise(productivity = mean(productivity), .groups = 'drop') |>
  ggplot(aes(x = year, y = productivity, color = species)) +
  geom_line(color = "black", alpha = 0.3) +  # Thin lines for random trends
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +  # Thick lines for median trends
  geom_point(alpha = 0.6) +  # Colored points for raw data
  scale_y_log10() +  # Logarithmic scale on the Y-axis
  labs(x = "Year", y = "Turnover (P/B × 100 % per day)", title = " ") +
  theme_bw() +  # Plot style
  theme(axis.text.x = element_text(color = c("red", "green", "yellow", "blue"))) +  # X-axis text color based on stress events
  facet_grid(region ~ species) +
  theme(legend.position = "none")

# ggsave("herb/turnover_herbivoros_spp_.png", width = 12, height = 8, dpi=1000)

