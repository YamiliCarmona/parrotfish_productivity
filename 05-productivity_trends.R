library(dplyr)
library(tidyr)
library(ggplot2)

# Load data-------
sspdata <- readRDS("data/fish_datagr_prod-by-species-herbivoros2.RDS") 

# Filter data-------
fish <- sspdata |> 
  janitor::clean_names() |> 
  filter(label == "PEC") |> 
  filter(year >= 2010) |> 
  filter(reef %in% c("CANTIL_MEDIO", "CASITAS", "ISLOTE_CABO_PULMO", "LOBERA_CABO_PULMO", "MORROS_CABO_PULMO",
                     "BURROS", "SAN_DIEGO_ABNEGADO", "REFUGIO_MORENA", "PUNTA_BOTELLA", "ISLOTE_AGUA_VERDE",
                     "ESPIRITU_SANTO_PARTIDA_NORESTE", "ESPIRITU_SANTO_ISLOTES_NORTE", "ESPIRITU_SANTO_BALLENA", "ESPIRITU_SANTO_PAILEBOTE", "ESPIRITU_SANTO_PUNTA_LOBOS",
                     "CARMEN_ABISMO", "CORONADO_LAJAS", "CORONADO_MONO", "DANZANTE_BIZNAGA", "MONSERRAT_PUNTA_SURESTE"
  )) |> 
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

# Sum for each transect-------
data_prod_brut <- sspdata |> 
  filter(Species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  group_by(Year,Region, Reef,Depth2,Transect) %>%
  mutate(
    Biom = sum(Biom)/Area, # (kg ha^−1)
    Prod = sum(Prod)/Area, # g d^−1 ha^−1
    Productivity = (Prod/Biom)*100) %>%
  ungroup() |> 
  group_by(Year, Reef, Species) %>%
  mutate(Biom = mean(Biom),
         Prod = mean(Prod),
         Productivity = mean(Productivity)) %>% 
  ungroup() %>%
  mutate(
    log10ProdB = Productivity, # % per day
    log10Biom = log10(Biom+1), #(g m -2)
    log10Prod = log10(Prod+1), # (g m-2 d-1)
    Latitude = as.numeric(as.character(Latitude)),
    Longitude = as.numeric(as.character(Longitude)))

# Print min/max values-------
print(paste("Minimum biomass:",min(data_prod_brut$Biom)))
print(paste("Maximum biomass:", max(data_prod_brut$Biom)))
print(paste("Minimum production:",min(data_prod_brut$Prod)))
print(paste("Maximum production:", max(data_prod_brut$Prod)))
print(paste("Minimum turnover:",min(data_prod_brut$Productivity)))
print(paste("Maximum turnover:", max(data_prod_brut$Productivity)))

# Data management---------------
biom75 = quantile(data_prod_brut$log10Biom,0.95)
biom25 = quantile(data_prod_brut$log10Biom,0.25)
prod75 = quantile(data_prod_brut$log10ProdB,0.75)
prod25 = quantile(data_prod_brut$log10ProdB,0.25)
max_biom = max(data_prod_brut$log10Biom)

# Diving data into 3 classes for each biomass/productivity relationship
management = data_prod_brut %>% 
  mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                        ifelse(log10ProdB > prod75,"partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
  mutate(Class = as.factor(Class))

# Species analysis-------
sites <- data_prod_brut |> 
  janitor::clean_names()

sp <- sites %>%
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |> 
  group_by(year, region) |> 
  summarise(prod = mean(prod), productivity = mean(productivity))

# Plotting species productivity trends-------
ggplot(sp, aes(x = year, y = prod, fill = year)) +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = " ", y = "Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  facet_wrap(~ region, scales = "free", ncol = 1)  +
  theme(legend.position = "none")+
  ylim(0, max(sp$prod, na.rm = TRUE)) 

# Plotting turnover trends-------
ggplot(sp, aes(x = year, y = productivity, fill = year)) +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = "Year", y = "Turnover (P/B × 100 % per day)") +
  theme_classic() +
  facet_wrap(~ region, scales = "free", ncol = 1)  +
  theme(legend.position = "none")+
  ylim(0, max(sp$productivity, na.rm = TRUE)) 

# Obtaining the maximum and minimum values of average productivity to set the y-axis limits
y_min <- min(sp$prod)
y_max <- max(sp$prod)

# Plotting productivity trends for the most productive reefs
ggplot(sp, aes(x = year, y = prod, color = region)) +
  geom_line() +
  geom_point()+
  scale_color_viridis_d(option = "plasma") +  
  theme_minimal() +
  labs(x = "Year", y = "Productivity (g d^−1 ha^−1)", title = " ") +
  theme(legend.position = "none") + 
  facet_wrap(~ region, scales = "free", ncol = 2) + 
  scale_x_continuous(breaks = seq(min(sp$year), max(sp$year), by = 8)) +
  ylim(y_min, y_max)  

# Plotting productivity trends for the top 5 productive species over the years
ggplot(spp, aes(x = year, y = prod, color = species)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "plasma") +  
  theme_minimal() +
  facet_wrap(~ species, scales = "free_y", ncol = 2) + 
  ylim(0, y_max) +
  labs(x = "Year", y = "Productivity", title = " ") +
  theme(legend.position = " ") 

# Scatter plot of regional and local productivity
ggplot(regional_data, aes(x = avg_biomass, y = prod, color = region)) +
  geom_point() +
  labs(
    title = " ",
    x = "Biomass",
    y = "Productivity",
    color = "Region"
  ) +
  theme_minimal()

# Scatter plot of biomass vs. productivity for each species
ggplot(local_data, aes(x = avg_biomass, y = prod, color = region)) +
  geom_point() +
  labs(
    x = "Biomass",
    y = "Productivity",
    color = " "
  ) +
  theme_minimal()  

# Density curves for model coefficients (slopes)
ggplot(local_data, aes(x = prod/avg_biomass)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(
    title = " ",
    x = "Productivity/Biomass",
    y = "Density"
  ) +
  theme_minimal()

# Scatter plot of biomass vs. productivity for each species with fitted regression lines
ggplot(density_data, aes(x = year, y = density, color = species)) +
  geom_point() +
  geom_line(aes(y = prediction), linetype = "dashed") +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    title = " ",
    x = "Year",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d(option = "plasma")




