# 1. mergedata------------
func_groups <- read.csv("data/cluster_to_create_traits.csv") |> 
  janitor::clean_names() 

ltem <- readRDS("data/tabla/ltem_historic_updated_modified_2024-04-23.RDS") |> 
  janitor::clean_names() |> 
  filter(!(reef == "SAN_JOSE_ANIMAS_PINACULOS" & year == 2014 & species == "Hoplopagrus guentherii"))

# Unir las bases de datos
merged_data <- left_join(ltem, func_groups, by = "species")



fishdata <- readRDS("data/tabla/fish_productivity_data.RDS")

# sspdata <- readRDS("data/fish_datagr_prod-by-species-herbivoros.RDS")
# alldata <- readRDS("data/fishdata_prod-by-reef-herbivoros.RDS")


str(fishdata)

fish <- fishdata |> 
  distinct(species, commercial)


reef <- fishdata |> 
  distinct(reef, protection)


data <- merged_data|> 
  janitor::clean_names() 

str(data)

data <- data %>%
  select(-region_y) %>%
  rename(region = region_x)

# Añadir la información de las especies y su estado comercial a alldata
data <- merge(data, fish, by = "species", all.x = TRUE)

# Añadir la información de los arrecifes y su estado de protección a alldata
data <- merge(data, reef, by = "reef", all.x = TRUE)

# Verificar el resultado
str(data)

# saveRDS(data, "data/fish_datagr_prod-by-species-herbivoros.RDS")


ver<- data |> 
  filter(label=="PEC") |> 
  distinct(species, trophic_level_x,trophic_level_y)

data <- data %>%
  select(-trophic_level_y) %>%
  rename(trophic_level = trophic_level_x)


str(data)

# saveRDS(data, "data/ltem_historic_integrada_2024-05-24.RDS")


# sst merge----------

ltem <- readRDS("data/ltem_historic_integrada_2024-05-24.RDS")

sst_data <- readRDS("data/tabla/sst_for_reefs.RDS")


# Definir una función para convertir a CamelCase
toCamelCase <- function(x) {
  words <- unlist(strsplit(x, "_"))
  capitalized_words <- paste0(toupper(substring(words, 1, 1)), substring(words, 2))
  camelCase <- paste(capitalized_words, collapse = "")
  return(camelCase)
}

# Aplicar la función a los nombres de las columnas
ltem <- ltem %>%
  rename_with(~ sapply(.x, toCamelCase)) %>% 
  rename(
    IDSpecies = IdSpecies,
    A_ord = AOrd,
    B_pen = BPen,
    MaxSizeTL = MaxSizeTl,
    IDReef = IdReef
  )


# Verificar los nombres de las columnas
colnames(ltem)




fish <- ltem %>%
  group_by(Reef) %>%
  mutate(tot_year = n_distinct(Year)) %>%
  ungroup()


fish <- fish |> 
  filter(Label == "PEC") |> 
  filter(tot_year > 6, Year >= 2010) |> 
  filter(Region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) |> 
  filter(Family %in% c("Acanthuridae", "Girellidae", "Kyphosidae", "Pomacentridae", "Scaridae")) |> 
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen = as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size = as.numeric(Size),
    Area = as.numeric(Area),
    Month = as.numeric(Month),
    Biomass = (Quantity * A_ord * (Size^B_pen)) / (Area * 100) # Fórmula para calcular la biomasa (ton/ha)
  ) |> 
  mutate(Biomass = as.numeric(Biomass))

unique(fish$Species)

str(fish)


sst_mean <- sst_data %>%
  rename(Year = year, sstmean= sst_avg)

# Fusionar los datos basándote en las columnas "Reef" y "Year"
newsites <- left_join(fish, sst_mean, by = c("Reef", "Year"))

colnames(newsites)


newsites <- newsites %>%
  select(-Region.y) %>%
  rename(Region = Region.x) 

# saveRDS(newsites, "data/fishdata_sstmean_maxsize_by_year_herbivoros.RDS")



# 2 sites-------

# Cargar librerías --------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)


# Cargar datos
ltem <- readRDS("data/ltem_historic_integrada_2024-05-24.RDS") 

regions <- c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")

# Filtrar por las regiones de interés
filtered_ltem <- ltem %>%
  filter(year >= 2010) |> 
  filter(region %in% regions) %>%
  filter(reef %in% c("CANTIL_MEDIO", "CASITAS", "ISLOTE_CABO_PULMO", "LOBERA_CABO_PULMO", "MORROS_CABO_PULMO",
                     "BURROS", "SAN_JOSE_ANIMAS_NORTE", "SAN_DIEGO_ABNEGADO", "REFUGIO_MORENA", "PUNTA_BOTELLA", "ISLOTE_AGUA_VERDE",
                     "ESPIRITU_SANTO_PARTIDA_NORESTE", "ESPIRITU_SANTO_ISLOTES_NORTE", "ESPIRITU_SANTO_BALLENA", "ESPIRITU_SANTO_PAILEBOTE", "ESPIRITU_SANTO_PUNTA_LOBOS",
                     "CARMEN_ABISMO", "CORONADO_LAJAS", "CORONADO_MONO", "DANZANTE_BIZNAGA", "MONSERRAT_PUNTA_SURESTE"
  ))
# filter(family == "Scaridae")
# filter(family %in% c("Carcharhinidae", "Ginglymostomatidae", "Sphyrnidae"))  

unique(filtered_ltem$protection)

# Definir una nueva columna para la clasificación de protección
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


# Seleccionar las columnas relevantes
site_characteristics <- filtered_ltem %>%
  # filter(region == "Corredor") |> 
  select(year, region, reef, depth, depth2, protection, total_years) %>%
  distinct()

# Estandarizar el número de sitios representativos por región, año y profundidad
standardize_sites <- function(data, max_sites = 5) {
  data %>%
    group_by(region, year, depth2, protection) %>%
    sample_n(min(n(), max_sites)) %>%
    ungroup()
}

# Aplicar la función para estandarizar los sitios
standardized_sites <- standardize_sites(site_characteristics)

sites <- standardized_sites |> 
  distinct(region, reef, depth2, total_years)

# CANTIL_MEDIO
# CASITAS
# ISLOTE_CABO_PULMO
# LOBERA_CABO_PULMO
# MORROS_CABO_PULMO
# 
# BURROS
# SAN_JOSE_ANIMAS_NORTE
# SAN_DIEGO_ABNEGADO
# REFUGIO_MORENA
# PUNTA_BOTELLA
# ISLOTE_AGUA_VERDE
# 
# ESPIRITU_SANTO_PARTIDA_NORESTE
# ESPIRITU_SANTO_ISLOTES_NORTE
# ESPIRITU_SANTO_BALLENA
# ESPIRITU_SANTO_PAILEBOTE
# ESPIRITU_SANTO_PUNTA_LOBOS
# 
# CARMEN_ABISMO
# CORONADO_LAJAS
# CORONADO_MONO
# DANZANTE_BIZNAGA
# MONSERRAT_PUNTA_SURESTE

# Filtrar los datos originales para mantener solo los sitios estandarizados
final_data <- filtered_ltem %>%
  inner_join(standardized_sites, by = c("year", "region", "reef", "depth2", "protection"))

# Verificar la cantidad de sitios por región, profundidad y protección
site_counts <- final_data %>%
  group_by(region, year, depth2, protection) %>%
  summarise(num_sites = n_distinct(reef), .groups = 'drop')

print(site_counts)

# Contar la cantidad de sitios por año y por región
site_counts_by_year_region <- final_data %>%
  group_by(region, year) %>%
  summarise(num_sites = n_distinct(reef), .groups = 'drop')

print(site_counts_by_year_region)

# Guardar el archivo con saveRDS
saveRDS(final_data, "data/standardized_ltem_sites_5_by_region.RDS")
# saveRDS(final_data, "data/standardized_ltem_sites.RDS")



# 3 ---------------------

library(dplyr)
library(janitor)
library(tidyverse)

# data  ----------


ltem <- readRDS("data/ltem_historic_integrada_2024-05-24.RDS")



fish <- final_data |> 
  filter(label == "PEC",
         region == "Corredor") |> 
  mutate(
    a_ord = as.numeric(a_ord),
    b_pen = as.numeric(b_pen),
    quantity = as.numeric(quantity),
    size = as.numeric(size),
    area = as.numeric(area),
    month = as.numeric(month),
    biomass = (quantity * a_ord * (size^b_pen)) / (area * 100) # Fórmula para calcular la biomasa (ton/ha)
  ) |> 
  mutate(biomass = as.numeric(biomass))



# Tendencias tallas de peces ----------

fish <- fish %>%
  group_by(reef) %>%
  mutate(tot_year = n_distinct(year)) %>%
  ungroup()

# Paso 2: Crear la tabla con los filtros y cálculos necesarios
datatable <- fish %>%
  filter(label == "PEC") %>%
  # filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>%
  # filter(tot_year > 6, year >= 2010) %>%
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) %>%
  group_by(year, region, reef, depth2, transect, species) %>%
  summarise(size = mean(size, na.rm = TRUE), .groups = 'drop') %>%
  # group_by(year, species, region) %>%
  # summarise(size = mean(size), .groups = 'drop') %>%
  collect()


fish %>%
  ggplot(aes(x = year, y = quantity)) +
  geom_point() +
  # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  # facet_grid( ~ region)
  facet_grid(region ~ species)


datatable |> 
  ggplot(aes(x = year, y = size)) +
  # geom_line() +
  geom_point() +
  labs(
    title = " ",
    x = "Año",
    y = "Size"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave("herb/size_herbivoros_plot.png", width = 12, height = 8, dpi=1000)


# Plotting with corrected column name
datatable |> 
  group_by(year, species, region) %>%
  summarise(size = mean(size), .groups = 'drop') %>%
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

# ggsave("herb/size_herbivoros_smooth.png", width = 12, height = 8, dpi=1000)


herbivoro <- fish |>
  # filter(diet.x == "Herbivoro") |>
  filter(family %in% c("Acanthuridae", "Girellidae", "Kyphosidae", "Pomacentridae", "Scaridae"))
#   filter(species %in% c("Acanthurus nigricans",
# "Acanthurus triostegus",
# "Acanthurus xanthopterus",
# "Prionurus laticlavius",
# "Girella simplicida",
# "Microspathodon dorsalis",
# "Stegastes flavilatus",
# "Stegastes rectifraenum",
# "Scarus compressus",
# "Scarus ghobban",
# "Scarus perrico",
# "Scarus rubroviolaceus",
# "Kyphosus vaigiensis" ,
# "Kyphosus azureus",
# "Kyphosus elegans",
# "Kyphosus ocyurus"
# ))


unique(herbivoro$species)

# Agrupar los datos por año y especie, y calcular la talla promedio de los peces para cada combinación
size_trends_species <- fish %>%
  filter(commercial == "yes") |> 
  group_by(year, species) %>%
  summarize(avg_size = mean(size, na.rm = TRUE))

# Crear el gráfico de tendencias con facetas por especie
ggplot(size_trends_species, aes(x = year, y = avg_size, group = species)) +
  geom_line() +
  geom_point() +
  labs(
    title = " ",
    x = "Año",
    y = "Talla Promedio de Peces"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")


# Guardar el gráfico
# ggsave("data/size_trends_species.png", width = 12, height = 8, dpi = 300)



ggplot(size_trends_species, aes(x = year, y = avg_size, group = species)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Agregar líneas de regresión
  labs(
    title = " ",
    x = "Año",
    y = "Talla Promedio"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")  # Eliminar la leyenda


# Biomasa  ------------

annual_biomass <- fish %>%
  filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>%
  filter(tot_year > 6, year >= 2010) %>%
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) %>%
  group_by(year, region, reef, depth2, transect, species) |>  # Agrupamos los datos
  summarise(biomass = sum(biomass, na.rm = TRUE)) |> 
  group_by(year, region, species) |> 
  summarise(avg_biomass = mean(biomass, na.rm = TRUE))

ggplot(annual_biomass, aes(x = year, y = avg_biomass)) +
  geom_line() +
  geom_point() +
  labs(
    title = " ",
    x = "Año",
    y = "Biomasa promedio (ton/ha)"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave("herb/biomass_herbivoros.png", width = 12, height = 8, dpi=1000)



(p <- fish |> 
    filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>%
    filter(tot_year > 6, year >= 2010) %>%
    filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                          "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
    mutate(year = factor(year),  # Factorizamos el año
           region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |> 
    group_by(year, region, island, reef, transect, depth2, species) |>  # Agrupamos los datos
    summarise(biomass = sum(biomass, na.rm = TRUE)) |>  # Sumamos la biomasa
    group_by(year, region, species) |>  # Agrupamos por año, región y grupo trófico
    summarise(biomass = mean(biomass, na.rm = TRUE)) |>  # Calculamos el promedio de la biomasa
    ggplot(aes(x = year , y = biomass, fill = species)) +  # Creamos el gráfico
    geom_bar(position = "stack", stat = "identity", col = "black") +  # Agregamos las barras
    # geom_point()+
    scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e", "#f4a582", "#92c5de"),  guide = "none") +  # Personalizamos los colores
    # scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"),  guide = "none") +  # Personalizamos los colores
    # facet_wrap(~region) +  # Dividimos por región
    facet_grid(region ~ species) +
    scale_x_discrete(breaks = c("2010",  "2013", "2016", "2019", "2022")) +
    labs(x = "Año", y = "Biomasa (ton/ha)", fill = "Species") +  # Etiquetas
    theme_classic())


# ggsave("herb/biomass_herbivoros_histo.png", width = 12, height = 8, dpi=1000)

# Aumentan o disminuyen de manera significativa?------------


# Visualización de datos
ggplot(datatable, aes(x = year, y = size)) +
  geom_point() +
  labs(title = " ",
       x = "Año",
       y = "Talla") +
  theme_minimal()

# Análisis de tendencia temporal
model <- lm(size ~ year, data = datatable)

# Resumen del modelo
summary(model)

# Prueba de hipótesis sobre la pendiente
# Hipótesis nula: No hay cambio significativo en las tallas a lo largo del tiempo
# Hipótesis alternativa: Hay un cambio significativo en las tallas a lo largo del tiempo
coef_test <- coef(summary(model))["year", "Pr(>|t|)"]
p_value <- coef_test

# Imprimir el valor p de la prueba de hipótesis
print(p_value)


library(broom)

# Ajustar modelos lineales para cada especie
size_trends_models <- datatable %>%
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  group_by(species) %>%
  do(model = lm(size ~ year, data = .))

# Extraer los coeficientes del modelo (pendientes) y su significancia
model_coefficients <- size_trends_models %>%
  tidy(model) %>%
  filter(term == "year")

# Ver los coeficientes y sus p-valores
print(model_coefficients)

# Ver los coeficientes y sus p-valores
model_coefficients %>%
  select(species, estimate, std.error, statistic, p.value)

# Filtrar las especies con pendientes significativas
significant_trends <- model_coefficients %>%
  filter(p.value < 0.05) %>%
  mutate(trend = ifelse(estimate > 0, "Aumenta", "Disminuye"))

# Ver los resultados
significant_trends

# 4 ----------------
library(dplyr)
library(tidyr)
library(ggplot2)

# cargar datos-------

sspdata <- readRDS("data/fish_datagr_prod-by-species-herbivoros2.RDS") 


fish <- sspdata |> 
  janitor::clean_names() |> 
  filter(label == "PEC") |> 
  filter(year >= 2010) |> 
  filter(reef %in% c("CANTIL_MEDIO", "CASITAS", "ISLOTE_CABO_PULMO", "LOBERA_CABO_PULMO", "MORROS_CABO_PULMO", #"SAN_JOSE_ANIMAS_NORTE", 
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
    biomass = (quantity * a_ord * (size^b_pen)) / (area * 100) # Fórmula para calcular la biomasa (ton/ha)
  ) |> 
  mutate(biomass = as.numeric(biomass))


unique(sspdata$Species)

# Sumar para cada transecto ---------
data_prod_brut <- sspdata |> 
  filter(Species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  #Sum for each transect
  group_by(Year,Region, Reef,Depth2,Transect) %>%
  mutate(
    Biom = sum(Biom)/Area,# (kg ha^−1) # porqué 500?
    Prod = sum(Prod)/Area,#g d^−1 ha^−1
    Productivity = (Prod/Biom)*100) %>%
  ungroup() |> 
  #Mean for each site
  group_by(Year, Reef, Species) %>%
  mutate(Biom = mean(Biom),
         Prod = mean(Prod),
         Productivity = mean(Productivity)) %>% 
  ungroup() %>%
  #Transforming data
  mutate(
    log10ProdB = Productivity,# % per day
    log10Biom = log10(Biom+1), #(g m -2)
    log10Prod = log10(Prod+1), # (g m-2 d-1)
    Latitude = as.numeric(as.character(Latitude)),
    Longitude = as.numeric(as.character(Longitude)))


print(paste("Minimum biomass:",min(data_prod_brut$Biom)))
print(paste("Maximum biomass:", max(data_prod_brut$Biom)))
print(paste("Minimum production:",min(data_prod_brut$Prod)))
print(paste("Maximum production:", max(data_prod_brut$Prod)))
print(paste("Minimum turnover:",min(data_prod_brut$Productivity)))
print(paste("Maximum turnover:", max(data_prod_brut$Productivity)))

# data_management---------------
biom75 = quantile(data_prod_brut$log10Biom,0.95)
biom25 = quantile(data_prod_brut$log10Biom,0.25)
prod75 = quantile(data_prod_brut$log10ProdB,0.75)
prod25 = quantile(data_prod_brut$log10ProdB,0.25)
max_biom = max(data_prod_brut$log10Biom)


#Diving data into 3 classes for each biomass/productivity relationship
management = data_prod_brut %>% 
  
  mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                        ifelse(log10ProdB > prod75,"partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
  mutate(Class = as.factor(Class))

range(management$Productivity)
range(management$Biom)
range(management$Prod)


unique(management$Class)


sites <- data_prod_brut |> 
  janitor::clean_names()


sp <- sites %>%
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |> 
  # group_by(year, region, island, reef, depth2, transect, species) |> 
  # summarise(prod = mean(prod), productivity = mean(productivity))|>  
  group_by(year, region) |> 
  summarise(prod = mean(prod), productivity = mean(productivity))


ggplot(sp, aes(x = year, y = prod, fill = year)) +
  # geom_boxplot() +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = " ", y = "Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  facet_wrap(~ region, scales = "free", ncol = 1)  +
  theme(legend.position = "none")+
  ylim(0, max(sp$prod, na.rm = TRUE)) 

# ggsave("herb/prod_region_herbivoros.png", width = 12, height = 8, dpi=1000)

ggplot(sp, aes(x = year, y = productivity, fill = year)) +
  # geom_boxplot() +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = "Year", y = "Turnover (P/B × 100 % per day)") +
  theme_classic() +
  facet_wrap(~ region, scales = "free", ncol = 1)  +
  theme(legend.position = "none")+
  ylim(0, max(sp$productivity, na.rm = TRUE)) 

# ggsave("herb/turnover_herbivoros.png", width = 12, height = 8, dpi=1000)



# 3. Obtener los valores máximos y mínimos de la productividad promedio para establecer los límites del eje y
y_min <- min(sp$prod)
y_max <- max(sp$prod)

# 4. Graficar las tendencias de productividad para los arrecifes más productivos
ggplot(sp, aes(x = year, y = prod, color = region)) +
  geom_line() +
  geom_point()+
  scale_color_viridis_d(option = "plasma") +  # Ajustar colores según preferencia
  theme_minimal() +
  labs(x = "Año", y = "Productivity (g d^−1 ha^−1)", title = " ") +
  theme(legend.position = "none") +  # Suprimir leyenda si hay muchos arrecifes
  facet_wrap(~ region, scales = "free", ncol = 2) +  # Ajustar escalas libres
  scale_x_continuous(breaks = seq(min(sp$year), max(sp$year), by = 8)) +
  ylim(y_min, y_max)  # Establecer límites del eje y



# ggsave("herb/tendencia_fishprod_reefs_prod.png", width = 12, height = 8, dpi=1000)

# especies --------
spp <- sites %>%
  filter(species %in% c( "Nicholsina denticulata", "Scarus compressus",
                         "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |> 
  # group_by(year, region, island, reef, depth2, transect, species) |>
  # summarise(prod = mean(prod))|>
  group_by(year, species) |> 
  summarise(prod = mean(prod))

y_max <- max(spp$prod, na.rm = TRUE)

# Graficar las tendencias de productividad para las 5 especies más productivas a lo largo de los años
ggplot(spp, aes(x = year, y = prod, color = species)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "plasma") +  # Ajustar colores según preferencia
  theme_minimal() +
  facet_wrap(~ species, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y para cada faceta
  ylim(0, y_max) +  # Establecer el mismo límite superior en el eje Y para todos los gráficos
  labs(x = "Año", y = "Productividad", title = " ") +
  theme(legend.position = " ") 

# ggsave("herb/tendencia_fishprod_especies_herb.png", width = 12, height = 8, dpi=1000)


ggplot(spp, aes(x = year, y = prod, color = species)) +
  geom_line() +
  geom_point() +
  labs(title = " ",
       x = "Año",
       y = "Productivity (g d^−1 ha^−1)",
       color = " ") +  # Etiqueta de la leyenda
  ylim(0, y_max) +  # Establecer límites en el eje Y
  facet_wrap(~ species, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y
  theme_minimal() +
  theme(legend.position = " ")  


# ggsave("herb/tendencia_fishprod_especies_herbivoros.png", width = 12, height = 8, dpi=1000)


prod_year <- sites |>
  filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) |> 
  filter(species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |>  
  group_by(year, region, island, reef, transect, depth2, species) |>  # Agrupamos los datos
  summarise(prod = sum(prod, na.rm = TRUE)) |> 
  group_by(year, region, species) |> 
  summarise(prod = mean(prod, na.rm = TRUE))

ggplot(prod_year, aes(x = year, y = prod)) +
  geom_line() +
  geom_point() +
  labs(
    title = " ",
    x = "Año",
    y = "Productivity (g d^−1 ha^−1)"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave("herb/preod_region_spp_herbivoros.png", width = 12, height = 8, dpi=1000)



# Preparar datos de productividad ----
prod_year <- sites %>%
  filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>%
  filter(species %in% c( "Nicholsina denticulata", "Scarus compressus",
                         "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) %>%
  # group_by(year, region, island, reef, transect, depth2, species) %>%
  # summarise(prod = sum(prod, na.rm = TRUE)) %>%
  group_by(year, region, reef, species) %>%
  summarise(prod = mean(prod, na.rm = TRUE))

# Preparar datos de biomasa -------
annual_biomass <- fish %>%
  filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) %>%
  filter(species %in% c( "Nicholsina denticulata", "Scarus compressus",
                         "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) %>%
  group_by(year, region, island, reef, transect, depth2, species) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE)) %>%
  group_by(year, region, reef, species) %>%
  summarise(avg_biomass = mean(biomass, na.rm = TRUE))

# Combinar datos ------
combined_data <- merge(prod_year, annual_biomass, by = c("year", "region", "species"))

# Crear gráfica combinada
ggplot(combined_data) +
  geom_line(aes(x = year, y = prod, color = "Productivity (g d^−1 ha^−1)")) +
  geom_point(aes(x = year, y = prod, color = "Productivity (g d^−1 ha^−1)")) +
  geom_line(aes(x = year, y = avg_biomass, color = "Biomasa (ton/ha)")) +
  geom_point(aes(x = year, y = avg_biomass, color = "Biomasa (ton/ha)")) +
  labs(
    title = " ",
    x = "Año",
    y = " "
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("Productivity (g d^−1 ha^−1)" = "blue", "Biomasa (ton/ha)" = "red"))

# Guardar gráfica
# ggsave("herb/productivity_biomass_comparison.png", width = 12, height = 8, dpi = 300)



ggplot(combined_data, aes(x = avg_biomass, y = prod)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = region)) +
  labs(
    title = " ",
    x = "Biomasa (ton/ha)",
    y = "Productividad (g d^−1 ha^−1)"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("Loreto" = "#b57560", "Corredor" = "#258f4e", "La Paz" = "#a30808", "Cabo Pulmo" = "#114a06"))

# Guardar gráfica
# ggsave("herb/productivity_biomass_comparison_relationship.png", width = 12, height = 8, dpi = 300)


regional_data <- merge(prod_year, annual_biomass, by = c("year", "region", "reef", "species")) |> 
  group_by(year, region, reef, species) %>%
  summarise(avg_biomass = mean(avg_biomass, na.rm = TRUE), prod = mean(prod))



# Gráfico de dispersión regional y local -------
ggplot(regional_data, aes(x = avg_biomass, y = prod, color = region)) +
  geom_point() +
  labs(
    title = " ",
    x = "Biomass",
    y = "Productivity",
    color = "Region"
  ) +
  theme_minimal()


local_data <- merge(prod_year, annual_biomass, by = c("year", "region","reef", "species")) |> 
  group_by(year, region, reef, species) %>%
  summarise(avg_biomass = mean(avg_biomass, na.rm = TRUE), prod = mean(prod))


ggplot(local_data, aes(x = avg_biomass, y = prod, color = region)) +
  geom_point() +
  labs(
    x = "Biomass",
    y = "Productivity",
    color = " "
  ) +
  theme_minimal()
# theme(legend.position = "none")  

# ggsave("herb/productivity_biomass_comparison_relationships_region.png", width = 12, height = 8, dpi = 300)



# Curvas de densidad para coeficientes del modelo (pendientes)
ggplot(local_data, aes(x = prod/avg_biomass)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Density Curves of Model Coefficients",
    x = "Productivity/Biomass",
    y = "Density"
  ) +
  theme_minimal()

# rangos de tallas  --------
# Filtrar las especies específicas y calcular las tallas mínimas y máximas
size_ranges <- sites %>%
  filter(species %in% c("Nicholsina denticulata", "Scarus compressus", "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) %>%
  group_by(year, species) %>%
  summarise(min_size = min(size), max_size = max(size), size = mean(size), prod = mean(prod), productivity = mean(productivity))


# Graficar
y_max <- max(size_ranges$prod, na.rm = TRUE)


ggplot(size_ranges, aes(x = size, y = prod, color = species)) +
  geom_line() +
  geom_point()+ 
  geom_ribbon(data = size_ranges, aes(ymin = 0, ymax = 0, xmin = min_size, xmax = max_size, fill = species), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = " ",
    x = "Longitud Total (cm)",
    y = "Productivity"
  ) +
  facet_wrap(~ species, scales = "free", ncol = 1) +
  # facet_grid(region ~ species) +
  ylim(0, y_max) +
  scale_fill_manual(values = c("Nicholsina denticulata" = "blue", "Scarus compressus" = "red", "Scarus ghobban" = "green", "Scarus perrico" = "purple", "Scarus rubroviolaceus" = "orange")) +
  theme_minimal() +
  theme(axis.text.y = element_text(face= "plain", size=8),
        axis.text.x=element_text(size=10),
        plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        # legend.text = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold.italic"))
# theme(
#   # legend.text = element_text(face = "italic"))
#   legend.position = "none")

# ggsave("herb/productivity_size_comparison_relationships_region_longuitud.png", width = 12, height = 8, dpi = 300)


# Regresion ------

density_data <- sspdata %>%
  filter(species %in% c( "Nicholsina denticulata", "Scarus compressus",
                         "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  group_by(year, region, reef, depth2, transect, species) %>%
  summarise(density = sum(quantity)/area, .groups = 'drop') %>%
  group_by(year, species) %>%
  summarise(density = mean(density)/area, .groups = 'drop') 

# Ajustar modelos de regresión lineal
models <- list()
for (fish_species in unique(density_data$species)) {
  model <- lm(density ~ year, data = density_data[density_data$species == fish_species, ])
  models[[fish_species]] <- model
}

# Extraer la tasa de cambio
tasa_cambio <- sapply(models, function(model) coef(model)[["year"]])

# Mostrar las tasas de cambio
tasa_cambio


# Preparar los datos de densidad ----------

density_data <- sspdata %>%
  filter(species %in% c("Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) %>%
  group_by(year, region, reef, depth2, transect, species) %>%
  reframe(density = sum(quantity)/area) %>%
  group_by(year, species) %>%
  reframe(density = mean(density))

# Ajustar modelos de regresión lineal y obtener predicciones
models <- density_data %>%
  group_by(species) %>%
  do(model = lm(density ~ year, data = .))

predictions <- density_data %>%
  group_by(species) %>%
  mutate(prediction = predict(models$model[[which(unique(species) == species)]], newdata = .))

# Fusionar las predicciones con los datos originales
density_data <- density_data %>%
  left_join(predictions %>% select(year, species, prediction), by = c("year", "species"))

# Graficar los datos y los modelos de regresión
ggplot(density_data, aes(x = year, y = density, color = species)) +
  geom_point() +
  geom_line(aes(y = prediction), linetype = "dashed") +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    title = " ",
    x = "Año",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d(option = "plasma")


#  6-----------------

# Cargar librerías --------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)


# Cargar datos

# ltem <- readRDS("data/ltem_historic_integrada_2024-05-24.RDS")
# data5 <- readRDS("data/standardized_ltem_sites_5_by_region.RDS")
# data <- readRDS("data/standardized_ltem_sites.RDS")
sspdata <- readRDS("data/fish_datagr_prod-by-species-herbivoros2.RDS") 

# Sumar para cada transecto ---------
data_prod_brut <- sspdata |> 
  filter(Species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  #Sum for each transect
  group_by(Year,Region, Reef,Depth2,Transect) %>%
  mutate(
    Biom = sum(Biom)/Area,# (kg ha^−1) # porqué 500?
    Prod = sum(Prod)/Area,#g d^−1 ha^−1
    Productivity = (Prod/Biom)*100) %>%
  ungroup() |> 
  #Mean for each site
  group_by(Year, Region, Reef, Transect, Species) %>%
  mutate(Biom = mean(Biom),
         Prod = mean(Prod),
         Productivity = mean(Productivity)) %>% 
  ungroup() %>%
  #Transforming data
  mutate(
    log10ProdB = Productivity,# % per day
    log10Biom = log10(Biom+1), #(g m -2)
    log10Prod = log10(Prod+1), # (g m-2 d-1)
    Latitude = as.numeric(as.character(Latitude)),
    Longitude = as.numeric(as.character(Longitude)))


print(paste("Minimum biomass:",min(data_prod_brut$Biom)))
print(paste("Maximum biomass:", max(data_prod_brut$Biom)))
print(paste("Minimum production:",min(data_prod_brut$Prod)))
print(paste("Maximum production:", max(data_prod_brut$Prod)))
print(paste("Minimum turnover:",min(data_prod_brut$Productivity)))
print(paste("Maximum turnover:", max(data_prod_brut$Productivity)))

# data_management---------------
biom75 = quantile(data_prod_brut$log10Biom,0.95)
biom25 = quantile(data_prod_brut$log10Biom,0.25)
prod75 = quantile(data_prod_brut$log10ProdB,0.75)
prod25 = quantile(data_prod_brut$log10ProdB,0.25)
max_biom = max(data_prod_brut$log10Biom)


#Diving data into 3 classes for each biomass/productivity relationship
management = data_prod_brut %>% 
  
  mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                        ifelse(log10ProdB > prod75,"partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
  mutate(Class = as.factor(Class))

range(management$Productivity)
range(management$Biom)
range(management$Prod)


unique(management$Class)


sites <- data_prod_brut |> 
  janitor::clean_names()

fish <- sites |> 
  filter(label == "PEC") |> 
  filter(year >= 2010) |> 
  filter(reef %in% c("CANTIL_MEDIO", "CASITAS", "ISLOTE_CABO_PULMO", "LOBERA_CABO_PULMO", "MORROS_CABO_PULMO",
                     "BURROS", "SAN_DIEGO_ABNEGADO", "REFUGIO_MORENA", "PUNTA_BOTELLA", "ISLOTE_AGUA_VERDE",
                     "ESPIRITU_SANTO_PARTIDA_NORESTE", "ESPIRITU_SANTO_ISLOTES_NORTE", "ESPIRITU_SANTO_BALLENA", 
                     "ESPIRITU_SANTO_PAILEBOTE", "ESPIRITU_SANTO_PUNTA_LOBOS", #"SAN_JOSE_ANIMAS_NORTE",
                     "CARMEN_ABISMO", "CORONADO_LAJAS", "CORONADO_MONO", "DANZANTE_BIZNAGA", "MONSERRAT_PUNTA_SURESTE"
  )) |> 
  # filter(family %in% c("Acanthuridae", "Girellidae", "Kyphosidae", "Pomacentridae", "Scaridae")) |> 
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
    biomass = (quantity * a_ord * (size^b_pen)) / (area * 100) # Fórmula para calcular la biomasa (ton/ha)
  ) |> 
  mutate(biomass = as.numeric(biomass))

str(fish)



community_data <- fish |> 
  # filter(region == "Corredor") |> 
  group_by(year, mpa, protection_level, region, reef, habitat, transect, depth2, species) |>
  summarise(abundance = sum(quantity, na.rm = TRUE),
            richness = n_distinct(species),
            biomass = sum(biomass, na.rm = TRUE),
            prod = mean(prod), 
            productivity = mean(productivity),
            size = mean(size, na.rm = TRUE))|> 
  group_by(year, region, reef, transect, species) |>
  summarise(abundance = mean(abundance),
            richness = sum(richness),
            avg_biomass = mean(biomass, na.rm = TRUE),
            prod = mean(prod), 
            productivity = mean(productivity),
            size = mean(size))
  
unique(community_data$reef)

# Tallas ---------

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

community_data |> 
  group_by(year, region, species) |>
  summarise(size = mean(size), .groups = 'drop') |>
  ggplot( aes(x = year, y = size)) +
  geom_line(aes(color = species), alpha = 0.3) +  # Líneas delgadas para tendencias aleatorias
  geom_smooth(aes(color = species), method = "loess", span = 0.75, se = FALSE, size = 1.2) +  # Ajuste local con span ajustado
  geom_point(aes(color = species), alpha = 0.6) +  # Puntos de colores para datos crudos
  # geom_vline(xintercept = bleaching_event_year, color = "red", linetype = "dashed") +  # Línea vertical para el evento de blanqueamiento
  # scale_color_manual(values = c("cryptobenthic" = "mediumblue", "damselfishes" = "lightblue", "noncryptic" = "darkblue")) +  # Colores de las especies
  labs(x = "Year", y = "size", title = " ") +
  theme_bw() +  # Estilo del gráfico
  # facet_grid(region ~ species) +
  theme(axis.text.x = element_text(color = "red")) #+  # Color del texto del eje X
# theme(legend.position = "none")


# ggsave("herb/size_herbivoros_smooth_.png", width = 12, height = 8, dpi=1000)

# Abundancia --------------

# Calcular el logaritmo de la abundancia
community_data$log_abundance <- log(community_data$abundance)

# Definir el año del blanqueamiento de coral
# bleaching_event_year <- 1998

# Crear el gráfico de abundancia logarítmica a lo largo del tiempo
community_data |> 
  # group_by(year, region, species) |>
  # summarise(abundance = mean(abundance), .groups = 'drop') |>
  ggplot( aes(x = year, y = abundance)) +
  geom_line(aes(color = species), alpha = 0.3) +  # Líneas delgadas para tendencias aleatorias
  geom_smooth(aes(color = species), method = "loess", span = 0.75, se = FALSE, size = 1.2) +  # Ajuste local con span ajustado
  geom_point(aes(color = species), alpha = 0.6) +  # Puntos de colores para datos crudos
  # geom_vline(xintercept = bleaching_event_year, color = "red", linetype = "dashed") +  # Línea vertical para el evento de blanqueamiento
  # scale_color_manual(values = c("cryptobenthic" = "mediumblue", "damselfishes" = "lightblue", "noncryptic" = "darkblue")) +  # Colores de las especies
  labs(x = "Year", y = "Abundance", title = " ") +
  theme_bw() +  # Estilo del gráfico
  # facet_grid(region ~ species) +
  theme(axis.text.x = element_text(color = "red")) #+  # Color del texto del eje X
  # theme(legend.position = "none")

# ggsave("herb/abundancia_herbivoros_spp.png", width = 12, height = 8, dpi=1000)

ggplot(community_data, aes(x = year, y = abundance)) +
  geom_smooth(aes(x = year, y = abundance, color = region, group = region), method = "loess", se = FALSE) +
  geom_point(aes(x = year, y = abundance, color = region, group = region), alpha = 0.3) +
  geom_smooth(col = "black") +
  labs(title = " ",
       x = "Year",
       y = "Abundance") +
  theme_classic()
  # facet_grid(region ~ species) +
  # theme(legend.position = "none")

community_data |> 
  group_by(year, region, species) %>%
  summarise(abundance = mean(abundance), .groups = 'drop') |>
  ggplot(aes(x = year, y = abundance)) +
  geom_line()+
  geom_point() +
  # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(
    title = " ",
    x = "Year",
    y =  "abundance"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.position = "none")

# Riqueza --------------


# Calcular la riqueza logarítmica
community_data$log_richness <- log(community_data$richness)

# Crear el gráfico de riqueza logarítmica a lo largo del tiempo

community_data |> 
  group_by(year, region, species) %>%
  summarise(richness = mean(richness), .groups = 'drop') |>
  ggplot( aes(x = year, y = richness)) +
  geom_line(aes(color = species), alpha = 0.3) +  # Líneas delgadas para tendencias aleatorias
  geom_smooth(aes(color = species), method = "loess", span = 0.75, se = FALSE, size = 1.2) +  # Ajuste local con span ajustado
  geom_point(aes(color = species), alpha = 0.6) +  # Puntos de colores para datos crudos
  # geom_vline(xintercept = bleaching_event_year, color = "red", linetype = "dashed") +  # Barras rojas para eventos
  # scale_color_manual(values = c("cryptobenthic" = "mediumblue", "damselfishes" = "lightblue", "noncryptic" = "darkblue")) +  # Colores de las especies
  labs(x = "Year", y = "Richness", title = " ") +
  theme_bw() +  # Estilo del gráfico
  theme(axis.text.x = element_text(color = "red"))


# Guardar el gráfico como una imagen 
# ggsave("herb/riqueza_herbivoros.png", width = 12, height = 8, dpi=1000)

community_data |> 
  group_by(year, region, species) %>%
  summarise(richness = mean(richness), .groups = 'drop') |>
  ggplot(aes(x=factor(year), y=richness)) +
  # geom_point() +
  geom_violin(trim = F) +
  geom_boxplot(width = .1) +
  labs(x = "", y = "Richness", title = "Fish Richness") +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = .5))

# ggsave("herb/riqueza_herbivoros_violin.png", width = 12, height = 8, dpi=1000)

# Biomasa -----------------


# Calcular la biomasa logarítmica
community_data$log_avg_biomass <- log(community_data$avg_biomass)

# Crear el gráfico de biomasa logarítmica a lo largo del tiempo

community_data |> 
  group_by(year, region, species) %>%
  summarise(avg_biomass = mean(avg_biomass), .groups = 'drop') |>
  ggplot( aes(x = year, y = avg_biomass)) +
  geom_line(aes(color = species), alpha = 0.3) +  # Líneas delgadas para tendencias aleatorias
  geom_smooth(aes(color = species), method = "loess", span = 0.75, se = FALSE, size = 1.2) +  # Ajuste local con span ajustado
  geom_point(aes(color = species), alpha = 0.6) +  # Puntos de colores para datos crudos
  # geom_vline(xintercept = bleaching_event_year, color = "red", linetype = "dashed") +  # Barras rojas para eventos
  # scale_color_manual(values = c("cryptobenthic" = "mediumblue", "damselfishes" = "lightblue", "noncryptic" = "darkblue")) +  # Colores de las especies
  labs(x = "Year", y = "Biomass (ton/ha)", title = " ") +
  theme_bw() +  # Estilo del gráfico
  # facet_grid(region ~ species) +
  theme(axis.text.x = element_text(color = "red"))# +  # Color del texto del eje X
  # theme(legend.position = "none")


community_data |> 
  group_by(year, region, species) %>%
  summarise(avg_biomass = mean(avg_biomass), .groups = 'drop') |>
  ggplot( aes(x = year, y = avg_biomass)) +
  geom_line() +
  geom_point() +
  labs(
    title = " ",
    x = "Año",
    y = "Biomasa promedio (ton/ha)"
  ) +
  facet_grid(region ~ species) +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave("herb/biomasa_herbivoros_spp.png", width = 12, height = 8, dpi=1000)



(p <- fish |> 
    mutate(year = factor(year),  # Factorizamos el año
           region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |> 
    group_by(year, region, island, reef, transect, depth2, species) |>  # Agrupamos los datos
    summarise(biomass = sum(biomass, na.rm = TRUE)) |>  # Sumamos la biomasa
    group_by(year, region, species) |>  # Agrupamos por año, región y grupo trófico
    summarise(biomass = mean(biomass, na.rm = TRUE)) |>  # Calculamos el promedio de la biomasa
    ggplot(aes(x = year , y = biomass, fill = species)) +  # Creamos el gráfico
    geom_bar(position = "stack", stat = "identity", col = "black") +  # Agregamos las barras
    # geom_point()+
    scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e", "#f4a582", "#92c5de"),  guide = "none") +  # Personalizamos los colores
    # scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"),  guide = "none") +  # Personalizamos los colores
    # facet_wrap(~region) +  # Dividimos por región
    facet_grid(region ~ species) +
    scale_x_discrete(breaks = c("2010",  "2013", "2016", "2019", "2022")) +
    labs(x = "Año", y = "Biomasa (ton/ha)", fill = "Species") +  # Etiquetas
    theme_classic())


# ggsave("herb/biomass_herbivoros_histo.png", width = 12, height = 8, dpi=1000)



# Productividad -----------

y_max <- max(community_data$prod, na.rm = TRUE)


community_data |> 
  group_by(year,  species) %>%
  summarise(prod = mean(prod), .groups = 'drop') |>
  ggplot(aes(x = year, y = prod, color = species)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "plasma") +  # Ajustar colores según preferencia
  theme_minimal() +
  facet_wrap(~ species, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y para cada faceta
  ylim(0, y_max) +  # Establecer el mismo límite superior en el eje Y para todos los gráficos
  labs(x = "Año", y = "Productivity (g d^−1 ha^−1)", title = " ") +
  theme(legend.position = " ") 


# Gráfica para la productividad de peces del arrecife

community_data |> 
  group_by(year, species) %>%
  summarise(prod = mean(prod), .groups = 'drop') |>
  ggplot(aes(x = year, y = prod, color = species)) +
  # ggplot( aes(x = year, y = prod)) +
  geom_line(color = "black", alpha = 0.3) +  # Líneas delgadas para tendencias aleatorias
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +  # Líneas gruesas para tendencias medianas
  geom_point(alpha = 0.6) +  # Puntos de colores para datos crudos
  # geom_vline(xintercept = 1998, color = "red", linetype = "dashed") +  # Línea vertical para el evento de blanqueamiento
  scale_y_log10() +  # Escala logarítmica en el eje Y
  # scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +  # Personaliza las marcas del eje X según tus datos
  labs(x = "Year", y = "Productivity (g d^−1 ha^−1)", title = " ") +
  theme_bw() +  # Estilo del gráfico
  theme(axis.text.x = element_text(color = c("red", "green", "yellow", "blue"))) #+  # Color del texto del eje X según los eventos de estrés
  # facet_grid(region ~ species) +
  # theme(legend.position = "none")

# ggsave("herb/prod_herbivoros_spp_.png", width = 12, height = 8, dpi=1000)

# Turnover -----------

community_data |> 
  group_by(year, region, species) %>%
  summarise(productivity = mean(productivity), .groups = 'drop') |>
  ggplot(aes(x = year, y = productivity, color = species)) +
  geom_line(color = "black", alpha = 0.3) +  # Líneas delgadas para tendencias aleatorias
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +  # Líneas gruesas para tendencias medianas
  geom_point(alpha = 0.6) +  # Puntos de colores para datos crudos
  # geom_vline(xintercept = 1998, color = "red", linetype = "dashed") +  # Línea vertical para el evento de blanqueamiento
  scale_y_log10() +  # Escala logarítmica en el eje Y
  # scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +  # Personaliza las marcas del eje X según tus datos
  labs(x = "Year", y = "Turnover (P/B × 100 % per day)", title = " ") +
  theme_bw() +  # Estilo del gráfico
  theme(axis.text.x = element_text(color = c("red", "green", "yellow", "blue"))) +  # Color del texto del eje X según los eventos de estrés
  facet_grid(region ~ species) +
  theme(legend.position = "none")


# ggsave("herb/turnover_herbivoros_spp_.png", width = 12, height = 8, dpi=1000)
