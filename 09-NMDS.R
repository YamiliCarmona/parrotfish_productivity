# Non-Metric Multidimensional Scaling (NMDS) 

# Load libraries

library(vegan) 
library(tidyverse)


# Load data ------------------------------------------------------------

sspdata <- readRDS("data/fish_datagr_prod-by-species-herbivoros2.RDS") |> 
  janitor::clean_names()


fish <- sspdata |> 
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


set.seed(666)

# Crear la matriz de comunidad
community_matrix <- fish %>%
  group_by(year, region, reef, transect, species) %>%
  summarise(quantity = sum(quantity), .groups = 'drop') %>%
  group_by(region, reef, species) %>%
  summarise(quantity = round(mean(quantity), 1), .groups = 'drop') %>%
  pivot_wider(names_from = "species", values_from = "quantity") %>%
  as.data.frame() # Convertir a data frame para asegurar la manipulación de filas y columnas

# Nombrar las filas como los arrecifes
rownames(community_matrix) <- community_matrix$reef
community_matrix$reef <- NULL # Eliminar la columna reef

# Ausencias verdaderas  
community_matrix[is.na(community_matrix)] <- 0 

# Calcular el NMDS
rev_NMDS <- metaMDS(community_matrix [,2:7], # Matriz de arrecife por especie
                    k = 2, # A estas dimensiones escalamos el NMDS (Reduced dimensions)
                    trymax = 1000) # Número de iteraciones que se van a realizar (cambiar si es necesario)   

# Plotear el stressplot
stressplot(rev_NMDS)

# Plotear los resultados de NMDS
plot(rev_NMDS)

# Extraer la información de 'region'
region <- community_matrix$region

# Plot personalizado del NMDS
ordiplot(rev_NMDS, type = "n")
orditorp(rev_NMDS, display = "species", col = "red", air = 2, pch = "+")
orditorp(rev_NMDS, display = "sites", col = c(rep("firebrick", 2), rep("darkgreen", 1), rep("blue", 13), rep("orange", 14)), air = 3, cex = 0.3)
ordihull(rev_NMDS, groups = region, draw = "polygon", label = TRUE, fill = "white", col = c("firebrick", "darkgreen", "lightblue", "gold"), alpha = 0.25)
# ordilabel(rev_NMDS, labels = region, fill = "white", border = NA)



print(unique(region))

# Convertir todos los valores a numéricos
community_matrix[] <- lapply(community_matrix, as.numeric)

# Calcular diversidad usando índice de Shannon
diversity_indices <- diversity(as.matrix(community_matrix), index = "shannon")
print(diversity_indices)


