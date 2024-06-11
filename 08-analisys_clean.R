# Load libraries --------------------------------------------------------

library(tidyverse)
library(readxl)
library(brms) # to run linear models
library(rstan) # to assess model diagnostics
library(DHARMa) # to assess model dianostics
library(tidybayes) # clean and plot bayesian models
library(patchwork) # stitching plots together
library(vegan) # run multivariate community analyses
library(ggvegan) # make vegan plots easier


# load data ------
# 06-community_data 



community_data <- fish |> 
  group_by(year, mpa, protection_level, region, reef, habitat, transect, depth2, species) |>
  summarise(abundance = sum(quantity, na.rm = TRUE),
            richness = n_distinct(species),
            biomass = sum(biomass, na.rm = TRUE),
            prod = mean(prod), 
            productivity = mean(productivity))|> 
  group_by(year, region, reef, transect) |>
  summarise(total_abund = mean(abundance),
            total_richness = sum(richness),
            total_bm = mean(biomass, na.rm = TRUE),
            total_prod = mean(prod), 
            productivity = mean(productivity))


# Entire community analyses --------------------------------------------------------------
# Productivity ---------
prod_gam <- brm(total_prod ~ s(year),
      data = community_data,
      family = Gamma(link = 'log'),
      iter = 10000,
      warmup = 2000,
      thin = 5,
      seed = 123,
      save_pars = save_pars(all = TRUE),
      control = list(adapt_delta = 0.999))


# saveRDS(prod_gam, 'data/Productivity_gam.rds')
# prod_gam <- readRDS('data/Productivity_gam.rds')

# Gráfico del término suave
plot(prod_gam)

# Predicciones del modelo
predicciones <- fitted(prod_gam)

# Gráfico de las predicciones vs datos originales
plot(community_data$year, community_data$total_prod, main = "GAM Predictions", xlab = "Year", ylab = "Total Production")
lines(community_data$year, predicciones, col = "red")

# check model diagnostics
# within chain autocorrelation
stan_ac(prod_gam$fit, pars = get_variables(prod_gam)[1:5]) # nice

# traceplots
stan_trace(prod_gam$fit, pars = get_variables(prod_gam)[1:5]) # nice

# ESS
stan_ess(prod_gam$fit, pars = get_variables(prod_gam)[1:5]) 

summary(prod_gam)

# Check simulated residuals
prod_gam_pred <-posterior_predict(prod_gam, ndraws = 250, summary = FALSE)

prod_gam_resid <- createDHARMa(simulatedResponse = t(prod_gam_pred),
               observedResponse = prod_gam$data$total_prod,
               fittedPredictedResponse = apply(prod_gam_pred, 2, median),
               integerResponse = FALSE)

plot(prod_gam_resid) 
# residuals are looking good, even the bottom one is fine
testDispersion(prod_gam_resid)

prod_gam_resid_recalc <- prod_gam_resid %>% 
  recalculateResiduals(group = community_data$year,
                       aggregateBy = median)

testTemporalAutocorrelation(prod_gam_resid_recalc, 
                            time = unique(community_data$year))
# sweet no temporal autocorrelation

# posterior predictive check looks great
pp_check(prod_gam, ndraws = 100)

conditional_effects(prod_gam)

prod_gam_loo <- brms::loo(prod_gam, moment_match = TRUE)
prod_gam_loo # only 2 values are between 0.5 and 0.7, none are greater than that

# ggsave("herb/prod_gam.png", width = 12, height = 8, dpi=1000)


# Biomass -----------------

bm_gam <- brm(total_bm ~ s(year), 
              data = community_data,
              family = Gamma(link = 'log'),
              iter = 10000,
              warmup = 2000,
              thin = 5,
              seed = 123,
              save_pars = save_pars(all = TRUE),
              control = list(adapt_delta = 0.999))
# saveRDS(bm_gam, 'data/Biomass_gam.rds')
#bm_gam <- readRDS('../Model_outputs/Biomass_gam.rds')

# check model diagnostics
# within chain autocorrelation
stan_ac(bm_gam$fit, pars = get_variables(bm_gam)[1:5]) # nice

# traceplots
stan_trace(bm_gam$fit, pars = get_variables(bm_gam)[1:5]) # nice

# ESS
stan_ess(bm_gam$fit, pars = get_variables(bm_gam)[1:5]) 

# look at the summary
# haven't overconstrained the model
# the model is strongly wiggly
summary(bm_gam)

# Check simulated residuals
bm_gam_pred <-  posterior_predict(bm_gam, ndraws = 250, summary = FALSE)

gm_gam_resid <- 
  createDHARMa(simulatedResponse = t(bm_gam_pred),
               observedResponse = bm_gam$data$total_bm,
               fittedPredictedResponse = apply(bm_gam_pred, 2, median),
               integerResponse = FALSE)

plot(gm_gam_resid) # residuals are looking good
testDispersion(gm_gam_resid)

gm_gam_resid_recalc <-  gm_gam_resid %>% 
  recalculateResiduals(group = community_data$year,
                       aggregateBy = median)

testTemporalAutocorrelation(gm_gam_resid_recalc, 
                            time = unique(community_data$year))
# sweet no temporal autocorrelation

# posterior predictive check looks great
pp_check(bm_gam, ndraws = 100)

conditional_effects(bm_gam)

bm_gam_loo <- brms::loo(bm_gam, moment_match = TRUE)
bm_gam_loo # only 1 value is between 0.5 and 0.7, none are greater than that

# ggsave("herb/biom_gam.png", width = 12, height = 8, dpi=1000)


# Abundance -----------

community_data$total_abund <- as.integer(community_data$total_abund)

abun_gam <- brm(total_abund ~ s(year), 
                data = community_data,
                family = negbinomial(link = 'log'),
                iter = 10000,
                warmup = 2000,
                thin = 5,
                seed = 123,
                save_pars = save_pars(all = TRUE),
                control = list(adapt_delta = 0.99))

# saveRDS(abun_gam, 'data/Abundance_gam.rds')
#abun_gam <- readRDS('../Model_outputs/Abundance_gam.rds')

# check model diagnostics
# within chain autocorrelation
stan_ac(abun_gam$fit, pars = get_variables(abun_gam)[1:5]) # nice

# traceplots
stan_trace(abun_gam$fit, pars = get_variables(abun_gam)[1:5]) # nice

# ESS
stan_ess(abun_gam$fit, pars = get_variables(abun_gam)[1:5]) 

# look at the summary
# haven't overconstrained the model
# the model is strongly wiggly
summary(abun_gam)

# Check simulated residuals
abun_gam_pred <- 
  posterior_predict(abun_gam, ndraws = 250, summary = FALSE)

abun_gam_resid <- 
  createDHARMa(simulatedResponse = t(abun_gam_pred),
               observedResponse = abun_gam$data$total_abund,
               fittedPredictedResponse = apply(abun_gam_pred, 2, median),
               integerResponse = TRUE)

plot(abun_gam_resid) # residuals are looking good
testDispersion(abun_gam_resid)

abun_gam_resid_recalc <- 
  abun_gam_resid %>% 
  recalculateResiduals(group = community_data$year,
                       aggregateBy = median)

testTemporalAutocorrelation(abun_gam_resid_recalc, 
                            time = unique(community_data$year))
# sweet no temporal autocorrelation

# posterior predictive check looks great
pp_check(abun_gam, ndraws = 100)

conditional_effects(abun_gam)

abun_gam_loo <- brms::loo(abun_gam, moment_match = TRUE)
abun_gam_loo # only 1 value is between 0.5 and 0.7, none are greater than that


# ggsave("herb/abun_gam.png", width = 12, height = 8, dpi=1000)



# Richness -----------
# now we'll run the gam

rich_gam <- brm(total_richness ~ s(year), 
                data = community_data,
                family = negbinomial(link = 'log'),
                iter = 10000,
                warmup = 2000,
                thin = 5,
                seed = 123,
                save_pars = save_pars(all = TRUE),
                control = list(adapt_delta = 0.99))

saveRDS(rich_gam, 'data/Richness_gam.rds')
#rich_gam <- readRDS('../Model_outputs/Richness_gam.rds')

# check model diagnostics
# within chain autocorrelation
stan_ac(rich_gam$fit, pars = get_variables(rich_gam)[1:5]) # nice

# traceplots
stan_trace(rich_gam$fit, pars = get_variables(rich_gam)[1:5]) # nice

# ESS
stan_ess(rich_gam$fit, pars = get_variables(rich_gam)[1:5]) 

# look at the summary
# haven't overconstrained the model
# the model is strongly wiggly
summary(rich_gam)

rich_gam_pred <-posterior_predict(rich_gam, ndraws = 250, summary = FALSE)

rich_gam_resid <- createDHARMa(simulatedResponse = t(rich_gam_pred),
               observedResponse = rich_gam$data$total_richness,
               fittedPredictedResponse = apply(rich_gam_pred, 2, median),
               integerResponse = TRUE)

plot(rich_gam_resid) # residuals are looking good
testDispersion(rich_gam_resid)

rich_gam_resid_recalc <- rich_gam_resid %>% 
  recalculateResiduals(group = community_data$year,
                       aggregateBy = median)

testTemporalAutocorrelation(rich_gam_resid_recalc, 
                            time = unique(community_data$year))
# sweet no temporal autocorrelation

# posterior predictive check looks great
pp_check(rich_gam, ndraws = 100)

conditional_effects(rich_gam)

rich_gam_loo <- brms::loo(rich_gam, moment_match = TRUE)
rich_gam_loo # all pareto-k values are less than 0.5

# ggsave("herb/rich_gam.png", width = 12, height = 8, dpi=1000)



# Crear la matriz de comunidad ----------
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



# Calcular el NMDS------------
NMDS <- metaMDS(community_matrix [,2:7], # Matriz de arrecife por especie
                    k = 2, # A estas dimensiones escalamos el NMDS (Reduced dimensions)
                    trymax = 1000) # Número de iteraciones que se van a realizar (cambiar si es necesario)   

# Plotear el stressplot
stressplot(NMDS)

# Plotear los resultados de NMDS
plot(NMDS)

# Extraer la información de 'region'
region <- community_matrix$region

# Plot personalizado del NMDS
ordiplot(NMDS, type = "n")
orditorp(NMDS, display = "species", col = "red", air = 2, pch = "+")
orditorp(NMDS, display = "sites", col = c(rep("firebrick", 2), rep("darkgreen", 1), rep("blue", 13), rep("orange", 14)), air = 3, cex = 0.3)
ordihull(NMDS, groups = region, draw = "polygon", label = TRUE, fill = "white", col = c("firebrick", "darkgreen", "lightblue", "gold"), alpha = 0.25)
# ordilabel(rev_NMDS, labels = region, fill = "white", border = NA)

# ggsave("herb/nmds.png", width = 12, height = 8, dpi=1000)


print(unique(region))
community_matrix[] <- lapply(community_matrix, as.numeric)


# Calcular diversidad usando índice de Shannon
diversity_indices <- diversity(as.matrix(community_matrix), index = "shannon")
print(diversity_indices)



# Threshold analysis ------------------------------------------------------------------------
# Richness of entire community
rich_deriv <-  calc_derivative(rich_gam,
                  epsilon = 0.1, order = 2,
                  2010, 2023, 's(year)')

rich_deriv %>% 
  mutate(year = round(year)) %>% 
  group_by(year) %>% 
  median_hdci(second_der, .width = 0.95) %>% 
  ungroup() %>% 
  dplyr::filter(.lower < 0 & .upper < 0)

# Community analysis ------------------------------------------------------------------------

community_multi <- community_matrix

# perform the multidimensional scale
# metaMDS automatically does a fourth root transformation and a wisconsin
# double standardization
survey_mds <- metaMDS(community_multi[, 2:7], plot = FALSE)

survey_mds$stress # there is 19.1% of variation that we are unable to explain
stressplot(survey_mds)

# let's extract the scores 
survey_mds_scores <- 
  survey_mds %>% 
  fortify() %>% 
  full_join(community_multi %>% 
              rownames_to_column(var = 'Label'))

survey_mds_scores

# Run a PERMANOVA -----------
# First, we need to recalculate the distance matrix because it isnt stored in the
# metaMDS function
survey_std <- wisconsin(community_multi[, c(2:7)] ^ 0.25)
survey_dist <- vegdist(survey_std, 'bray')

# now we'll run the PERMANOVA - this test tells us if the means of the centroids
# are significantly different from one another
adonis(survey_dist ~ bleach, data = community_multi, distance = 'bray', permutations = 999)

# cool, now that we know that they're different, we can assess whether this
# difference is due to a difference in variance - ie. are the points spread
# differently between groups
survey_perm <- betadisper(survey_dist, community_multi$bleach)
plot(survey_perm)
anova(survey_perm)
# the variance between groups is not significantly different from one another

# This tells us the contribution of each species to the difference between
# pre and post '98 bleaching
simper(survey_std, community_multi$bleach) %>% 
  summary()









