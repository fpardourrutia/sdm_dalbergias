# En este script se explorarán las variables ambientales para los modelos
# de distribución potencial para ajusat. En particular, se revisarán las
# colecciones de variables: "bio", "envirem", "evapotranspiration" y "globe_dem

library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("raster")
library("stringi")
library("RStoolbox") # Para hacer PCA con rasters
source("0_config.R")

################################################################################
# 1. Leyendo los rasters de las covariables a utilizar
################################################################################

# # Obteniendo las rutas de las variables a utilizar. 
# paths_covariables <- list.files(
#   "~/Dropbox/carpetas_compartidas/dalbergias/datos_variables_amb_harmony/",
#   pattern="\\.tif$",
#   recursive = TRUE,
#   full.names=TRUE) %>%
#   data_frame(
#     nombre_variable = basename(.),
#     ruta_variable = .) %>%
#   filter(
#     stri_detect_regex(ruta_variable, "bio|envirem|evapotranspiration|globe_dem")
#   ) 
# View(paths_covariables)
# 
# # Crear un ladrillo con los rasters anteriores:
# brick <- brick()
# for(i in 1:nrow(paths_covariables))
# {
#   aux <- raster(paths_covariables$ruta_variable[i])
#   brick <- addLayer(brick,aux)
# }
# 
# saveRDS(brick, rutas_archivos_productos["brick_variables_ambientales"])
brick_variables_ambientales <- readRDS(rutas_archivos_productos["brick_variables_ambientales"])

################################################################################
# 2. Análisis exploratorios de las variables a utilizar
################################################################################

# Creando un data frame con las variables ambientales para analizarlas
df_variables_ambientales <- as.data.frame(brick_variables_ambientales) %>%
  # Filtrando NA's
  filter(complete.cases(.))
summary(df_variables_ambientales)
glimpse(df_variables_ambientales)

# Revisando correlaciones entre las variables
cor(df_variables_ambientales) %>%
  as_data_frame(rownames = "variable_1") %>%
  gather("variable_2", "correlacion", -variable_1) %>%
  arrange(correlacion)
# Hay muchísimas variables que están muy correlacionadas, por ello se hará un
# PCA con el fin de eliminar variables que podrían sesgar los resultados

################################################################################
# 3. PCA de las variables ambientales
################################################################################
# https://adnguyen.github.io/demos/RasterPCA_demo.html

pca_variables_ambientales <- rasterPCA(brick_variables_ambientales, spca = TRUE)
# Se realiza un PCA estandarizado para que los rangos de las variables no
# afecten el resultado
saveRDS(pca_variables_ambientales, rutas_archivos_productos[
  "pca_variables_ambientales"])

# Revisando el modelo:
str(pca_variables_ambientales)
summary(pca_variables_ambientales$model)

# Haciendo un diagrama de codo para seleccionar el número de componentes a
# utilizar:
pca_variables_ambientales$model$sdev %>%
  data_frame(sd_componentes = .) %>%
  transmute(
    numero_componentes = 1:nrow(.),
    var_componentes = sd_componentes^2,
    porcentaje_acumulado = cumsum(var_componentes)
  ) %>%
  ggplot(aes(x = numero_componentes, y = porcentaje_acumulado)) +
  geom_point() +
  geom_line()
# Usaré los primeros 6 componentes de acuerdo con la gráfica de codo.

# Revisando los coeficientes de cada variable en cada componente para saber qué
# representan cada uno de los 6 primeros. Esto lo haré después porque requiere
# de una extensa investigación. ***
pca_variables_ambientales$model$loadings[,1:6] %>%
  View()

# Guardando ladrillo con los seis primeros componentes principales para utilizarlos
# en el SDM
brick_componentes_principales <- raster::subset(pca_variables_ambientales$map, 1:6)
saveRDS(brick_componentes_principales,
  rutas_archivos_productos["brick_componentes_principales"])


