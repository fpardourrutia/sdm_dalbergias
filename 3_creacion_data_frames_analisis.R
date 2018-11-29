# En este script se crean los data frames para el ajuste de los modelos de
# de distribución potencial de especies.

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("ggplot2")
library("purrr")
source("0_config.R")

################################################################################
# 1. Revisando las especies en los datos limpios
################################################################################

rbind(
  readRDS(rutas_archivos_productos["observaciones_limpias_herbario"]) %>%
    select(
      genero,
      especie
    ),
  readRDS(rutas_archivos_productos["observaciones_limpias_snib"]) %>%
    select(
      genero,
      especie
    ),
  readRDS(rutas_archivos_productos["observaciones_limpias_infys_2004_2016_arbolado_repoblado"]) %>%
    select(
      genero,
      especie
    )
) %>%
  distinct(genero, especie) %>%
  arrange(genero, especie) %>%
  View()

# Super bien los nombres, las especies de interés son solo 4:
# Dalbergia granadillo
# Dalbergia stevensonii
# Swietenia humilis
# Swietenia macrophylla

################################################################################
# 2. Leyendo las observaciones de interés considerando únicamente esos registros
################################################################################

# Los datos del herbario sólo tienen registros de las especies anteriores.
observaciones_limpias_herbario <- readRDS(
  rutas_archivos_productos["observaciones_limpias_herbario"])
glimpse(observaciones_limpias_herbario)

observaciones_limpias_snib <- readRDS(
  rutas_archivos_productos["observaciones_limpias_snib"])  %>%
  filter(
    (genero == "Dalbergia" & especie == "granadillo") |
    (genero == "Dalbergia" & especie == "stevensonii") |
    (genero == "Swietenia" & especie == "humilis") |
    (genero == "Swietenia" & especie == "macrophylla")
  )
glimpse(observaciones_limpias_snib)
observaciones_limpias_snib %>%
  distinct(genero, especie)

observaciones_limpias_infys <- readRDS(
rutas_archivos_productos["observaciones_limpias_infys_2004_2016_arbolado_repoblado"]) %>%
  filter(
    (genero == "Dalbergia" & especie == "granadillo") |
    (genero == "Dalbergia" & especie == "stevensonii") |
    (genero == "Swietenia" & especie == "humilis") |
    (genero == "Swietenia" & especie == "macrophylla")
  )
glimpse(observaciones_limpias_infys)
View(observaciones_limpias_infys)

# Revisando las especies encontradas en todos los data frames:
rbind(
  observaciones_limpias_herbario %>%
    select(
      genero,
      especie
    ),
  observaciones_limpias_snib %>%
    select(
      genero,
      especie
    ),
  observaciones_limpias_infys %>%
    select(
      genero,
      especie
    )
  ) %>%
  distinct(genero, especie) %>%
  View()


################################################################################
# 3. Creando data frame para ajustar modelos de distribución potencial usando 
# datos de abundancia.
################################################################################

# Los datos del INFyS tienen las siguientes características:
# 1. Cada pareja de coordenadas (longitud, latitud), representa al mismo esfuerzo
# de muestreo, por lo que podemos usar datos de abundancias sin correr el riesgo
# de sesgar espacialmente el análisis por estos motivos. Este sesgo podría ocurrir
# en el SNIB, por ejemplo, si en sitio de muestro se hizo un censo de los árboles
# en un área extensa, y se asoció a la misma coordenada; mientras que para otros
# muestreos se hizo un censo en una mucha menor área, asociada a la misma
# coordenada. Esto es cierto en particular para los datos de "Arbolado", porque
# para los de "repoblado" sólo tenemos información del 2015-2016 (y no de <2014).
# 2. Como espacialmente los muestreos son siempre en las mismas coordenadas (que
# son fijas), es sencillo eliminar pseudoreplicados.
# 3. El uso del INFyS disminuye el riesgo de sesgos muestrales porque es un
# monitoreo estandarizado a escala nacional.

# Por ello, para crear el data frame con datos de abundancia se utilizarán
# únicamente datos del INFyS.

glimpse(observaciones_limpias_infys)

df_sdm_datos_abundancia_infys <- observaciones_limpias_infys  %>%
  filter(tabla == "Arbolado") %>%
  # Agrupando por muestreo espacial, genero y especie
  group_by(conglomerado, upm_id, genero, especie) %>%
  summarise(
    longitud = first(longitud),
    latitud = first(latitud),
    # Para cada localidad espacial se utiliza el máximo valor de "n" a lo largo
    # de todos los remuestreos, con el fin de eliminar pseudoreplicados por
    # muestreos del mismo individuo en distintos años
    n = max(n)
  ) %>%
  ungroup() %>%
  dplyr::select(
    -conglomerado,
    -upm_id
  ) %>%
  mutate(
    id = 1:nrow(.),
    genero = as.character(genero),
    especie = as.character(especie)
  ) %>%
  # Repitiendo cada renglón el número de veces que dicta "n"
  ddply(.(id), function(df){
    numero_repeticiones <- df$n
    resultado <- map_dfr(1:numero_repeticiones, function(i){
      return(df)
    })
    return(resultado)
  }) %>%
  dplyr::select(
    -id,
    -n
  )
  
glimpse(df_sdm_datos_abundancia_infys)
View(df_sdm_datos_abundancia_infys)

ggplot(df_sdm_datos_abundancia_infys,
  aes(x = longitud, y = latitud, colour = paste0(genero, " ", especie))) +
  geom_jitter()

saveRDS(df_sdm_datos_abundancia_infys,
  rutas_archivos_productos["df_sdm_datos_abundancia_infys"])

################################################################################
# 4. Creando data frame para ajustar modelos de distribución potencial usando 
# datos de presencia
################################################################################

# Como para los otros data frames no se satisfacen las caracterísitcas mencionadas
# para el INFyS (en particular, 1 y 3), el análisis con toda la información
# conjunta será utilizando datos de presencia únicamente. Cabe destacar que aquí
# tenemos la esperanza de que la gran cantidad de datos ayude a ajustar un mejor
# modelo

glimpse(observaciones_limpias_snib)
glimpse(observaciones_limpias_herbario)

### Antes de proceder, es conveniente recalcar que no hay que eliminar posibles
# pseudoreplicados en los datos porque en Maxent existe la opción de simplemente
# eliminar datos repetidos que caigan en las mismas celdas del raster.

df_sdm_datos_incidencia_todas_fuentes <- rbind(
  observaciones_limpias_herbario %>%
    select(
      genero,
      especie,
      longitud,
      latitud
    ),
  observaciones_limpias_snib %>%
    select(
      genero,
      especie,
      latitud,
      longitud
    ),
  observaciones_limpias_infys %>%
    select(
      genero,
      especie,
      longitud,
      latitud
    )
  ) %>%
  distinct() %>%
  mutate(
    genero = as.character(genero),
    especie = as.character(especie)
  )

glimpse(df_sdm_datos_incidencia_todas_fuentes)
View(df_sdm_datos_incidencia_todas_fuentes)

df_sdm_datos_incidencia_todas_fuentes %>%
  group_by(genero, especie) %>%
  tally()

ggplot(df_sdm_datos_incidencia_todas_fuentes,
  aes(x = longitud, y = latitud, colour = paste0(genero, " ", especie))) +
  geom_jitter()

saveRDS(df_sdm_datos_incidencia_todas_fuentes,
  rutas_archivos_productos["df_sdm_datos_incidencia_todas_fuentes"])
  
