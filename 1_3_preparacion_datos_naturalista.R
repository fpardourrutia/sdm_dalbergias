library("plyr")
library("dplyr")
library("tidyr")
library("readr")
library("stringi")
library("geosphere")
# Estos tres últimos paquetes son necesarios para revisar que los datos no tengan
# las coordenadas ofuscadas.
library("rgdal")
library("raster")
library("ggplot2")

################################################################################
# 1. Leyendo los datos a revisar
################################################################################

# Leyendo las bases de datos con información del SNIB:
lista_nombres_archivos <- list.files(rutas_subcarpetas_insumos["naturalista"], pattern = ".csv")
lista_rutas_archivos <- paste0(rutas_subcarpetas_insumos["naturalista"], "/", lista_nombres_archivos)

# Leyendo shape file con los estados de México para revisar que las coordenadas
# de los registros sean las reales.
mex_edos <- readOGR(dsn = rutas_subcarpetas_insumos["mex_edos"], layer = "mex_edos")
#str(mex_edos)

# Leyendo raster para obtener el sistema de coordenadas necesario para espacializar
# los registros de naturalista
raster_referencia <- raster(rutas_archivos_insumos["raster_referencia"])

################################################################################
# 2. Formando data frame de interés
################################################################################

registros_naturalista <- ldply(1:length(lista_rutas_archivos), function(i){
  resultado <- read_csv(lista_rutas_archivos[[i]]) %>%
    mutate(
      archivo_origen = lista_nombres_archivos[i]
    )
  return(resultado)
  }) %>%
  dplyr::select(
    id,
    observed_on,
    latitude,
    longitude,
    scientific_name,
    # La siguiente columna es un auxiliar comprobar si las coordenadas
    # están ofuscadas o no
    lugar_auxiliar = place_guess 
  ) %>%
  separate(scientific_name, into = c("genero", "especie")) %>%
  dplyr::select(
    id,
    fecha = observed_on,
    longitud = longitude,
    latitud = latitude,
    genero,
    especie,
    lugar_auxiliar
  ) %>%
  arrange(longitud, latitud, fecha)

View(registros_naturalista)

################################################################################
# 3. Revisiones varias
################################################################################

## Revisando que los datos de naturalista no tengan las coordenadas ofuscadas

# Espacializando el data frame con los registros de Naturalista
registros_naturalista_sp <- registros_naturalista
coordinates(registros_naturalista_sp) <-~ longitud + latitud
projection(registros_naturalista_sp) <- projection(raster_referencia)
projection(mex_edos) <- projection(raster_referencia)

# Extrayendo el estado al que pertenece cada registro.
extract(mex_edos, registros_naturalista_sp, df = TRUE) %>%
  cbind(registros_naturalista_sp@data %>%
      dplyr::select(lugar_auxiliar)) %>%
  View()
# Parece que los registros siguen ofuscados porque hay muchos que caen en el mar.
# Por ahora, estos datos son inutilizables.

