library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("readxl")
library("lubridate")
library("measurements") # para efectuar el cambio de coordenadas a grados y decimales
source("0_config.R")

################################################################################
# 1. Leyendo los datos a revisar
################################################################################

datos_crudos <- read_excel(rutas_archivos_insumos["herbario_nacional_mexico"])
View(datos_crudos)

################################################################################
# 2.Investigando las coordenadas faltantes de Dalbergia stevensonii
################################################################################

# Este paso únicamente se realizó para esta especie, ya que no hay muchos datos
# y hay que aprovecharlos al máximo.

conv_unit(-90.5167, from = 'dec_deg', to = "deg_min_sec")

### Boca Lacantún ###
# -90º42'8" 16º34'51"
# http://www.nuestro-mexico.com/Chiapas/Ocosingo/Areas-de-menos-de-20-habitantes/Boca-Lacantun/

### 15 km al NW de Boca Lacatún, camino a Palenque ###
# Con las coordenadas anteriores se midieron 15km sobre la carretera a Palenque
# usando Google Maps
# 16.658095, -90.811003
# -90º48'40" 16º39'29"

### Chajul ###
# -90º55'0" 16º7'0"
# https://www.dices.net/mapas/mexico/mapa.php?nombre=Chajul&id=9658

### Loma bonita ###
# -92º4'12" 16º54'34"
# http://www.nuestro-mexico.com/Chiapas/Ocosingo/Areas-de-menos-de-100-habitantes/Loma-Bonita/

### Nuevo Chihuahua ###
# -90º31'0" 16º16'0"
# https://www.dices.net/mapas/mexico/mapa.php?nombre=Nuevo-Chihuahua&id=101982

### Nuevo Guerrero ###
# -91º17'8" 16º59'12"
# http://www.nuestro-mexico.com/Chiapas/Ocosingo/Areas-de-menos-de-500-habitantes/Nuevo-Guerrero/

### Desviación San Javier-Frontera Echeverría-Lacantún ###
# http://www.nuestro-mexico.com/Chiapas/Ocosingo/Areas-de-menos-de-100-habitantes/San-Javier/
# Con la anterior se sacó la desviación citada y se vio que las coordenadas son:
# -91°00'29" 16°45'42"

coordenadas_llenadas_a_mano = data_frame(
  descripcion_coordenadas = c(
    "20 km. Desviación San Javier-Frontera Echeverría-Lacantún.",
    "A 15 km al NW de Boca Lacatún, camino a Palenque.",
    "En Boca Lacatún camino a Palenque, Mpio. Ocosingo.",
    "En ejido Chajul.",
    "En ejido Loma Bonita a la orilla del río Lacantún.",
    "En Nuevo Chihuahua a 33 km al N del Vértice del Rio. Chixoy camino a Chajul en zona Marqués de Comillas",
    "En Nuevo Guerrero a 100 km al SE de Palenque camino a Boca Lacantún"
  ),
  Longitud_completada = c(
    "-91°00'29\"",
    "-90°48'40\"",
    "-90°42'8\"",
    "-90°55'0\"",
    "-92°4'12\"",
    "-90°31'0\"",
    "-91°17'8\""
  ),
  Latitud_completada = c(
    "16°45'42\"",
    "16°39'29\"",
    "16°34'51\"",
    "16°7'0\"",
    "16°54'34\"",
    "16°16'0\"",
    "16°59'12\""
  )
)
head(coordenadas_llenadas_a_mano, 10)

################################################################################
# 3. Creando data frame con los fatos limpios
################################################################################

observaciones_limpias_herbario <- datos_crudos %>%
  transmute(
    id_ejemplar = IdEjemplar,
    # IndividuosCopias se ve interesante pero como es 1 en el 99% de los casos
    # no vale la pena investigarla a detalle.
    fecha_colecta = paste0(AnioInicial, "/", MesInicial, "/", DiaInicial) %>%
      as_date(), # Hay 3 fechas que no parsearon porque vienen como 9999/99/99
    genero,
    especie = epitetoespecifico,
    Latitud,
    Longitud,
    descripcion_coordenadas = NombreOriginal# Este dato se necesita para agregar las coordenadas investigadas.
  ) %>%
  
  # Agregando las coordenadas que se investigaron de "Dalbergia stevensonii:
  left_join(coordenadas_llenadas_a_mano, by = "descripcion_coordenadas") %>%
  mutate(
    Longitud = ifelse(especie == "stevensonii" & is.na(Longitud),
      Longitud_completada, Longitud),
    Latitud = ifelse(especie == "stevensonii" & is.na(Latitud),
      Latitud_completada, Latitud),
    coordenadas_investigadas = !is.na(Longitud_completada)
  ) %>%
  dplyr::select(
    -Latitud_completada,
    -Longitud_completada,
    -descripcion_coordenadas
  ) %>%
  filter(!is.na(Longitud), !is.na(Latitud)) %>%
  
  # Convirtiendo latitud y longitud de "grados", "minutos" y "segundos" a "grados"
  separate(Longitud,
    into = c("longitud_grados", "longitud_minutos", "longitud_segundos", "vacio"),
    sep = "°|'|\"",
    remove = FALSE,
    convert = TRUE
    ) %>%
  dplyr::select(-vacio) %>%
  separate(Latitud,
    into = c("latitud_grados", "latitud_minutos", "latitud_segundos", "vacio"),
    sep = "°|'|\"",
    remove = FALSE,
    convert = TRUE) %>%
  dplyr::select(-vacio) %>%
  mutate(
    # Arreglando los casos en los que sólo se tenía información a nivel minutos
    longitud_segundos = ifelse(is.na(longitud_segundos), 0, longitud_segundos),
    latitud_segundos = ifelse(is.na(latitud_segundos), 0, latitud_segundos),
    
    # Creando las nuevas coordenadas
    longitud = paste0(longitud_grados, " ", longitud_minutos, " ", longitud_segundos) %>%
      conv_unit(from = 'deg_min_sec', to = 'dec_deg') %>%
      as.numeric() %>%
      round(4),
    latitud = paste0(latitud_grados, " ", latitud_minutos, " ", latitud_segundos) %>%
      conv_unit(from = 'deg_min_sec', to = 'dec_deg') %>%
      as.numeric() %>%
      round(4)
  ) %>%
  dplyr::select(
    -longitud_grados,
    -longitud_minutos,
    -longitud_segundos,
    -latitud_grados,
    -latitud_minutos,
    -latitud_segundos,
    -Latitud,
    -Longitud
  )
glimpse(observaciones_limpias_herbario)

saveRDS(observaciones_limpias_herbario, rutas_archivos_productos["observaciones_limpias_herbario"])
