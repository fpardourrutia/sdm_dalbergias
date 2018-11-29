library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("geosphere")
source("0_config.R")

################################################################################
# 1. Leyendo los datos a revisar
################################################################################

# Leyendo las bases de datos con información del SNIB:
lista_nombres_bases <- list.files(rutas_subcarpetas_insumos["snib_plantas_bases"],
  pattern = ".sqlite")
lista_rutas_bases <- paste0(rutas_subcarpetas_insumos["snib_plantas_bases"], "/",
  lista_nombres_bases)
lista_conexiones <- llply(lista_rutas_bases, src_sqlite)
names(lista_conexiones) <- lista_nombres_bases

# Revisando cada una de las bases para encontrar los campos de interés:
l_ply(names(lista_conexiones), function(nombre_conexion){
  print(nombre_conexion)
  lista_conexiones[[nombre_conexion]] %>%
    src_tbls() %>%
    print()
})
# Todas las bases tienen exactamente las mismas tablas, por lo que la tabla
# útil es la de "plantas"

## Revisando la tabla de "plantas" en todas las bases. Notar que cambia de nombre
## según la base.
l_ply(names(lista_conexiones), function(nombre_conexion){
  print(nombre_conexion)
  nombre_tabla_principal <- (stri_match_first_regex(nombre_conexion,
    pattern = ".*(plantas.*).sqlite"))[[1,2]]
  lista_conexiones[[nombre_conexion]] %>%
    tbl(nombre_tabla_principal) %>%
    glimpse()
})

# Obteniendo los campos de interés de todas las bases de datos
# registros_snib <- ldply(names(lista_conexiones), function(nombre_conexion){
#   print(nombre_conexion)
#   nombre_tabla_principal <- (stri_match_first_regex(nombre_conexion,
#     pattern = ".*(plantas.*).sqlite"))[[1,2]]
#   resultado <- lista_conexiones[[nombre_conexion]] %>%
#     tbl(nombre_tabla_principal) %>%
#     mutate(
#       sub_base = nombre_conexion
#     ) %>%
#     select(
#       sub_base,
#       ogc_fid,
#       idejemplar, # Llave de los registros
#       generovalido,
#       especievalida,
#       genero,
#       especie,
#       longitud,
#       latitud,
#       fechacolecta,
#       fuente
#     ) %>%
#     collect()
# })
# saveRDS(registros_snib, rutas_archivos_productos["registros_snib"])
registros_snib <- readRDS(rutas_archivos_productos["registros_snib"])
glimpse(registros_snib)
View(registros_snib)

################################################################################
# 2. Revisiones varias
################################################################################

## Revisando si las bases de datos tienen registros duplicados entre sí

nrow(registros_snib)
# 6155608

registros_snib %>%
  group_by(sub_base) %>%
  tally() %>%
  ungroup() %>%
  filter(sub_base != "plantas.sqlite") %>%
  pull(n) %>%
  sum()
# 3077804 registros en "plantas.sqlite", divididos también en las otras tablas.
# Por ello, la única tabla verdaderamente útil es la de plantas.

df_auxiliar_registros_snib <- registros_snib %>%
  # Filtrando sólo los registros de la base general, ya que las otras son simplemente
  # subconjuntos
  filter(sub_base == "plantas.sqlite") %>%
  # Filtrando especies de interés
  filter(
    # Dalbergias y Swietenias
    stri_detect_fixed(generovalido, "Dalbergia")|
    stri_detect_fixed(generovalido, "Swietenia")|
    stri_detect_fixed(genero, "Dalbergia")|
    stri_detect_fixed(genero, "Swietenia")
  )

df_auxiliar_registros_snib %>%
  group_by(especie, especievalida) %>%
  tally() %>%
  arrange(especie, especievalida) %>%
  View()
# Podemos ver que la columna "especievalida" es la que contiene la información
# curada, por lo que será la utilizada.

# Ahora revisaremos si no hay duplicados en cuanto a "idejemplar", para ver si
# cada registro es en realidad un ejemplar distinto.
nrow(df_auxiliar_registros_snib)

df_auxiliar_registros_snib %>%
  distinct(idejemplar) %>%
  nrow()
# Perfecto, coinciden

# NOTA IMPORTANTE: Suponemos que cada registro en el SNIB corresponde a un
# individuo (no que corresponde a parte de un individuo como es el caso del INFyS)

df_auxiliar_registros_snib_posiblemente_limpios <- registros_snib %>%
  # Filtrando sólo los registros de la base general, ya que las otras son simplemente
  # subconjuntos
  filter(sub_base == "plantas.sqlite") %>%
  select(
    -sub_base,
    -ogc_fid,
    -generovalido,
    -genero,
    -especie
  ) %>%
  # Filtrando especies de interés
  filter(
    especievalida %in% c(
      "Dalbergia granadillo",
      "Dalbergia retusa",
      "Dalbergia stevensonii",
      "Swietenia macrophylla",
      "Swietenia mahogani",
      "Swietenia humilis"
    )
  ) %>%
  separate(especievalida, into = c("genero", "especie"))

## Revisiones varias

df_auxiliar_registros_snib_posiblemente_limpios %>%
  group_by(genero, especie) %>%
  tally()
# Perfecto, tenemos observaciones de todas las especies.

# Revisión rápida para darnos una idea de si existen registros duplicados o no:
df_auxiliar_registros_snib_posiblemente_limpios %>%
  inner_join(df_auxiliar_registros_snib_posiblemente_limpios, by = c("genero", "especie")) %>%
  mutate(
    distancia_entre_observaciones_m = distGeo(
      data_frame("longitud" = longitud.x, "latitud" = latitud.x),
      data_frame("longitud" = longitud.y, "latitud" = latitud.y)
    )
  ) %>%
  # Quitando observaciones que trivialmente son la misma
  filter(idejemplar.x != idejemplar.y) %>%
  arrange(distancia_entre_observaciones_m) %>%
  # Quitar el siguiente filtro para la enorme cantidad de registros exactamente duplicados:
  filter(distancia_entre_observaciones_m > 0) %>%
  View()
# Parece que existen muchísimos registros duplicados, pero confiamos en el equipo
# revisor del SNIB y posiblemente las coordenadas fueron calculadas por unidad de
# muestreo y no por observación individual. Por ello, no se eliminarán. Sin embargo,
# un caso en el que eliminaremos registros es cuando están en las mismas coordenadas,
# con las mismas fechas de captura, pero de distintas fuentes.

## Revisando posibles duplicados provenientes de distintas fuentes
df_auxiliar_registros_snib_posiblemente_limpios %>%
  inner_join(df_auxiliar_registros_snib_posiblemente_limpios, by = c("genero", "especie")) %>%
  mutate(
    distancia_entre_observaciones_m = distGeo(
      data_frame("longitud" = longitud.x, "latitud" = latitud.x),
      data_frame("longitud" = longitud.y, "latitud" = latitud.y)
    )
  ) %>%
  # Quitando observaciones que trivialmente son la misma
  filter(idejemplar.x != idejemplar.y) %>%
  filter(fuente.x != fuente.y) %>%
  arrange(distancia_entre_observaciones_m) %>%
  View()


## Revisando fuentes para ver si no hay registros del INFyS
df_auxiliar_registros_snib_posiblemente_limpios %>%
  group_by(fuente) %>%
  tally() %>%
  arrange(fuente) %>%
  View()
# Eliminaremos los datos del "INFyS. 2010" para evitar redundancias.

observaciones_limpias_snib <- registros_snib %>%
  
  # Filtrando sólo los registros de la base general, ya que las otras son simplemente
  # subconjuntos
  filter(sub_base == "plantas.sqlite") %>%
  
  # Filtrando especies de interés
  filter(
    especievalida %in% c(
      "Dalbergia granadillo",
      "Dalbergia retusa",
      "Dalbergia stevensonii",
      "Swietenia macrophylla",
      #"Swietenia mahogani",
      "Swietenia humilis"
    )
  ) %>%
  
  # Redondeando la latitud y la longitud, para evitar errores por diferencias
  # minúsculas entre ellas
  mutate(
    latitud = round(latitud, 4),
    longitud = round(longitud, 4)
  ) %>%
  
  # Agrupando observaciones por género + especie ("especievalida"), "latitud",
  # "longitud", "fechacolecta" y "fuente"
  group_by(especievalida, fechacolecta, latitud, longitud, fuente) %>%
  tally() %>%
  
  # Ahora, para cada conjunto de registros con la misma "especievalida",
  # "fechacolecta", "latitud" "longitud", para asociarle el número de individuos,
  # minimizando pseudoreplicados, simplemente tomar la mayor n de todas las
  # fuentes.
  group_by(especievalida, fechacolecta, latitud, longitud) %>%
  summarise(
    numero_fuentes = n(),
    n = max(n),
    fuentes = paste0(fuente, collapse = ", ")
  ) %>%
  ungroup() %>%
  
  # Eliminando registros del "INFyS. 2010"
  filter(!stri_detect_fixed(fuentes, "INFyS. 2010")) %>%
  
  separate(especievalida, into = c("genero", "especie")) %>%
  # select(
  #   -numero_fuentes,
  #   -fuentes
  # ) %>%
  rename(
    fecha_colecta = fechacolecta
  )
  
saveRDS(observaciones_limpias_snib, rutas_archivos_productos["observaciones_limpias_snib"])
