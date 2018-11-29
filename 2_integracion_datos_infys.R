# En este script se integrarán los datos extraídos del INFyS para el análisis
# de la siguiente manera:
# 1. Por medio de análisis de distancias se obtendrá la equivalencia entre
# números de "conglomerado" (usados en los muestreos de 2004 a 2014) y "upm_ids"
# (usadas en los muestreos del 2015 y 2016).
# 2. Se agruparán "conglomerados" y "upm_id"s consideradas equivalentes y se les
# asociarán unas coordenadas iguales al promedio entre ambas.
# 3. Se unirán a la anterior tabla de coordenadas los datos de "conglomerados" y
# "upm_id"s que no tuvieron pareja, sus coordenadas se quedan como calculadas con
# anterioridad.
# 4. Se usan las coordenadas anteriores para espacializar las observaciones de
# las especies de interés que fueron obtenidas con anterioridad.

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("lubridate")
library("geosphere") # Obtener distancias geográficas con la función "distGeo()"
library("raster")
library("rgeos")
library("ggplot2")
source("0_config.R")

################################################################################
# 1. Leyendo todos los data frames a utilizar
################################################################################

### Leyendo archivos de observaciones ###

nombres_archivos_observaciones <- list.files(
  ruta_carpeta_productos, pattern = "1_1_observaciones_limpias_infys.*")
rutas_archivos_observaciones <- paste0(
  ruta_carpeta_productos, "/", nombres_archivos_observaciones)

lista_dfs_observaciones <- llply(rutas_archivos_observaciones, function(ruta){
  resultado <- readRDS(ruta)
  return(resultado)
})
names(lista_dfs_observaciones) <- stri_match_first_regex(nombres_archivos_observaciones, "(.*).rds")[,2]

# Revisando los data frames leídos
l_ply(1:length(lista_dfs_observaciones), function(i){
  print(names(lista_dfs_observaciones)[i])
  glimpse(lista_dfs_observaciones[[i]])
})
# Perfecto!

### Leyendo archivos de coordenadas de muestras del INFyS###

nombres_archivos_coordenadas <- list.files(
  ruta_carpeta_productos, pattern = "1_1_coordenadas")
rutas_archivos_coordenadas <- paste0(
  ruta_carpeta_productos, "/", nombres_archivos_coordenadas)

lista_dfs_coordenadas <- llply(rutas_archivos_coordenadas, function(ruta){
  resultado <- readRDS(ruta)
  return(resultado)
})
names(lista_dfs_coordenadas) <- stri_match_first_regex(nombres_archivos_coordenadas,
  "(.*).rds")[,2]

# Revisando los data frames leídos
l_ply(1:length(lista_dfs_coordenadas), function(i){
  print(names(lista_dfs_coordenadas)[i])
  glimpse(lista_dfs_coordenadas[[i]])
})
# Perfecto!

################################################################################
# 2. Homologando "conglomerado"s con "upm_id"s del INFyS, y creando una tabla
# general de coordenadas para el INFyS
################################################################################

# Como para los datos del INFyS tenemos dos claves: "conglomerado" y "upmid", y
# no tenemos la equivalencia, lo que haremos es emparejar los conglomerados
# más cercanos:

# 1. Promediar las coordenadas de muestreos sobre el mismo "conglomerado" y "upm_id"
# respectivamente (cuando aplique). Revisar que no haya mucha variabilidad entre
# promedios.
# 2. Asociar a cada "conglomerado" el "upm_id" que está a menor distancia.
# 3. Verificar si la elección parece razonable


# Revisando que los distintos muestreos sobre el mismo "upm_id" tengan coordenadas
# parecidas.
lista_dfs_coordenadas[["1_1_coordenadas_upm_ids_infys_2015"]] %>%
  rbind(lista_dfs_coordenadas[["1_1_coordenadas_upm_ids_infys_2016"]]) %>%
  group_by(upm_id) %>%
  mutate(
    longitud_media_upm_id = mean(longitud_upm_id),
    latitud_media_upm_id = mean(latitud_upm_id)#,
    #n = n()
  ) %>%
  ungroup() %>%
  
  # Calculando distancias de cada muestreo particular a la media de upm_id
  mutate(
    distancia_centroide_m = distGeo(
      data_frame(
        "longitud_upm_id" = longitud_upm_id,
        "latitud_upm_id" = latitud_upm_id),
      data_frame(
        "longitud_media_upm_id" = longitud_media_upm_id,
        "latitud_media_upm_id" = latitud_media_upm_id
      )
    )
  ) %>%
  pull(distancia_centroide_m) %>%
  quantile(probs = seq(0, 1, 0.001))
# El 99% son iguales, y máximo distan de la media 250m. La causa más probable
# que va con la logística del INFyS es que se muestrearon "upm_id"'s distintos
# en el 2015 y 2016, por lo que casi todos dan 0... pero no está de más este
# procedimiento por los pocos que fueron muestreados en los dos años.

## Calculando el data frame con los promedios de coordenadas para cada "upm_id"
coordenadas_upm_ids_infys_2015_2016 <- lista_dfs_coordenadas[["1_1_coordenadas_upm_ids_infys_2015"]] %>%
  rbind(lista_dfs_coordenadas[["1_1_coordenadas_upm_ids_infys_2016"]] %>%
      mutate(
        upm_id = as.numeric(upm_id)
      )
  ) %>%
  group_by(upm_id) %>%
  summarise(
    longitud_upm_id = mean(longitud_upm_id),
    latitud_upm_id = mean(latitud_upm_id)
  ) %>%
  ungroup()
summary(coordenadas_upm_ids_infys_2015_2016)
glimpse(coordenadas_upm_ids_infys_2015_2016) # Observations: 9,947

# Revisando llaves naturales

lista_dfs_coordenadas[["1_1_coordenadas_conglomerados_infys_2004_2014"]] %>%
  nrow()
lista_dfs_coordenadas[["1_1_coordenadas_conglomerados_infys_2004_2014"]] %>%
  distinct(conglomerado) %>%
  nrow()

coordenadas_upm_ids_infys_2015_2016 %>%
  nrow()
coordenadas_upm_ids_infys_2015_2016 %>%
  distinct(upm_id) %>%
  nrow()
# Perfecto, están bien definidas las llaves naturales

################################################################################

## Ahora se encontrará la equivalencia entre "conglomerado"s y "upm_id"s
## asociando a cada "conglomerado" su "upm_id" más cercana y fitrando las que
## estén por arriba de cierto umbral (a obtener)

# Por medio de funciones espaciales se seleccionarán los "upm_id" que están
# más cerca de cada "conglomerado", para minimizar el trabajo computacional que
# se requiere para hacer este cálculo (que de otra forma involucraría un producto
# cartesiano.)

# Leyendo raster para obtener el sistema de coordenadas necesario para espacializar
# los data frames
raster_referencia <- raster(rutas_archivos_insumos["raster_referencia"])

# Espacializando los data frames para intersectar los data frames de "conglomerado"s
# y "upm_id"s por medio de un búfer y disminuir la complejidad en memoria y
# procesamiento de la búsqueda de parejas.
coordenadas_conglomerados_infys_2004_2014_sp <- as.data.frame(
  lista_dfs_coordenadas[["1_1_coordenadas_conglomerados_infys_2004_2014"]])
coordinates(coordenadas_conglomerados_infys_2004_2014_sp) <-~ longitud_conglomerado + latitud_conglomerado
projection(coordenadas_conglomerados_infys_2004_2014_sp) <- projection(raster_referencia)

coordenadas_upm_ids_infys_2015_2016_sp <- as.data.frame(coordenadas_upm_ids_infys_2015_2016)
coordinates(coordenadas_upm_ids_infys_2015_2016_sp) <-~ longitud_upm_id + latitud_upm_id
projection(coordenadas_upm_ids_infys_2015_2016_sp) <- projection(raster_referencia)

# Creando data frame con los upm_id's y sus conglomerados cercanos, para ello,
# creamos un búfer alrededor de cada conglomerado y extraemos los "upm_id"s que
# caigan dentro de él. Como este procedimiento tiene como fin único minimizar
# la complejidad del proceso de emparejamiento, no importa el Warning que
# básicamente dice que no se pudieron crear búfers perfectamente circulares.
relacion_upm_ids_conglomerados_cercanos <- coordenadas_conglomerados_infys_2004_2014_sp %>%
  gBuffer(byid = TRUE,
    width = 0.3,
    id = coordenadas_conglomerados_infys_2004_2014_sp$conglomerado) %>%
  extract(coordenadas_upm_ids_infys_2015_2016_sp, sp = TRUE) %>%
  transmute(
    id_pareja = 1:nrow(.),
    numero_upm_id = point.ID, #número de renglón de la tabla "coordenadas_upm_ids_infys_2015_2016_sp"
    conglomerado = conglomerado
  )

summary(relacion_upm_ids_conglomerados_cercanos)
# Los NA's corresponden a "upm_id"'s que no tuvieron "conglomerado"s cercanos, no está
# mal que sólo sean 97
# Cabe destacar que los "numero_upm_id" representan a los números de renglones de los
# "upm_id"'s.
glimpse(relacion_upm_ids_conglomerados_cercanos)

# Usando el data frame creado anteriormente para asignar a cada "conglomerado"
# su "upm_id" más cercano.
parejas_upm_ids_conglomerados <- relacion_upm_ids_conglomerados_cercanos %>%
  # eliminando upm_id's que no tuvieron conglomerados asociados
  filter(!is.na(conglomerado)) %>%
  
  # Obteniendo las coordenadas para cada "upm_id"
  inner_join(coordenadas_upm_ids_infys_2015_2016 %>%
    mutate(
      numero_upm_id = 1:nrow(.)
    ), by = "numero_upm_id") %>%
  dplyr::select(
    -numero_upm_id
  ) %>%
  
  # Obteniendo las coordenadas para cada "conglomerado
  inner_join(lista_dfs_coordenadas[["1_1_coordenadas_conglomerados_infys_2004_2014"]],
    by = "conglomerado") %>%
  
  # Calculando distancias entre parejas de "conglomerados" y "upm_ids"
  mutate(
    distancia_upm_id_conglomerado_m = distGeo(
      data_frame(
        "longitud_conglomerado" = longitud_conglomerado,
        "latitud_conglomerado" = latitud_conglomerado),
      data_frame(
        "longitud_upm_id" = longitud_upm_id,
        "latitud_upm_id" = latitud_upm_id
      )
    )
  ) %>%
  # Para cada "conglomerado" eligiendo el "upm_id" de distancia mínima
  arrange(conglomerado, distancia_upm_id_conglomerado_m) %>%
  group_by(conglomerado) %>%
  summarise(
    upm_id = first(upm_id),
    distancia_upm_id_conglomerado_m = first(distancia_upm_id_conglomerado_m),
    longitud_conglomerado = first(longitud_conglomerado),
    latitud_conglomerado = first(latitud_conglomerado),
    longitud_upm_id = first(longitud_upm_id),
    latitud_upm_id = first(latitud_upm_id)
  ) %>%
  ungroup() %>%
  mutate(
    id_pareja = 1:nrow(.)
  )
glimpse(parejas_upm_ids_conglomerados)
summary(parejas_upm_ids_conglomerados)

# Revisando una distancia natural para cortar: se considerarán como equivalentes
# las parejas de "conglomerado" y "upm_id" que disten entre sí algo menor que
# este umbral
parejas_upm_ids_conglomerados %>%
  filter(distancia_upm_id_conglomerado_m < 2000) %>%
  # Creando una columna que calcule la función de distribución
  arrange(distancia_upm_id_conglomerado_m) %>%
  mutate(
    distribucion = (1:nrow(.)) / nrow(.)
  ) %>%
  ggplot(aes(x = distancia_upm_id_conglomerado_m, y = distribucion)) +
  geom_line()
# Se ve que ya no se acumulan muchos conglomerados a una distancia mayor a 600m,
# por lo que parece un umbral razonable de corte. También tiene sentido desde el
# punto de vista del muestreo, ya que suponiendo una malla regular de 1250m entre
# conglomerados, una distancia de 600m se escucha cerca el umbral máximo para
# considerar conglomerados iguales.

# Calculando la tabla de equivalencias entre "conglomerado"s y "upmid"s con el
# umbral propuesto
equivalencias_upm_ids_conglomerados <- parejas_upm_ids_conglomerados %>%
  filter(distancia_upm_id_conglomerado_m < 600)
View(equivalencias_upm_ids_conglomerados)
glimpse(equivalencias_upm_ids_conglomerados)

# Revisando que en la tabla de equivalencias no haya duplicados en las columnas
# de "upm_id" ni "conglomerado"
nrow(equivalencias_upm_ids_conglomerados)

equivalencias_upm_ids_conglomerados %>%
  distinct(conglomerado) %>%
  nrow()

equivalencias_upm_ids_conglomerados %>%
  distinct(upm_id) %>%
  nrow()
# Perfecto, no hay duplicados, como se espera

# Revisando la tabla de contingencia entre si un "conglomerado" y su correspondiente
# "upm_id" tienen especies de interés.
equivalencias_upm_ids_conglomerados %>%
  left_join(
    rbind(
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_arbolado"]],
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_arbolado"]],
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_repoblado"]],
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_repoblado"]]
    ) %>%
    distinct(upm_id) %>%
    mutate(
      upm_id = as.numeric(upm_id),
      upm_id_tiene_datos_interes = TRUE
    ), by = "upm_id") %>%
  left_join(lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2004_2014_arbolado"]] %>%
    distinct(conglomerado) %>%
    mutate(
      conglomerado_tiene_datos_interes = TRUE
    ), by = "conglomerado") %>%
  dplyr::select(
    upm_id_tiene_datos_interes,
    conglomerado_tiene_datos_interes
  ) %>%
  table(useNA = "always")
# Está muy rara, aumenta la probabilidad de no tener datos de interés en 2015-2016
# si en 2004-2014 los tuvo y viceversa. Igual y es por los repoblados.

# Calculando la misma tabla sin tomar en cuenta repoblados:
equivalencias_upm_ids_conglomerados %>%
  left_join(
    rbind(
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_arbolado"]],
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_arbolado"]]
    ) %>%
    distinct(upm_id) %>%
    mutate(
      upm_id = as.numeric(upm_id),
      upm_id_tiene_datos_interes = TRUE
    ), by = "upm_id") %>%
  left_join(lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2004_2014_arbolado"]] %>%
    distinct(conglomerado) %>%
    mutate(
      conglomerado_tiene_datos_interes = TRUE
    ), by = "conglomerado") %>%
  dplyr::select(
    upm_id_tiene_datos_interes,
    conglomerado_tiene_datos_interes
  ) %>%
  table(useNA = "always")
# Creo que se ve un poco más razonable, pero de todas formas no mejora mucho.

# Revisando distancias entre "upm_id"s y conglomerados únicamente para los
# muestreos que tienen especies de interés
equivalencias_upm_ids_conglomerados %>%
  left_join(
    rbind(
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_arbolado"]],
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_arbolado"]],
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_repoblado"]],
      lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_repoblado"]]
    ) %>%
    distinct(upm_id) %>%
    mutate(
      upm_id = as.numeric(upm_id),
      upm_id_tiene_datos_interes = TRUE
    ), by = "upm_id") %>%
  left_join(lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2004_2014_arbolado"]] %>%
    distinct(conglomerado) %>%
    mutate(
      conglomerado_tiene_datos_interes = TRUE
    ), by = "conglomerado") %>%
  filter(!is.na(upm_id_tiene_datos_interes), !is.na(conglomerado_tiene_datos_interes)) %>%
  pull(distancia_upm_id_conglomerado_m) %>%
  quantile(probs = seq(0, 1, 0.01))
# Se ven muy bien, para quitar pseudoreplicados estoy considerando conglomerados
# que están a 100m o menos de distancia...

# Finalmente, creando el data frame correspondiente a las coordenadas para
# los muestreos del INFyS, homologando "conglomerado"s y "upm_id"s equivalentes:
# muestreos a los que se les encontró en los datos un "conglomerado" y un "upm_id"
coordenadas_infys_2004_2016 <- equivalencias_upm_ids_conglomerados %>%
  transmute(
    conglomerado = conglomerado,
    upm_id = upm_id,
    longitud = (longitud_conglomerado + longitud_upm_id) / 2,
    latitud = (latitud_conglomerado + latitud_upm_id) / 2
  ) %>%
  rbind(
    # Muestreos a los que se les encontró en los datos únicamente un "conglomerado"
    lista_dfs_coordenadas[["1_1_coordenadas_conglomerados_infys_2004_2014"]] %>%
      anti_join(equivalencias_upm_ids_conglomerados, by = "conglomerado") %>%
      transmute(
        conglomerado = conglomerado,
        upm_id = NA,
        longitud = longitud_conglomerado,
        latitud = latitud_conglomerado
      ),
    
    # Muestreos a los que se les encontró en los datos únicamente un "ump_id"
    coordenadas_upm_ids_infys_2015_2016 %>%
      anti_join(equivalencias_upm_ids_conglomerados, by = "upm_id") %>%
      transmute(
        conglomerado = NA,
        upm_id = upm_id,
        longitud = longitud_upm_id,
        latitud = latitud_upm_id
      )
  )
glimpse(coordenadas_infys_2004_2016)

# Revisando que no haya registros duplicados
nrow(coordenadas_infys_2004_2016)
coordenadas_infys_2004_2016 %>%
  distinct(conglomerado, upm_id) %>%
  nrow()
# Perfecto! Coinciden

# Revisando que los números de coordenadas coincidan con los de las tablas
# originales:
coordenadas_infys_2004_2016 %>%
  filter(!is.na(conglomerado)) %>%
  nrow()
nrow(lista_dfs_coordenadas[["1_1_coordenadas_conglomerados_infys_2004_2014"]])
# Perfecto! Coinciden

coordenadas_infys_2004_2016 %>%
  filter(!is.na(upm_id)) %>%
  nrow()
nrow(coordenadas_upm_ids_infys_2015_2016)
# Perfecto! Coinciden

saveRDS(coordenadas_infys_2004_2016, rutas_archivos_productos["coordenadas_infys_2004_2016"])

ggplot(coordenadas_infys_2004_2016, aes(x = longitud, y = latitud)) +
  geom_point()

################################################################################
# 3. Creando el data frame con los datos integrados de todos los muestreos del
# INFyS
################################################################################

observaciones_limpias_infys_2004_2016_arbolado_repoblado <- rbind(
  coordenadas_infys_2004_2016 %>%
    inner_join(lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2004_2014_arbolado"]],
      by = "conglomerado") %>%
    mutate(
      tabla = "Arbolado"
    ),
  coordenadas_infys_2004_2016 %>%
    inner_join(
      rbind(
        lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_arbolado"]] %>%
          mutate(
            tabla = "Arbolado"
          ),
        lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_arbolado"]] %>%
          mutate(
            tabla = "Arbolado"
          ),
        lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_repoblado"]] %>%
          mutate(
            tabla = "Repoblado"
          ),
        lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_repoblado"]] %>%
          mutate(
            tabla = "Repoblado"
          )
      ) %>%
      mutate(
        upm_id = as.integer(upm_id)
      ), by = "upm_id")
)
glimpse(observaciones_limpias_infys_2004_2016_arbolado_repoblado)

# Revisando que los números de renglones coincidan
nrow(observaciones_limpias_infys_2004_2016_arbolado_repoblado)

nrow(lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2004_2014_arbolado"]]) +
  nrow(rbind(
    lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_arbolado"]],
    lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_arbolado"]],
    lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2015_repoblado"]],
    lista_dfs_observaciones[["1_1_observaciones_limpias_infys_2016_repoblado"]]
  ))
# No coinciden, pero esto puede ser porque las coordenadas de algunos conglomerados
# no fueron registradas apropiadamente.

# Revisando que no haya registros duplicados:
observaciones_limpias_infys_2004_2016_arbolado_repoblado %>%
  distinct(conglomerado, upm_id, anio, genero, especie, tabla) %>%
  nrow()
# Perfecto, coinciden!

saveRDS(observaciones_limpias_infys_2004_2016_arbolado_repoblado,
 rutas_archivos_productos["observaciones_limpias_infys_2004_2016_arbolado_repoblado"])


  