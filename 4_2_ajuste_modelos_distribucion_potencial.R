library("raster")
library("dismo")
library("rJava")

library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("ggplot2")

################################################################################
# 1. Leyendo los datos a utilizar
################################################################################

df_sdm_datos_abundancia_infys <- readRDS(
  "../productos_parciales/df_sdm_datos_abundancia_infys.rds")
glimpse(df_sdm_datos_abundancia_infys)

df_sdm_datos_incidencia_todas_fuentes <- readRDS(
  "../productos_parciales/df_sdm_datos_incidencia_todas_fuentes.rds")
glimpse(df_sdm_datos_incidencia_todas_fuentes)

brik_pca_variables_ambientales <- readRDS("../productos_parciales/4_brik_pca_variables_ambientales.rds")

################################################################################
# 2. Ajustando modelos con validación cruzada
################################################################################

modelos_sdm_datos_abundancia_infys_cv <- df_sdm_datos_abundancia_infys %>%
  dlply(.(genero, especie), function(df){
    
    # Para el nombre del archivo que genera Maxent
    nombre_cientifico <- paste0(unique(tolower(df$genero)),
      "_", unique(df$especie))
    
    df_entrenamiento <- df %>%
      dplyr::select(
        -genero,
        -especie
      )
    
    resultado <- maxent(
      brik_pca_variables_ambientales,
      df_entrenamiento,
      removeDuplicates = FALSE,
      path = paste0("../resultados_sdm/abundancia_infys/", nombre_cientifico, "_cv"),
      
      args = c(
        "-P",
        "replicates=5",
        "writebackgroundpredictions=true")
    )
    #-P: curvas de respuesta
})
  
modelos_sdm_datos_incidencia_todas_fuentes_cv <- df_sdm_datos_incidencia_todas_fuentes %>%
  dlply(.(genero, especie), function(df){
    
    # Para el nombre del archivo que genera Maxent
    nombre_cientifico <- paste0(unique(tolower(df$genero)),
      "_", unique(df$especie))
    
    df_entrenamiento <- df %>%
      dplyr::select(
        -genero,
        -especie
      )
    
    resultado <- maxent(
      brik_pca_variables_ambientales,
      df_entrenamiento,
      removeDuplicates = TRUE,
      path = paste0("../resultados_sdm/incidencia_todas_fuentes/", nombre_cientifico, "_cv"),
      
      args = c(
        "-P",
        "replicates=5",
        "writebackgroundpredictions=true")
    )
    #-P: curvas de respuesta
  })

################################################################################
# 3. Ajustando modelos finales (sin validación cruzada)
################################################################################

# Falta seleccionar variables de importancia

modelos_sdm_datos_abundancia_infys <- df_sdm_datos_abundancia_infys %>%
  dlply(.(genero, especie), function(df){
    
    # Para el nombre del archivo que genera Maxent
    nombre_cientifico <- paste0(unique(tolower(df$genero)),
      "_", unique(df$especie))
    
    df_entrenamiento <- df %>%
      dplyr::select(
        -genero,
        -especie
      )
    
    resultado <- maxent(
      brik_pca_variables_ambientales,
      df_entrenamiento,
      removeDuplicates = FALSE,
      path = paste0("../resultados_sdm/abundancia_infys/", nombre_cientifico),
      
      args = c(
        "-P",
        "writebackgroundpredictions=true")
    )
    #-P: curvas de respuesta
  })
# Arreglar los nombres, estos son provisionales
names(modelos_sdm_datos_abundancia_infys) <- df_sdm_datos_abundancia_infys %>%
  ddply(.(genero, especie), function(df){
    resultado <- paste0(tolower(unique(df$genero)), "_", unique(df$especie)) %>%
      data_frame(nombre = .)
  }) %>%
  pull(nombre)

modelos_sdm_datos_incidencia_todas_fuentes <- df_sdm_datos_incidencia_todas_fuentes %>%
  dlply(.(genero, especie), function(df){
    
    # Para el nombre del archivo que genera Maxent
    nombre_cientifico <- paste0(unique(tolower(df$genero)),
      "_", unique(df$especie))
    
    df_entrenamiento <- df %>%
      dplyr::select(
        -genero,
        -especie
      )
    
    resultado <- maxent(
      brik_pca_variables_ambientales,
      df_entrenamiento,
      removeDuplicates = TRUE,
      path = paste0("../resultados_sdm/incidencia_todas_fuentes/", nombre_cientifico),
      
      args = c(
        "-P",
        "writebackgroundpredictions=true")
    )
    #-P: curvas de respuesta
  })
# Arreglar los nombres, estos son provisionales
names(modelos_sdm_datos_incidencia_todas_fuentes) <- df_sdm_datos_incidencia_todas_fuentes %>%
  ddply(.(genero, especie), function(df){
    resultado <- paste0(tolower(unique(df$genero)), "_", unique(df$especie)) %>%
      data_frame(nombre = .)
  }) %>%
  pull(nombre)


################################################################################
# 4. Prediciendo y ploteando resultados
################################################################################

llply(1:length(modelos_sdm_datos_abundancia_infys), function(i){
  predict(modelos_sdm_datos_abundancia_infys[[i]], brik_pca_variables_ambientales,
  filename = paste0("../resultados_sdm/predicciones/",
    names(modelos_sdm_datos_abundancia_infys)[i], "_abundancia_infys.tif"),
  format = "GTiff", overwrite=TRUE)
})

llply(1:length(modelos_sdm_datos_incidencia_todas_fuentes), function(i){
  predict(modelos_sdm_datos_incidencia_todas_fuentes[[i]], brik_pca_variables_ambientales,
  filename = paste0("../resultados_sdm/predicciones/",
    names(modelos_sdm_datos_incidencia_todas_fuentes)[i], "_incidencia_todas_fuentes.tif"),
  format = "GTiff", overwrite=TRUE)
})

# Ploteando resultados
llply()

plot(pred_snib_v0)
points(shape_snib_snmb_lcc[
  shape_snib_snmb_lcc$nom_cient == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2016) &
    shape_snib_snmb_lcc$esquema == "SNIB",])

plot(pred_snib_snmb_v0)
points(shape_snib_snmb_lcc[
  shape_snib_snmb_lcc$nom_cient == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2016),])

