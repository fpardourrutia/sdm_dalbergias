# Este es un archivo de configuración donde se asignarán los parámetros que son
# utilizados a lo largo de todas los scripts utilizados en este análisis.

# Ruta carpeta raiz (relativa a la carpeta donde se encuentra este script)
ruta_carpeta_raiz <- ".."

################################################################################
# Insumos
################################################################################

# Definiendo el nombre y la ruta de la carpeta que contiene los insumos a utilizar:
nombre_carpeta_insumos <- "datos"
ruta_carpeta_insumos <- paste0(ruta_carpeta_raiz, "/", nombre_carpeta_insumos)
dir.exists(ruta_carpeta_insumos)

# Definiendo los nombres y la ruta de cada uno de los insumos individuales.
# Estos se deben encontrar dentro de la carpeta "nombre_carpeta_insumos".

# Agregar aquí el identificador y nombre de cada archivo de insumos
nombres_archivos_insumos <- c(
  ## INFyS
  "arbolado_estandarizado_ctz_2004_2014" = "infys/arbolado_estandarizado_ctz_2004_2014.csv",
  "infys_depurado_2004_2014" = "infys/infys_depurado_2004_2014.rds",
  "infys_2015" = "infys/infys_2015.xlsx",
  "infys_2016" = "infys/infys_2016.xlsx",
  
  ## Herbario
  "herbario_nacional_mexico" = "herbario_nacional_mexico/scep_dgranadillo_dstevensonii.xlsx",
  
  ## Auxiliares
  "raster_referencia" = "raster_referencia/lon_lat.tif"

)
rutas_archivos_insumos <- paste0(ruta_carpeta_insumos,
  "/", nombres_archivos_insumos)
names(rutas_archivos_insumos) <- names(nombres_archivos_insumos)

# Para cuando, por ejemplo, se va a realizar procesamiento en Batch de los
# contenidos de cada carpeta individual.
nombres_subcarpetas_insumos <- c(
  ## SNIB
  "snib_plantas_bases" = "snib_plantas/bases",
  
  ## Naturalista
  "naturalista" = "naturalista",
  
  ## Auxiliares
  "mex_edos" = "mex_edos"
)

rutas_subcarpetas_insumos <- paste0(ruta_carpeta_insumos,
  "/", nombres_subcarpetas_insumos)
names(rutas_subcarpetas_insumos) <- names(nombres_subcarpetas_insumos)


################################################################################
# Productos
################################################################################

# Definiendo el nombre y la ruta de la carpeta donde se guardarán los productos:
nombre_carpeta_productos <- "productos"
ruta_carpeta_productos <- paste0(ruta_carpeta_raiz, "/", nombre_carpeta_productos)
if(!dir.exists(ruta_carpeta_productos)){
  dir.create(ruta_carpeta_productos)
}

# Definiendo los nombres y la ruta de cada uno de los productos individuales.
# Estos se almacenarán en la carpeta "nombre_carpeta_productos"

# Agregar aquí el identificador y nombre de cada archivo de productos
nombres_archivos_productos <- c(
  
  "coordenadas_conglomerados_infys_2004_2014" = "1_1_coordenadas_conglomerados_infys_2004_2014.rds",
  "observaciones_limpias_infys_2004_2014_arbolado" = "1_1_observaciones_limpias_infys_2004_2014_arbolado.rds",
  "coordenadas_upm_ids_infys_2015" = "1_1_coordenadas_upm_ids_infys_2015.rds",
  "observaciones_limpias_infys_2015_arbolado" = "1_1_observaciones_limpias_infys_2015_arbolado.rds",
  "observaciones_limpias_infys_2015_repoblado" = "1_1_observaciones_limpias_infys_2015_repoblado.rds",
  "coordenadas_upm_ids_infys_2016" = "1_1_coordenadas_upm_ids_infys_2016.rds",
  "observaciones_limpias_infys_2016_arbolado" = "1_1_observaciones_limpias_infys_2016_arbolado.rds",
  "observaciones_limpias_infys_2016_repoblado" = "1_1_observaciones_limpias_infys_2016_repoblado.rds",
  
  "registros_snib" = "1_2_registros_snib.rds",
  "observaciones_limpias_snib" = "1_2_observaciones_limpias_snib.rds",
  
  "observaciones_limpias_herbario" = "1_4_observaciones_limpias_herbario.rds",
  
  "coordenadas_infys_2004_2016" = "2_coordenadas_infys_2004_2016.rds",
  "observaciones_limpias_infys_2004_2016_arbolado_repoblado" = "2_observaciones_limpias_infys_2004_2016_arbolado_repoblado.rds",
  
  "df_sdm_datos_abundancia_infys" = "3_df_sdm_datos_abundancia_infys.rds",
  "df_sdm_datos_abundancia_infys_conglomerados_anios" = "3_df_sdm_datos_abundancia_infys_conglomerados_anios.rds",
  "df_sdm_datos_incidencia_todas_fuentes" = "3_df_sdm_datos_incidencia_todas_fuentes.rds",
  "df_sdm_datos_incidencia_todas_fuentes_anios" = "3_df_sdm_datos_incidencia_todas_fuentes_anios.rds",
  
  "brick_variables_ambientales" = "4_1_brick_variables_ambientales.rds",
  "pca_variables_ambientales" = "4_1_pca_variables_ambientales.rds",
  "brick_componentes_principales" = "4_1_brick_componentes_principales.rds"
)
rutas_archivos_productos <- paste0(ruta_carpeta_productos,
  "/", nombres_archivos_productos)
names(rutas_archivos_productos) <- names(nombres_archivos_productos)