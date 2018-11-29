library("plyr")
library("dplyr")
library("tidyr")
library("readxl")
library("readr")
library("ggplot2")
library("geosphere") # Obtener distancias geográficas con la función "distGeo()"
source("funciones_auxiliares.R")
source("0_config.R")

################################################################################
# 1. Leyendo los datos a revisar
################################################################################

### INFyS 2004-2014 ###
arbolado_estandarizado_ctz_2004_2014 <- read_csv(
  rutas_archivos_insumos["arbolado_estandarizado_ctz_2004_2014"])
infys_depurado_2004_2014 <- readRDS(rutas_archivos_insumos["infys_depurado_2004_2014"])
glimpse(infys_depurado_2004_2014)

### INFyS 2015 ###
nombres_hojas_infys_2015 <- excel_sheets(rutas_archivos_insumos["infys_2015"])
lista_hojas_infys_2015 <- llply(1:length(nombres_hojas_infys_2015), function(i){
  resultado <- read_excel(rutas_archivos_insumos["infys_2015"], sheet = i, guess_max = 1000000)
  return(resultado)
})
names(lista_hojas_infys_2015) <- nombres_hojas_infys_2015

### INFyS 2016 ###
nombres_hojas_infys_2016 <- excel_sheets(rutas_archivos_insumos["infys_2016"])
lista_hojas_infys_2016 <- llply(1:length(nombres_hojas_infys_2016), function(i){
  resultado <- read_excel(rutas_archivos_insumos["infys_2016"], sheet = i, guess_max = 1000000)
  return(resultado)
})
names(lista_hojas_infys_2016) <- nombres_hojas_infys_2016

################################################################################
# 2. Revisando cada una de las bases de datos anteriores
################################################################################

### arbolado_estandarizado_ctz_2004_2014 e infys_depurado_2004_2014 ###

glimpse(arbolado_estandarizado_ctz_2004_2014)
# Conglomerado: nombre de conglomerado
# cgl_sit_arb: conglomerado, # sitio y # registro (!= arbol)
# Anio: anio de visita. cgl_sit_arb + Anio es una llave de cada registro (hay algunos errores)
# Tipo_cgl: tipo de conglomerado.
# Sitio: número de sitio
# Registro: número de registro o "tronco"
# Arbol: número de arbol, un árbol puede constar de varios registros
# NumeroTallos
# Genero, Especie: género y especie como fueron descritos en campo
# Genero_Orig_corregido, Especie_Orig_corregido: genero y especies originales
# estandarizadas
# Genero_CTZ, Especie_CTZ, Genero_APG, Especie_APG: nombre científico de acuerdo a distintas convenciones.
# Categoria_Infra_Orig_corregido, Categoria_Infra_CTZ, Categoria_Infra_APG: tipo de subcategoría (si aplica)
# Infra_Orig_corregido, Infra_CTZ, Infra_APG: subcategoría
# IdCondicion, Condicion: tipo de observacion
# Las otras variables corresponden a los valores de diámtero normal y altura
# total por observación, así como medidas por "Genero y especie" con su respectivo
# valor Z (yo creo que para validar si la especie fue correctamente identificada).
# Notar que faltan las coordenadas, por lo que posiblemente sea mejor utilizar la
# base "infys_depurado_2004_2014",

# # Revisando los renglones donde alguno de los siguientes grupos de columnas no
# # tiene el mismo valor que las demás:
# # Genero, Genero_Orig_corregido, Genero_CTZ, Genero_APG
# # Especie, Especie_Orig_corregido, Especie_CTZ, Especie_APG
# 
# revision_valores_columnas_genero_especie <- arbolado_estandarizado_ctz_2004_2014 %>%
#   transmute(
#     id_arbol = 1:nrow(.),
#     Genero_Especie = paste0(Genero, " ", Especie),
#     Genero_Orig_corregido = paste0(Genero_Orig_corregido, " ", Especie_Orig_corregido),
#     Genero_CTZ_Especie_CTZ = paste0(Genero_CTZ, " ", Especie_CTZ),
#     Genero_APG_Especie_APG = paste0(Genero_APG, " ", Especie_APG)
#   ) %>%
#   gather("variable", "valor", Genero_Especie, Genero_Orig_corregido,
#     Genero_CTZ_Especie_CTZ, Genero_APG_Especie_APG) %>%
#   group_by(id_arbol) %>%
#   mutate(
#     n = length(unique(valor))
#   ) %>%
#   ungroup() %>%
#   filter(n > 1) %>%
#   spread("variable", "valor") %>%
#   select(-id_arbol, -n) %>%
#   distinct()
# 
# # Revisando los renglones donde alguno de los siguientes grupos de columnas no
# # Categoria_Infra_Orig_corregido, Categoria_Infra_CTZ, Categoria_Infra_APG
# # Infra_Orig_corregido, Infra_CTZ, Infra_APG
# 
# revision_valores_columnas_infra <- arbolado_estandarizado_ctz_2004_2014 %>%
#   transmute(
#     id_arbol = 1:nrow(.),
#     Infra_Orig_corregido = paste0(Categoria_Infra_Orig_corregido, " ", Infra_Orig_corregido),
#     Infra_CTZ = paste0(Categoria_Infra_CTZ, " ", Infra_CTZ),
#     Infra_APG = paste0(Categoria_Infra_APG, " ", Infra_APG)
#   ) %>%
#   gather("variable", "valor", Infra_Orig_corregido, Infra_CTZ, Infra_APG) %>%
#   group_by(id_arbol) %>%
#   mutate(
#     n = length(unique(valor))
#   ) %>%
#   ungroup() %>%
#   filter(n > 1) %>%
#   spread("variable", "valor") %>%
#   select(-id_arbol, -n) %>%
#   distinct()

glimpse(infys_depurado_2004_2014)
revision_valores_infys_depurado_2004_2014 <- revisa_valores(infys_depurado_2004_2014)
# Conglomerado: nombre del conglomerado
# sitio/Sitio: preferir "sitio" sobre "Sitio"
# de las coordenadas por Sitio.
# `Ciclo Muestreo`: sólo está del 2004-2007 es incorrecto, a menos que tenga otros fines.
# Cgls_Anio_muestreo: es incorrecto, a menos que tenga otros fines.
# Clgs_Accesibilidad_SUPRA: si fue muestreado o no el conglomerado
# Cgls_Accesibilidad: tipo de conglomerado y explicación de "Clgs_Accesibilidad_SUPRA"
# Cgls_M_Formato: tipo de muestreo según comunidad de árboles
# Cgls_M_Formato_Inferido: más detalles (revisar qué es)
# Sitio_Tipo: una clasificación del tipo de sitio
# Sitio_Accesibilidad: accesibilidad del sitio
# Sitio_Uso_de_Coordenadas_Campo: tipo de coordenadas que se muestran
# X_WGS84_Dep, Y_WGS84_Dep: coordenadas X y Y. Estas coordenadas se han tomado con
# distintos grados de precisión de acuerdo al muestreo (por ejemplo, a veces se
# tienen hasta 4 coordenadas distintas por sitio y otras veces la misma coordenada
# para todo el conglomerado), pero en general son bastante aceptables.
# cgl_sit_arb: conglomerado, # sitio + # registro
# Anio: por alguna razón, aquí los años sí van del 2004-2014.
# Tipo_cgl: inicial o reemplazo, hay que ver qué diferencias hay entre ellos.
# Registro: número de registro o "tronco"
# Arbol: número de arbol, un árbol puede constar de varios registros
# IdCondicion, Condicion: tipo de observacion
# Las otras variables corresponden a los valores de diámtero normal y altura
# total por observación, así como medidas por "Genero y especie" con su respectivo
# valor Z (yo creo que para validar si la especie fue correctamente identificada).

## Cabe destacar que en esta base de datos existen registros duplicados. Esto
## checarlo con base en la llave natural de la tabla: "cgl_sit_arb" + "Anio".

arbolado_estandarizado_ctz_2004_2014 %>%
  distinct(cgl_sit_arb, Anio) %>%
  nrow()

infys_depurado_2004_2014 %>%
  distinct(cgl_sit_arb, Anio) %>%
  nrow()

# Son iguales, esto comprueba que las variables utilizadas son una llave natural
# de ambas tablas y que ambas tablas tienen la misma información con alta proba-
# bilidad. Para estar seguros, comparar registro por registro:

anti_join(infys_depurado_2004_2014, arbolado_estandarizado_ctz_2004_2014,
  by = c("cgl_sit_arb", "Anio")) %>%
  View()

anti_join(arbolado_estandarizado_ctz_2004_2014, infys_depurado_2004_2014,
  by = c("cgl_sit_arb", "Anio")) %>%
  View()

# Por ello, se utilizará la tabla "infys_depurado_2004_2014" para este análisis.

# Encontrando maneras de identificar observaciones repetidas a lo largo de distintos
# muestreos. Para ello, debemos obtener una manera de saber qué observaciones se
# tomaron en el mismo lugar, pero en diferentes periodos de tiempo. Al principio
# creí que las observaciones podían ser agrupadas por sitios correspondientes a
# conglomerados, ya que estos son fijos a lo largo de los distintos muestreos.
# No obstante, esto no sucede en la base porque no está bien capturada la
# relación de cada sitio con sus correspondientes coordenadas, como se puede ver
# en las siguientes tablas:

## Número de coordenadas por muestreo de sitio de acuerdo con la columna "Sitio"
infys_depurado_2004_2014 %>%
  distinct(Conglomerado, Sitio,  Anio, X_WGS84_Dep, Y_WGS84_Dep) %>%
  group_by(Conglomerado, Sitio, Anio) %>%
  tally() %>%
  pull(n) %>%
  table()
# La mayoría de los muestreos de sitios tienen 4 coordenadas distintas,
# lo que sugiere una discrepancia entre la columna de Sitio en la base de datos
# y los sitios wcomo realmente se tomaron en campo.

## Número de coordenadas por muestreo de sitio de acuerdo con la columna "sitio"
infys_depurado_2004_2014 %>%
  distinct(Conglomerado, sitio, Anio, X_WGS84_Dep, Y_WGS84_Dep) %>%
  group_by(Conglomerado, sitio, Anio) %>%
  tally() %>%
  ungroup() %>% 
  pull(n) %>%
  table()
# Sin embargo, se puede ver que la columna "sitio" al parecer sí refleja de manera
# adecuada los sitios como realmente se tomaron en campo, aunque aún queda la posi-
# bilidad de que los nombres de los sitios difieran entre remuestreos, por ejemplo,
# que el sitio 2 tomado en un año sea el sitio 3 al siguiente, etc.

# ¿Coincide la columna "sitio" con el sitio especificado en la columna "cgl_sit_arb"?
infys_depurado_2004_2014 %>% 
  separate(cgl_sit_arb, into = c("cgl", "sit", "arb")) %>%
  select(sitio, sit) %>%
  table()
# No coinciden en absoluto. Es más, son totalmente independientes y "sitio" para
# haber sido asignada aleatoriamente, por ello, igual y es preferible hacer el
# estudio mediante agregados por conglomerado.

# ¿Coincide la columna "Sitio" con el sitio especificado en la columna "cgl_sit_arb"?
infys_depurado_2004_2014 %>% 
  separate(cgl_sit_arb, into = c("cgl", "sit", "arb")) %>%
  select(Sitio, sit) %>%
  table()
# Coincide perfectamente... parece que a cada registro se le asignó un "sitio"
# aleatorio, y por lo tanto, las coordenadas (dadas por dichos "sitios") no son
# muy confiables. Creo que la mejor manera de eliminar registros duplicados
# es hacerlo por conglomerado.

## Ahora, con el fin de asignarle a cada conglomerado una coordenada de la
## manera más natural posible, se revisarán cuantas coordenadas distintas tiene
## cada conglomerado por muestreo y en total

infys_depurado_2004_2014 %>%
  distinct(Conglomerado, Anio, X_WGS84_Dep, Y_WGS84_Dep) %>%
  group_by(Conglomerado, Anio) %>%
  tally() %>%
  ungroup() %>%
  pull(n) %>%
  table()
# Máximo 4 coordenadas distintas por muestreo de conglomerado.

infys_depurado_2004_2014 %>%
  distinct(Conglomerado, X_WGS84_Dep, Y_WGS84_Dep) %>%
  group_by(Conglomerado) %>%
  tally() %>%
  ungroup() %>%
  pull(n) %>%
  table()
# Máximo 4 coordenadas distintas por conglomerado en todos los años.

# Proporción de coordenadas distintas por conglomerado.
infys_depurado_2004_2014 %>%
  filter(!is.na(X_WGS84_Dep), !is.na(Y_WGS84_Dep)) %>%
  ddply(.(Conglomerado), function(df){
    resultado <- df %>%
      mutate(
        tipo_coordenadas = paste0(X_WGS84_Dep, "_", Y_WGS84_Dep) %>%
          as.factor() %>%
          as.numeric()
      ) %>%
      group_by(tipo_coordenadas) %>%
      tally() %>%
      ungroup() %>%
      mutate(
        proporcion = n / sum(n),
        cantidad_coordenadas_distintas = max(tipo_coordenadas)
      )
  }, .progress = "text") %>%
  ggplot(aes(x = Conglomerado, y = proporcion, fill = as.factor(tipo_coordenadas))) +
  geom_area(position = "stack") +
  facet_wrap(~as.factor(cantidad_coordenadas_distintas))
# Claramente las coordenadas están en la misma proporción para todos los
# conglomerados, por lo que simplemente hare la media (no ponderada) de las
# coordenadas no nulas para sacar la coordenada que asociaré a cada conglomerado.

## Revisando la siguiente cuestión: creo que hay registros
## duplicados en la base que tienen la misma "llave" "(Anio, cgl_sit_arb)".
infys_depurado_2004_2014 %>%
  select(
    -sitio,
    -X_WGS84_Dep,
    -Y_WGS84_Dep) %>%
  distinct() %>%
  nrow()

infys_depurado_2004_2014 %>%
  distinct(Anio, cgl_sit_arb) %>%
  nrow()
# Parecía que había varios registros repetidos, pero al quitar las etiquetas
# "aleatorias" de sitio y las coordenadas, vemos que no hay tantos

## Revisando que las coordenadas de los sitios sean razonables con respecto a
## su promedio, es decir, que sitios pertenecientes a un mismo conglomerado no
## estén dispersos por todo el país.

df_auxiliar_infys_depurado_2004_2014 <- infys_depurado_2004_2014 %>%
  # Asociando a cada conglomerado el promedio de sus coordenadas no nulas:
  filter(!is.na(X_WGS84_Dep), !is.na(Y_WGS84_Dep)) %>%
  rename(
    x = X_WGS84_Dep,
    y = Y_WGS84_Dep
  ) %>%
  distinct(Conglomerado, x, y) %>%
  group_by(Conglomerado) %>%
  mutate(
    # Usamos la mediana para evitar que sitios atípicos muevan el centroide del
    # conglomerado mucho
    x_mediana_conglomerado = median(x),
    y_mediana_conglomerado = median(y)
  ) %>%
  ungroup() %>%
  mutate(
    # La función distGeo() puede aceptar data frames del mismo tamaño como input
    # y calcular distancias entre los i-ésimos renglones de cada uno. O bien,
    # puede recibir un data frame y un único vector y calcular la distancia
    # entre cada elemento del data frame y el vector
    distancia_centroide_m = distGeo(
      data_frame("x" = x, "y" = y),
      data_frame(
        "x_mediana_conglomerado" = x_mediana_conglomerado,
        "y_mediana_conglomerado" = y_mediana_conglomerado
      )
    ),
    
    # Por curiosidad se calcula la distancia aproximada al centroide también
    distancia_centroide_m_aproximacion = 110000 * sqrt(
      (x - x_mediana_conglomerado) ^ 2 +
          (y - y_mediana_conglomerado) ^ 2)
      # 110000m = 1º
  )

df_auxiliar_infys_depurado_2004_2014 %>%
  pull(distancia_centroide_m) %>%
  quantile(probs = seq(0, 1, 0.01))
# Creo que un buen corte son 70m

# Por curiosidad, graficando ambas distancias:
ggplot(df_auxiliar_infys_depurado_2004_2014 %>%
    filter(distancia_centroide_m < 500),
  aes(x = distancia_centroide_m, y = distancia_centroide_m_aproximacion)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

## Revisando cuáles registros son los duplicados:
# Tomo las llaves naturales que siguen duplicadas después de eliminar las
# etiquetas aleatorias que les fueron pegadas a los registros y tomar registros
# distintos. De esta manera, todos los registros considerados para esta
# visualización corresponden a puros registros distintos que comparten llave
infys_depurado_2004_2014 %>%
  select(
    -sitio,
    -X_WGS84_Dep,
    -Y_WGS84_Dep
  ) %>%
  distinct() %>%
  group_by(Anio, cgl_sit_arb) %>%
  tally() %>%
  ungroup() %>%
  filter(n > 2) %>%
  inner_join(infys_depurado_2004_2014 %>%
    select(
      -sitio,
      -X_WGS84_Dep,
      -Y_WGS84_Dep
    ), by = c("Anio", "cgl_sit_arb")) %>%
  arrange(
    Anio,
    cgl_sit_arb
  ) %>%
  View()
# Podemos ver que los registros distintos con la misma llave natural corresponden
# a errores, y pueden ser eliminados fácilmente.

# Especies de interés:
# Dalbergia granadillo
# Dalbergia retusa
# Dalbergia stevensonii
# Swietenia macrophylla
# Swietenia mahogani
# Swietenia humilis

## Revisando especies correspondientes a los géneros de interés:
infys_depurado_2004_2014 %>%
  select(
    Genero,
    Especie,
    Genero_Orig_corregido,
    Especie_Orig_corregido,
    Genero_CTZ,
    Especie_CTZ,
    Genero_APG,
    Especie_APG
  ) %>%
  filter(
    # Dalbergias y Swietenias
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")|
      stri_detect_fixed(Genero_Orig_corregido, "erg")|
      stri_detect_fixed(Genero_Orig_corregido, "wie")|
      stri_detect_fixed(Especie_Orig_corregido, "steven")|
      stri_detect_fixed(Genero_CTZ, "erg")|
      stri_detect_fixed(Genero_CTZ, "wie")|
      stri_detect_fixed(Especie_CTZ, "steven")|
      stri_detect_fixed(Genero_APG, "erg")|
      stri_detect_fixed(Genero_APG, "wie")|
      stri_detect_fixed(Especie_APG, "steven")
  ) %>%
  unique() %>%
  arrange(Genero_CTZ, Especie_CTZ)

## Por último, revisar si para las especies de interés un "Arbol" puede
## constar de muchos "Registros", tomando la precaución de eliminar registros
## duplicados por medio de la llave natural (arb se refiere a registro en dicha
## llave).

infys_depurado_2004_2014 %>%
  
  # Eliminando registros duplicados por medio de la llave natural
  group_by(Anio, cgl_sit_arb) %>%
  summarise_all(.funs = first) %>%
  ungroup() %>%
  
  # Filtrando especies de interés
  filter(
    # Dalbergias y Swietenias
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")|
      stri_detect_fixed(Genero_Orig_corregido, "erg")|
      stri_detect_fixed(Genero_Orig_corregido, "wie")|
      stri_detect_fixed(Especie_Orig_corregido, "steven")|
      stri_detect_fixed(Genero_CTZ, "erg")|
      stri_detect_fixed(Genero_CTZ, "wie")|
      stri_detect_fixed(Especie_CTZ, "steven")|
      stri_detect_fixed(Genero_APG, "erg")|
      stri_detect_fixed(Genero_APG, "wie")|
      stri_detect_fixed(Especie_APG, "steven")
  ) %>%
  
  # Viendo cuántos registros tiene cada árbol perteneciente a las especies de
  # interés
  group_by(Anio, Conglomerado, Sitio, Arbol) %>%
  summarise(
    Genero = first(Genero),
    Especie = first(Especie),
    Genero_Orig_corregido = first(Genero_Orig_corregido),
    Especie_Orig_corregido = first(Especie_Orig_corregido),
    Genero_CTZ = first(Genero_CTZ),
    Especie_CTZ = first(Especie_CTZ),
    Genero_APG = first(Genero_APG),
    Especie_APG = first(Especie_APG),
    n = n()
  ) %>%
  filter(n >= 2) %>%
  View()
# Sí hay árboles con varios registros para nuestras especies de interés:
# Dalbergia granadillo y Swietenia humilis.

# Generando la tabla de observaciones limpias para el INFyS 2004-2014, por
# conglomerado, la cuál se construirá de la siguiente forma:
# 1. A cada conglomerado se asociará el promedio no ponderado de las coordenadas
# no nulas que tiene asociadas después de filtrar aquellas que disten de la 
# mediana del conglomerado algo mayor que 70m (percentil 99 de las distancias de
# los sitios a la mediana de su conglomerado correspondiente)
# 2. Para distinguir registros distintos en un mismo muestreo de conglomerado,
# se utilizará la llave (cgl_sit_arb, Anio). Recordar: registro != arbol.
# 3. Ya que calculamos el número de registros distintos, se agregarán los registros
# que corresponden al mismo árbol, para tener una tabla de árboles con información
# del muestreo de conglomerado al que pertenecen, así como su nombre científico.
# 4. Para cada especie de interés, asignar a un conglomerado el número de
# observaciones como el máximo del número de observaciones obtenido en un sólo
# año. Este approach es conservador ya que no estaremos duplicando observaciones
# que se vieron en varios años. Cabe destacar que este paso se realizará al
# final, puesto que hay que considerar los datos que corresponden al INFyS 2015
# y 2016 antes de agregar.

# Calculando una tabla preeliminar que contendrá las coordenadas de cada
# conglomerado.
coordenadas_conglomerados_infys_2004_2014 <- infys_depurado_2004_2014 %>%
  rename(
    x = X_WGS84_Dep,
    y = Y_WGS84_Dep
  ) %>%
  # Asociando a cada conglomerado el promedio de sus coordenadas no nulas, que
  # distan de la mediana algo menor o igual que 70m:
  filter(!is.na(x), !is.na(y)) %>%
  distinct(Conglomerado, x, y) %>%
  group_by(Conglomerado) %>%
  mutate(
    # Usamos la mediana para evitar que sitios atípicos muevan el centroide del
    # conglomerado mucho
    x_mediana_conglomerado = median(x),
    y_mediana_conglomerado = median(y)
  ) %>%
  ungroup() %>%
  mutate(
    distancia_centroide_m = distGeo(
      data_frame("x" = x, "y" = y),
      data_frame(
        "x_mediana_conglomerado" = x_mediana_conglomerado,
        "y_mediana_conglomerado" = y_mediana_conglomerado
      )
    )
  ) %>%
  filter(distancia_centroide_m <= 70) %>%
  group_by(Conglomerado) %>%
  # Calculando las coordenadas para cada conglomerado con los sitios elegidos
  summarise(
    longitud_conglomerado = mean(x),
    latitud_conglomerado = mean(y)
  ) %>%
  ungroup() %>%
  rename(
    conglomerado = Conglomerado
  )

glimpse(coordenadas_conglomerados_infys_2004_2014)
saveRDS(coordenadas_conglomerados_infys_2004_2014,
  rutas_archivos_productos["coordenadas_conglomerados_infys_2004_2014"])

# Estas observaciones aún no tienen coordenadas pues estas se asignarán considerando
# también las de sus "upm_id"'s equivalentes (cuando aplique)
observaciones_limpias_infys_2004_2014_arbolado <- infys_depurado_2004_2014 %>%
  # Distinguiendo registros distintos por medio de la llave natural
  group_by(Anio, cgl_sit_arb) %>%
  summarise_all(funs(first)) %>%
  ungroup() %>%
  # Agregando registros correspondientes al mismo "Arbol"
  group_by(Anio, Conglomerado, Sitio, Arbol) %>%
  summarise_all(funs(first)) %>%
  ungroup() %>%
  # Seleccionando especies de interés
  filter(
    stri_detect_fixed(Genero_APG, "erg")| # Dalbergias
      stri_detect_fixed(Genero_APG, "wie")| # Swietenias
      stri_detect_fixed(Especie_APG, "steven") # stevensonii que no sale ???
  ) %>%
  # Calculando el total de individuos por muestreo de conglomerado
  group_by(Conglomerado, Anio, Genero_APG, Especie_APG) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  select(
    conglomerado = Conglomerado,
    anio = Anio,
    genero = Genero_APG,
    especie = Especie_APG,
    n
  )

summary(observaciones_limpias_infys_2004_2014_arbolado)
glimpse(observaciones_limpias_infys_2004_2014_arbolado)
saveRDS(observaciones_limpias_infys_2004_2014_arbolado,
  rutas_archivos_productos["observaciones_limpias_infys_2004_2014_arbolado"])

  # Finalmente, calculando el máximo número de observaciones para calcular la
  # n final.
  
  # Corrección: el agregado por conglomerados se realizará hasta que se tengan
  # todos los datos del INFyS depurados.
  # group_by(Conglomerado, Genero_APG, Especie_APG) %>%
  # summarise(
  #   longitud_conglomerado = first(longitud_conglomerado),
  #   latitud_conglomerado = first(latitud_conglomerado),
  #   n = max(n_muestreo)
  # )

  # %>% ggplot(aes(x = longitud_conglomerado, y = latitud_conglomerado)) + geom_point()

################################################################################

### lista_hojas_infys_2015 ###

# Revisando todos los data frames que contiene la lista:
l_ply(names(lista_hojas_infys_2015), function(x){
  print(x)
  glimpse(lista_hojas_infys_2015[[x]])
})

# Las tablas de interés para este análisis son "Arbolado", "Sitios", "UPM".
# No se tomarán en cuenta los árboles pequeños de la tabla "Repoblado" porque
# estos no vienen en la tabla del INFyS 2004-2014.

## Analizando tabla por tabla:
lista_hojas_infys_2015[["Arbolado"]] %>%
  summary()

## Revisando si ArboladoID es una llave numérica para los árboles o registros.
lista_hojas_infys_2015[["Arbolado"]] %>%
  pull(ArboladoID) %>%
  unique() %>%
  length()
nrow(lista_hojas_infys_2015[["Arbolado"]])
# Difieren en 1, por lo que la puedo considerar una llave primaria de los registros.

## No se qué significa "EsSubmuestra", pero no hay especies de interés con esa
## variable verdadera, por lo que podemos ignorarlo.
lista_hojas_infys_2015[["Arbolado"]] %>%
  filter(EsSubmuestra == 1) %>%
  distinct(Genero, Especie) %>%
  View()

## Revisando las especies de interés_
lista_hojas_infys_2015[["Arbolado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  distinct(Genero, Especie) %>%
  arrange(Genero, Especie)
# Sólo hay datos de "Dalbergia granadillo", "Swietenia humilis" y
# "Swietenia macrophylla".

# Sin embargo notar que:
# D. stevensonii es una especie que se distribuye exclusivamente en bosques
# tropicales perennifolios en México, Guatemala y Belice. En México se encuentra
# únicamente en el estado de Chiapas (Breedlove, 1986).

## Creo que muchos registros se agregan en la misma "Rama" y muchas ramas se agregan
## en el mismo individuo. Revisando este hecho:

lista_hojas_infys_2015[["Arbolado"]] %>%
  nrow()

lista_hojas_infys_2015[["Arbolado"]] %>%
  distinct(SitioID, Rama) %>%
  nrow()

lista_hojas_infys_2015[["Arbolado"]] %>%
  distinct(SitioID, Individuo) %>%
  nrow()

# Los números indican que sí porque hay más ramas que individos.

# Número de ramas
lista_hojas_infys_2015[["Arbolado"]] %>%
  distinct(SitioID, Rama) %>%
  nrow()

# Ramas asociadas a dos o más individuos: muy pocas, el 0.1%
lista_hojas_infys_2015[["Arbolado"]] %>%
  group_by(SitioID, Rama) %>%
  summarise(
    min_individuo = min(Individuo),
    max_individuo = max(Individuo)
  ) %>%
  ungroup() %>%
  filter(min_individuo != max_individuo) %>%
  nrow()

# Revisando si hay registros pertenecientes a la misma "Rama" para las especies
# de interés:
lista_hojas_infys_2015[["Arbolado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")|
    stri_detect_fixed(Genero, "wie")|
    stri_detect_fixed(Especie, "steven")
  ) %>%
  group_by(SitioID, Rama) %>%
  tally() %>%
  filter(n >= 2)
# No hay individuos agrupados en "Ramas" para nuestras especies de interés

## Revisando si hay registros pertenecientes al mismo "Individuo" para las especies
## de interés

lista_hojas_infys_2015[["Arbolado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  group_by(SitioID, Individuo) %>%
  summarise(
    Genero = first(Genero),
    Especie = first(Especie),
    n = n()
  ) %>%
  filter(n >= 2) %>%
  View()
# Sí hay, por ello, agregaré por individuos para calcular la cantidad de observaciones
# por muestreo de conglomerado.

## Revisando las coordenadas de los sitios con respecto al promedio por UPMID

lista_hojas_infys_2015[["Sitios"]] %>%
  group_by(UPMID) %>%
  tally() %>%
  pull(n) %>%
  table(useNA = "always")
# perfecto, únicamente 4 sitios por UPMID

lista_hojas_infys_2015[["Sitios"]] %>%
  mutate(
    x = as.numeric(X),
    y = as.numeric(Y)
  ) %>%
  group_by(UPMID) %>%
  mutate(
    x_media_conglomerado = mean(x),
    y_media_conglomerado = mean(y)
  ) %>%
  ungroup() %>%
  mutate(
    distancia_x_m = abs(x - x_media_conglomerado) * 110000,
    distancia_y_m = abs(y - y_media_conglomerado) * 110000
    # 110000m = 1º
  ) %>%
  summary()
# Al parecer todos los sitios asociados a un mismo conglomerado tienen las mismas
# coordenadas en este caso.

## Generando la tabla de observaciones de arbolado:

# Columnas importantes de la tabla "Arbolado":
# UPMID: (UPMID, Sitio) es una llave foránea del muestreo de sitio (donde se
# encuentran las coordenadas correspondientes), también UPMID sirve para unir
# con la tabla "UPM" y obtener el año de muestreo.
# ArboladoID: llave numérica de la tabla
# Sitio: para poder asociar un registro con sus coordenadas correspondientes
# Individuo: número de árbol. Recordar que un árbol puede constar de 1 o más registros.
# Genero/ Especie.

# Columnas importantes de la tabla "Sitios"
# UPMID, sitio, X, Y

# Columas importantes de la tabla "UPM":
# UPMID, FechaInicio FechaFin

# Calculando una tabla preeliminar que contendrá las coordenadas de cada
# conglomerado, calculadas promediando las coordenadas de cada uno de los sitios
# que conforman cada uno de ellos.
coordenadas_upm_ids_infys_2015 <- lista_hojas_infys_2015[["UPM"]] %>%
  # Asociando a cada conglomerado el promedio de sus coordenadas no nulas. Por
  # consistencia con "observaciones_limpias_infys_2004_2014", se asociarán a
  # cada arbol las coordenadas promedio de los sitios del conglomerado al que
  # pertenecen.
  mutate(
    anio_inicio = year(FechaInicio),
    anio_fin = year(FechaFin)
  ) %>%
  select(
    UPMID,
    anio_inicio,
    anio_fin
  ) %>%
  inner_join(lista_hojas_infys_2015[["Sitios"]] %>%
      mutate(
        UPMID = as.numeric(UPMID)
      ) %>%
      mutate(
        X = as.numeric(X),
        Y = as.numeric(Y)
      ) %>%
      select(
        UPMID,
        #sitio, solo para revisión preliminar, que da 4 sitios por UPMID
        X,
        Y
      ), by = "UPMID") %>%
  group_by(UPMID) %>%
  summarise(
    longitud_upm_id = mean(X, na.rm = TRUE),
    latitud_upm_id = mean(Y, na.rm = TRUE)#,
    #anio_inicio = first(anio_inicio),
    #anio_fin = first(anio_fin)
  ) %>%
  ungroup() %>%
  rename(upm_id = UPMID)

coordenadas_upm_ids_infys_2015 %>%
  glimpse()
coordenadas_upm_ids_infys_2015 %>%
  distinct(upm_id) %>%
  nrow()
# Perfecto, los números de registros coinciden

saveRDS(coordenadas_upm_ids_infys_2015,
  rutas_archivos_productos["coordenadas_upm_ids_infys_2015"])

observaciones_limpias_infys_2015_arbolado <- lista_hojas_infys_2015[["Arbolado"]] %>%
  
  # Corrigiendo un error
  mutate(
    Especie = ifelse(Especie == "macphylla", "macrophylla", Especie)
  ) %>%
  
  select(
    upm_id = UPMID,
    Sitio,
    ArboladoID,
    Individuo,
    Genero,
    Especie
  ) %>%
  # Filtrando especies de interés
  filter(
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  # Tomando a los distintos individuos como observaciones.
  distinct(upm_id, Sitio, Individuo, .keep_all = TRUE) %>%
  # Calculando las cuentas por muestreo de conglomerado y especie
  group_by(upm_id, Genero, Especie) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  transmute(
    upm_id = upm_id,
    anio = 2015,
    genero = Genero,
    especie = Especie,
    n
  )

glimpse(observaciones_limpias_infys_2015_arbolado)
summary(observaciones_limpias_infys_2015_arbolado)
saveRDS(observaciones_limpias_infys_2015_arbolado,
  rutas_archivos_productos["observaciones_limpias_infys_2015_arbolado"])

################################################################################

# Julián está interesado también en los datos de árboles pequeños, es decir, en
# los registros pertenecientes a la tabla "Repoblado".

lista_hojas_infys_2015[["Repoblado"]] %>%
  glimpse()

# Revisando que efectivamente haya 1 registro por sitio para cada género y especie,
# es decir, la tabla anterior debe tener a lo más cuatro registros por "UPMID",
# "Genero" y "Especie"
lista_hojas_infys_2015[["Repoblado"]] %>%
  distinct(UPMID, sitio, Genero, Especie) %>%
  group_by(UPMID, Genero, Especie) %>%
  tally() %>%
  pull(n) %>%
  table(useNA = "always")
# Perfecto!

observaciones_limpias_infys_2015_repoblado <- lista_hojas_infys_2015[["Repoblado"]] %>%
  select(
    upm_id = UPMID,
    # Aunque en teoría no nos interesa el sitio porque al final las
    # coordenadas las tenemos por conglomerado, en la práctica esta información
    # nos sirve como control de calidad.
    # sitio,
    Genero,
    Especie,
    Frecuencia025150,
    Frecuencia151275,
    Frecuencia275
  ) %>%
  filter(
    # Dalbergias y Swietenias
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  # Calculando cuentas totales por muestra de conglomerado
  gather("variable", "cuenta", contains("Frecuencia"), na.rm = TRUE) %>%
  mutate(
    cuenta = as.numeric(cuenta)
  ) %>%
  group_by(upm_id, Genero, Especie) %>%
  summarise(
    n = sum(cuenta)
  ) %>%
  ungroup() %>%
  transmute(
    upm_id = upm_id,
    anio = "2015",
    genero = Genero,
    especie = Especie,
    n = n
  )

glimpse(observaciones_limpias_infys_2015_repoblado)
saveRDS(observaciones_limpias_infys_2015_repoblado,
  rutas_archivos_productos["observaciones_limpias_infys_2015_repoblado"])
  
################################################################################

### lista_hojas_infys_2016 ###

# Revisando todos los data frames que contiene la lista:
l_ply(names(lista_hojas_infys_2016), function(x){
  print(x)
  glimpse(lista_hojas_infys_2016[[x]])
})

# Las tablas de interés para este análisis son "Arbolado", "Sitios", "UPM".
# No se tomarán en cuenta los árboles pequeños de la tabla "Repoblado" porque
# estos no vienen en la tabla del INFyS 2004-2014.

## Revisando si "Numero" es una llave del muestreo del sitio:
lista_hojas_infys_2016[["Arbolado"]] %>%
  distinct(UPMID, Numero) %>%
  nrow()

lista_hojas_infys_2016[["Arbolado"]] %>%
  distinct(Numero) %>%
  nrow()
# Claramente Numero por sí mismo no es una llave natural del sitio

## Revisando cuántos "Numero"s distintos hay por "UPMID"
lista_hojas_infys_2016[["Arbolado"]] %>%
  distinct(UPMID, Numero) %>%
  group_by(UPMID) %>%
  tally() %>%
  summary()

## Revisando si ("UPMID", "Numero", "Consecutivo") se puede considerar como una
## llave natural para los registros de la tabla "Arbolado"
lista_hojas_infys_2016[["Arbolado"]] %>%
  distinct(UPMID, Numero, Consecutivo) %>%
  nrow()

lista_hojas_infys_2016[["Arbolado"]] %>%
  distinct(UPMID, Numero, Consecutivo, DiametroNormal, AlturaTotal) %>%
  nrow()
# Las dos cuentas anteriores dan casi la misma cantidad de registros, lo que
# refuerza la hipótesis de que "Consecutivo" es una cuenta de registros distintos.

## Revisando las especies de interés_
lista_hojas_infys_2016[["Arbolado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  distinct(Genero, Especie) %>%
  arrange(Genero, Especie)
# Sólo hay datos de "Dalbergia granadillo", "Swietenia humilis" y
# "Swietenia macrophylla".

## Revisando las relaciones entre "Consecutivo", "Individuo" y "Rama"

# Creo que muchos registros se agregan en la misma "Rama" y muchas ramas se agregan
# en el mismo individuo. Revisando este hecho:
lista_hojas_infys_2016[["Arbolado"]] %>%
  nrow()

lista_hojas_infys_2016[["Arbolado"]] %>%
  distinct(UPMID, Numero, Consecutivo) %>%
  nrow()
# Dan casi igual, pero recordemos que las diferencias se pueden referir a registros
# duplicados, como se vio al la cantidad de registros que difieren en
# ("UPMID", "Numero", "Consecutivo") con los que difieren en
# ("UPMID", "Numero", "Consecutivo", "DiametroNormal", "AlturaTotal")

lista_hojas_infys_2016[["Arbolado"]] %>%
  distinct(UPMID, Numero, Rama) %>%
  nrow()

lista_hojas_infys_2016[["Arbolado"]] %>%
  distinct(UPMID, Numero, Individuo) %>%
  nrow()
# Parece que sí porque hay menos individuos que ramas

# Ramas asociadas a dos o más individuos: muy pocas, sólo 5. Esto revela que
# las ramas pertenecen a uno y sólo un individuo (al menos en teoría)
lista_hojas_infys_2016[["Arbolado"]] %>%
  group_by(UPMID, Numero, Rama) %>%
  summarise(
    min_individuo = min(Individuo),
    max_individuo = max(Individuo)
  ) %>%
  ungroup() %>%
  filter(min_individuo != max_individuo) %>%
  nrow()

# Revisando si hay registros pertenecientes a la misma "Rama" para las especies
# de interés:
lista_hojas_infys_2016[["Arbolado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  group_by(UPMID, Numero, Rama) %>%
  tally() %>%
  filter(n >= 2) # Sólo hay una rama compuesta de 2 o más registros

# Revisando si hay registros pertenecientes al mismo "Individuo" para las especies
# de interés

lista_hojas_infys_2016[["Arbolado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  group_by(UPMID, Numero, Individuo) %>%
  summarise(
    Genero = first(Genero),
    Especie = first(Especie),
    n = n()
  ) %>%
  filter(n >= 2) %>%
  View()
# Sí hay, por ello, agregaré por individuos para calcular la cantidad de observaciones
# por muestreo de conglomerado.

## Ahora, de la tabla de "Sitios" se revisarán si ("UPMID", "Numero") es una
## llave natural:
lista_hojas_infys_2016[["Sitios"]] %>%
  distinct(UPMID, Numero) %>%
  nrow()

lista_hojas_infys_2016[["Sitios"]] %>%
  nrow()

# Parece que no: hay que revisar a fondo qué está pasando
lista_hojas_infys_2016[["Sitios"]] %>%
  distinct(UPMID, Numero) %>%
  nrow()

lista_hojas_infys_2016[["Sitios"]] %>%
  distinct(UPMID, Numero, Long, Lat) %>%
  nrow()
# Hay poca discrepancia, no usar X, Y porque en esas columnas hay registros
# duplicados con distinto redondeo.

## Revisando los registros de "Sitios" con los mismos valores en "UPMID" y "Numero"
## pero con distintas coordenadas:

lista_hojas_infys_2016[["Sitios"]] %>%
  distinct(UPMID, Numero, Long, Lat, Datum) %>%
  group_by(UPMID, Numero) %>%
  mutate(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  arrange(UPMID, Numero) %>%
  View()
# Hay bastante discrepancia entre coordenadas para un mismo sitio, hay que tener
# cuidado.

# Ahora obteniendo más datos de esos sitios para ver si nos pueden dar más idea
# acerca de qué hacer con ellos.

lista_hojas_infys_2016[["Sitios"]] %>%
  distinct(UPMID, Numero, Long, Lat, Datum) %>%
  # Todos son WGS84
  group_by(UPMID, Numero) %>%
  tally() %>%
  ungroup() %>%
  filter(n >= 2) %>%
  inner_join(lista_hojas_infys_2016[["Sitios"]], by = c("UPMID", "Numero")) %>%
  View()

## Comparando las coordenadas de los sitios en la anterior tabla con la de todos
## los otros sitios que pertenecen al mismo conglomerado

lista_hojas_infys_2016[["Sitios"]] %>%
  # Eliminando sitios con coordenadas 0:
  filter(X != 0, Y != 0) %>%
  # Eliminando duplicados por redondeo
  group_by(UPMID, Numero, Long, Lat) %>%
  summarise(
    x = mean(as.numeric(X), na.rm = TRUE),
    y = mean(as.numeric(Y), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Calculando medianas por muestreo de conglomerado. Esto lo hacemos para eliminar
  # sitios con coordenadas atípicas antes de asignar a cada conglomerado el
  # promedio de las coordenadas de sus sitios.
  group_by(UPMID) %>%
  mutate(
    x_mediana_conglomerado = median(x, na.rm = TRUE),
    y_mediana_conglomerado = median(y, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    distancia_centroide_m = distGeo(
      data_frame("x" = x, "y" = y),
      data_frame(
        "x_mediana_conglomerado" = x_mediana_conglomerado,
        "y_mediana_conglomerado" = y_mediana_conglomerado
      )
    )
  ) %>%
  # Filtrando NA's y NaN's
  filter(!is.na(distancia_centroide_m)) %>%
  pull(distancia_centroide_m) %>%
  quantile(probs = seq(0, 1, 0.01))
# 70m también se ve como un buen corte.

## Revisando si "UPMID" es una llave de la tabla "UPM"
nrow(lista_hojas_infys_2016[["UPM"]])

lista_hojas_infys_2016[["UPM"]] %>%
  distinct(UPMID) %>%
  nrow()
# Hay fallas

# Revisando qué está fallando
lista_hojas_infys_2016[["UPM"]] %>%
  group_by(UPMID) %>%
  tally() %>%
  filter(n >= 2) %>%
  inner_join(lista_hojas_infys_2016[["UPM"]], by = "UPMID") %>%
  View()
# Vemos que hay tanto conglomerados duplicados como remuestreos, por lo que
# debemos distinguirlos.

# Tratando de distinguir registros duplicados de remuestreos verdaderos.
# ¿Como hacerlo?: creo que las columnas de fecha inicial y fecha final de muestreo
# son la clave. Como los nombres de las columnas son incorrectos, estas columnas
# se llaman "Sitio" y "Numero"
lista_hojas_infys_2016[["UPM"]] %>%
  distinct(UPMID, Sitio, Numero) %>%
  group_by(UPMID) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  arrange(UPMID) %>%
  View()
# Son sólo 32 conglomerados que eliminaré, en vista de que existe la posibilidad
# de pseudoreplicados de las observaciones de árboles si no es posible distinguir
# cuáles pertenecen a qué muestreo.

# Revisando registros a eliminar de las especies que nos interesan:
lista_hojas_infys_2016[["UPM"]] %>%
  distinct(UPMID, Sitio, Numero) %>%
  group_by(UPMID) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  inner_join(lista_hojas_infys_2016[["Arbolado"]], by = "UPMID") %>%
  filter(
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  View()
# Sólo es un registro que claramente es duplicado, no es necesario el esfuerzo.

## Generando la tabla de observaciones:

# Columnas importantes de la tabla "Arbolado":
# UPMID: llave del muestreo de conglomerado
# Numero: ("UPMID", "Numero") es una llave de cada muestreo de sitio.
# Consecutivo: ("UPMID", "Numero", "Consecutivo") es una llave de la tabla "Arbolado"
# Individuo: número de árbol. Recordar que un árbol puede constar de 1 o más registros.
# Genero/ Especie.

# Columnas importantes de la tabla "Sitios"
# UPMID, sitio, X, Y

# Columas importantes de la tabla "UPM": ninguna, ya que los registros que pudieran
# haber sido muestreados 2+ veces (no identificables a primera vista porque en
# la tabla de Arbolado sólo vienen distintos por Conglomerado y no por muestreo),
# sólo es un registro que aparte está duplicado. Si queremos ser muy precisos,
# podemos extraer el año de "Sitio

coordenadas_upm_ids_infys_2016 <- lista_hojas_infys_2016[["Sitios"]] %>%
  # Eliminando sitios con coordenadas 0:
  filter(X != 0, Y != 0) %>%
  # Eliminando duplicados por redondeo
  group_by(UPMID, Numero, Long, Lat) %>%
  summarise(
    x = mean(as.numeric(X), na.rm = TRUE),
    y = mean(as.numeric(Y), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Calculando medianas por muestreo de conglomerado. Esto lo hacemos para eliminar
  # sitios con coordenadas atípicas antes de asignar a cada conglomerado el
  # promedio de las coordenadas de sus sitios.
  group_by(UPMID) %>%
  mutate(
    x_mediana_upm_id = median(x, na.rm = TRUE),
    y_mediana_upm_id = median(y, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    distancia_centroide_m = distGeo(
      data_frame("x" = x, "y" = y),
      data_frame(
        "x_mediana_upm_id" = x_mediana_upm_id,
        "y_mediana_upm_id" = y_mediana_upm_id
      )
    )
  ) %>%
  # Filtrando NA's y NaN's, así como sitios que disten de la mediana algo mayor
  # a 70m
  filter(!is.na(distancia_centroide_m), distancia_centroide_m <= 70) %>%
  # Con los sitios que nos quedan, calcular las coordenadas de los conglomerados.
  # Notar que si un conglomerado no tuvo sitios asociados, simplemente es
  # eliminado por default.
  group_by(UPMID) %>%
  summarise(
    longitud_upm_id = mean(x, na.rm = TRUE),
    latitud_upm_id = mean(y, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(
    upm_id = UPMID
  )

glimpse(coordenadas_upm_ids_infys_2016)
saveRDS(coordenadas_upm_ids_infys_2016,
  rutas_archivos_productos["coordenadas_upm_ids_infys_2016"])

observaciones_limpias_infys_2016_arbolado <- lista_hojas_infys_2016[["Arbolado"]] %>%
  # Distinguiendo registros distintos por medio de la llave compuesta
  group_by(UPMID, Numero, Consecutivo) %>%
  summarise_all(funs(first)) %>%
  ungroup() %>%
  # Agregando registros correspondientes al mismo "Individuo"
  group_by(UPMID, Numero, Individuo) %>%
  summarise_all(funs(first)) %>%
  ungroup() %>%
  # Seleccionando especies de interés
  filter(
    stri_detect_fixed(Genero, "erg")| # Dalbergias
      stri_detect_fixed(Genero, "wie")| # Swietenias
      stri_detect_fixed(Especie, "steven") # stevensonii que no sale ???
  ) %>%
  # Calculando el total de individuos por muestreo de conglomerado
  group_by(UPMID, Genero, Especie) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  transmute(
    upm_id = UPMID,
    anio = 2016,
    genero = Genero,
    especie = Especie,
    n
  )

glimpse(observaciones_limpias_infys_2016_arbolado)
View(observaciones_limpias_infys_2016_arbolado)
saveRDS(observaciones_limpias_infys_2016_arbolado,
  rutas_archivos_productos["observaciones_limpias_infys_2016_arbolado"])

################################################################################

# Julián está interesado también en los datos de árboles pequeños, es decir, en
# los registros pertenecientes a la tabla "Repoblado".

lista_hojas_infys_2016[["Repoblado"]] %>%
  glimpse()

# Probando si la variable "CveCanalillo" se puede considerar como un indicador
# del sitio muestreado para cada valor de "UPMID"
lista_hojas_infys_2016[["Repoblado"]] %>%
  distinct(UPMID, CveCanalillo) %>%
  group_by(UPMID) %>%
  tally() %>%
  ungroup() %>%
  summary()
# Claramente no, por ello, creo que no existe un indicador fiel que nos diga
# que no hay registros duplicados. Entonces lo tendré que revisar a ojo para las
# especies de interés:

lista_hojas_infys_2016[["Repoblado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")| # Dalbergias
      stri_detect_fixed(Genero, "wie")| # Swietenias
      stri_detect_fixed(Especie, "steven") # stevensonii que no sale ???
  ) %>%
  arrange(UPMID, CveCanalillo) %>%
  View()
# Parece que no, los datos son seguros de usar.

# Revisando la cantidad de renglones para ver si sigue bien todo:
lista_hojas_infys_2016[["Repoblado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")| # Dalbergias
      stri_detect_fixed(Genero, "wie")| # Swietenias
      stri_detect_fixed(Especie, "steven") # stevensonii que no sale ???
  ) %>%
  nrow()

lista_hojas_infys_2016[["Repoblado"]] %>%
  filter(
    stri_detect_fixed(Genero, "erg")| # Dalbergias
      stri_detect_fixed(Genero, "wie")| # Swietenias
      stri_detect_fixed(Especie, "steven") # stevensonii que no sale ???
  ) %>%
  # Quitando columnas de cuentas que posiblemente puedan ocultar registros que
  # en realidad son duplicados
  select(-CveCanalillo, -Ancho, -Profundidad) %>%
  distinct() %>%
  nrow()
# Diferencias mínimas, parece que todos los registros son genuinos (no hay
# duplicados)

observaciones_limpias_infys_2016_repoblado <- lista_hojas_infys_2016[["Repoblado"]] %>%
  select(
    upm_id = UPMID,
    Genero,
    Especie,
    Frecuencia025150,
    Frecuencia151275,
    Frecuencia275
  ) %>%
  filter(
    # Dalbergias y Swietenias
    stri_detect_fixed(Genero, "erg")|
      stri_detect_fixed(Genero, "wie")|
      stri_detect_fixed(Especie, "steven")
  ) %>%
  # Calculando cuentas totales por muestra de conglomerado
  gather("variable", "cuenta", contains("Frecuencia"), na.rm = TRUE) %>%
  mutate(
    cuenta = as.numeric(cuenta)
  ) %>%
  group_by(upm_id, Genero, Especie) %>%
  summarise(
    n = sum(cuenta)
  ) %>%
  ungroup() %>%
  transmute(
    upm_id = upm_id,
    anio = "2016",
    genero = Genero,
    especie = Especie,
    n = n
  )

glimpse(observaciones_limpias_infys_2016_repoblado)
saveRDS(observaciones_limpias_infys_2016_repoblado,
  rutas_archivos_productos["observaciones_limpias_infys_2016_repoblado"])
