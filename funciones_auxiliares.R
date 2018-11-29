library("plyr")
library("dplyr")
library("tidyr")
library("readxl")
library("lubridate")
library("stringi")
library("forcats") # Para trabajar con factores, en "genera_llave"
library("purrr") # Para "genera_tabla_2"

################################################################################
# Funciones auxiliares sobre listas de data frames
################################################################################

# Función auxiliar para leer una hoja determinada de cada uno de los archivos de
# Excel en una carpeta específica y guardar los data frames resultantes en una
# lista:
# ruta_carpeta_origen: ruta de la carpeta que contiene los archivos de excel.
# hoja: hoja a seleccionar de cada archivo de Excel en la carpeta (la misma para
# todos, ya que se supone tienen el mismo formato).
# La función regresa una lista donde cada elemento es el data frame correspondiente
# a un Excel en la carpeta. Todas las columnas son de tipo caracter para evitar
# introducir NA's por incompatibilidad de datos. Cada uno de estos data frames
# contiene una columna llamada "archivo_origen" que contiene el string
# "nombre_carpeta_nombre_excel" para cada registro.
# Notas:
# 1. Un elemento en la carpeta especificada deben ser un archivo de Excel si y
# sólo si su nombre empieza con una letra.
# 2. Los nombres de dichos archivos sólo deben consistir en caracteres
# alfanuméricos y guion bajo para que los nombres sean asignados correctamente
# 3. Todos los exceles deben contener una columna llamada "Serie", que enliste los
# renglones no vacíos.
lee_exceles <- function(ruta_carpeta_origen, hoja){
  
  # Obteniendo el nombre de la carpeta origen para formar el campo "archivo_origen"
  # y el nombre del archivo de warnings:
  nombre_carpeta_origen <- basename(ruta_carpeta_origen)
  
  # Formando la ruta a cada uno de los exceles en "ruta_carpeta_origen" (supuesto 1):
  rutas_exceles <- list.files(ruta_carpeta_origen,
    full.names = TRUE,
    pattern = "^[[:alpha:]].*")
  
  # Leyendo cada archivo de Excel:
  lista_df_exceles <- llply(rutas_exceles, function(x){
    
    # Obteniendo el nombre del Excel para usarlo en el print y columna: "archivo_origen"
    nombre_archivo <- (basename(x) %>%
        stri_match_first_regex("(\\w+).xlsx"))[,2]
    
    # Primero se leerá cada archivo, no importando warnings, con el fin de saber
    # el número de columnas de cada uno y poder especificar que todas sean leídas
    # como texto (y así evitar warnings la segunda vez que se lean)
    aux <- suppressWarnings(read_excel(x, sheet = hoja))
    
    # Leyendo el Excel por segunda vez, especificando las columnas como texto.
    datos <- read_excel(x, sheet = hoja, col_types = rep("text", ncol(aux))) %>%
      # Supuesto 3
      filter(!is.na(Serie)) %>%
      # Agregando el archivo origen a los datos correspondientes
      mutate(
        archivo_origen = nombre_carpeta_origen %>%
          paste0("_", nombre_archivo)
      )
    
    # Generando mensaje:
    paste0(nombre_archivo, ": ", nrow(datos), " x ", ncol(datos)) %>%
      print()
    
    return(datos)
  })
  
  # Asignando nombres de los archivos a la lista correspondiente:
  names(lista_df_exceles) <- nombre_carpeta_origen %>%
    paste0("_", (basename(rutas_exceles) %>%
        stri_match_first_regex("(\\w+).xlsx"))[,2])

  return(lista_df_exceles)
}

################################################################################

# Función auxiliar para que, dada una lista de data frames, se haga un data frame
# donde Aij = 1 si la j-ésima se encuentra en el i-ésimo data frame y 0 e.o.c.
# lista_df: lista nombrada de data frames
# La función regresa el data frame descrito anteriormente
# Nota: Considerar explorar el data frame resultante con las funciones glimpse(),
# encuentra_columnas()", y colSums().
crea_resumen_columnas_df <- function(lista_df){
  resultado <- ldply(lista_df, function(df){
    
    # Para df, creando un data frame con un columna llamada "nombre_columna" que
    # contendrá los nombres de las columnas en df
    resultado <- data_frame(nombre_columna = colnames(df))
    return(resultado)
  }) %>%
    select(
      # Por lo siguiente se requiere que la lista de data frames esté nombrada:
      nombre_df = .id,
      nombre_columna
    ) %>%
    # Generando una columna auxiliar para llenar un spread de nombres de columnas
    # para cada excel
    mutate(
      uno = 1
    ) %>%
    spread(key = nombre_columna, value = uno, fill = 0)
  
  return(resultado)
}

################################################################################

# Función auxiliar para renombrar una columna en todos los data frames de una lista.
# Para usarla, un supuesto es que la columna "nombre_nuevo" no existe si existe
# "nombre_anterior":
# lista_df: lista de data frames
# nombre_anterior: nombre de la columna a renombrar
# nombre_nuevo: nombre nuevo de la columna
# La función regresa una lista de data frames, por lo que se puede pipear.
# Si la columna "nombre_anterior" no existe en un data frame de la lista, la
# función no realiza cambios en ese data frame.
renombra_columna <- function(lista_df, nombre_anterior, nombre_nuevo){
  llply(lista_df, function(df, nombre_anterior, nombre_nuevo){
    # Obteniendo nombres de las columnas del df
    nombres_columnas <- names(df)
    # Renombrando columna
    nombres_columnas[nombres_columnas == nombre_anterior] <- nombre_nuevo
    # Sustituyendo nombres en el df
    names(df) <- nombres_columnas
    return(df)
  }, nombre_anterior, nombre_nuevo)
}

################################################################################

# Funcion auxiliar para pasar a minúsculas todos los nombres de las columnas de
# los df en una lista:
# lista_df: lista de data frames
# La función regresa una lista de data frames, por lo que se puede pipear.
renombra_columnas_minusculas <- function(lista_df){
  llply(lista_df, function(df){
    names(df) <- names(df) %>%
      tolower()
    return(df)
  })
}

################################################################################

# Función auxiliar para hacer un inner join de cada uno de los data frames en una
# lista, con otro data frame:
# lista_df: lista de data frames
# df: data frame con el que se hará el inner join de cada uno de los data frames
# en la lista.
# llaves_union: parámetro pasado a "dplyr::inner_join" para especificar los campos
# sobre los cuáles se realizará el join.
# La función regresa una lista de los resultados de cada uno de los joins.
# Si cambia el número de renglones de un data frame en la lista después del join
# manda un warning de que se pudieron haber introducido artefactos.
inner_join_lista <- function(lista_df, df, llaves_union){
  n <- length(lista_df)
  lista_joins <- llply(1:n, function(i){
    resultado <- inner_join(lista_df[[i]], df, by = llaves_union)
    
    if(nrow(resultado) != nrow(lista_df[[i]]))
      warning(
        paste0("Cambio en número de renglones después del join, para el df: ",
          names(lista_df)[i])
      )

    return(resultado)
  })
  
  # Agregando nombres a "lista_joins"
  names(lista_joins) <- names(lista_df)
  
  return(lista_joins)
}

################################################################################

# Función que recibe una lista de data frames y una de catálogos (que también son
# data frames) y, por medio de un vector que especifica qué columnas de qué
# data frames corresponden a qué columnas de qué catálogos, permite revisar los
# valores en cada data frame que no corresponden con los de catálogos.
# lista_df: lista nombrada de data frames
# lista_catalogos: lista nombrada de catalogos
# relacion_columnas_catalogo: vector con nombres y entradas de la forma:
# c("df.columna" = "catalogo.columna") que indicará qué data frame (y columna) de
# "lista_df" debe corresponder con qué data frame (y columna) de "lista_catalogos".
# La función regresa un data frame donde cada registro tiene los campos:
# "tabla", "campo", "catálogo" "serie" y "valor" y corresponde a un registro
# que no contiene un valor en el catálogo correspondiente.
# Notas:
# 1. Las columnas de cada data frame en "lista_df" deben contener sólo caracteres
# alfanuméricos y guiones bajos.
# 2. Cada data frame en "lista_df" debe contener una columna llamada "serie".
# 3. Las entradas de "relacion_columnas_catalogo" puede ser especificadas de la
# manera c(".columna" = "catalogo.columna"), lo que significa que esa columna en
# cada data frame debe estar en el catálogo correspondiente.
revisa_columnas_catalogos <- function(lista_df, lista_catalogos, relacion_columnas_catalogo){
  
  # Calculando la longitud de "lista_df" que será de utilidad posteriormente
  numero_df <- length(lista_df)
  
  # Transformando "relacion_columnas_catalogo" en un data frame para su fácil
  # manipulación:
  relacion_columnas_catalogo_df <- data_frame(
    df.columna = names(relacion_columnas_catalogo),
    catalogo.columna = relacion_columnas_catalogo
    ) %>%
    # Quedándonos sólo con renglones distintos:
    distinct() %>%
    separate(df.columna, into = c("nombre_df", "nombre_columna_df"), sep = "\\.") %>%
    separate(catalogo.columna, into = c("nombre_catalogo", "nombre_columna_catalogo"), sep = "\\.")
  
  # Verificando que todos los data frames y catálogos estén en las listas
  # correspondientes:
  data_frames_considerados <- relacion_columnas_catalogo_df %>%
    # Filtrando los casos donde no se especifica un data frame
    filter(nombre_df != "") %>%
    pull(nombre_df) %>%
    unique() 
  
  catalogos_considerados <- relacion_columnas_catalogo_df %>%
    pull(nombre_catalogo) %>%
    unique() 
  
  # Si hay data frames escritos en "relacion_columnas_catalogo" que no se encuentran
  # en "lista_df", entonces parar la ejecución:
  if(sum(data_frames_considerados %in% names(lista_df)) != length(data_frames_considerados)){
    "Hay data frames especificados en 'relacion_columnas_catalogo' que no se " %>%
      paste0("encuentran en 'lista_df'") %>%
      stop()
  }
  
  # Si hay catálogos escritos en "relacion_columnas_catalogo" que no se encuentran
  # en "lista_catálogos", entonces parar la ejecución:
  if(sum(catalogos_considerados %in% names(lista_catalogos)) != length(catalogos_considerados)){
    "Hay catálogos especificados en 'relacion_columnas_catalogo' que no se " %>%
      paste0("encuentran en 'lista_catalogos'") %>%
      stop()
  }
  
  # Ahora se pasará a interpretar cada renglón de "relacion_columnas_catalogo_df"
  # para realizar los joins correspondientes.
  resultado <- apply(relacion_columnas_catalogo_df, 1, function(x){
    
    # Obteniendo valores de cada renglón del data frame
    nombre_df <- x["nombre_df"]
    nombre_columna_df <- x["nombre_columna_df"]
    nombre_catalogo <- x["nombre_catalogo"]
    nombre_columna_catalogo <- x["nombre_columna_catalogo"]
    
    # Si el data frame es especificado explícitamente:
    if(nombre_df != ""){
      
      df <- lista_df[[nombre_df]]
      catalogo <- lista_catalogos[[nombre_catalogo]]
      
      # Si la columna correspondiente no se encuentra en el data frame especificado
      if(!(nombre_columna_df %in% colnames(df))){
        paste0("La columna ", nombre_columna_df, " no se encuentra en el data frame ",
          nombre_df) %>%
          stop()
      }
      
      # Si la columna correspondiente no se encuentra en el catálogo especificado
      if(!(nombre_columna_catalogo %in% colnames(catalogo))){
        paste0("La columna ", nombre_columna_catalogo, " no se encuentra en el catálogo ",
          nombre_catalogo) %>%
          stop()
      }
      
      # Si se pasaron los dos filtros anteriores, hacer el anti join para
      # obtener la información de los registros del data frame considerado, que
      # para la columna considerada, no tienen valores en el catálogo
      
      # Generando la condición de unión para el anti_join
      condicion <- nombre_columna_catalogo
      names(condicion) <- nombre_columna_df
      
      # Generando la expresión para el select_.
      # Se generarán las columnas "tabla", "campo" y "catalogo"
      expresion_select <- list("tabla", "campo", "catalogo", "serie", nombre_columna_df)
      names(expresion_select) <- c("tabla", "campo", "catalogo", "serie", "valor")
      
      resultado <- df %>%
        anti_join(catalogo, by = condicion) %>%
        mutate(
          tabla = nombre_df,
          campo = nombre_columna_df,
          catalogo = nombre_catalogo
        ) %>%
        # Seleccionando columnas de interés
        select_(.dots = expresion_select)
      
    } else{
      
      catalogo <- lista_catalogos[[nombre_catalogo]]
      
      # Generando la condición de unión para el anti_join
      condicion <- nombre_columna_catalogo
      names(condicion) <- nombre_columna_df
      
      # Generando la expresión para el select_.
      # Se generarán las columnas "tabla", "campo" y "catalogo"
      expresion_select <- list("tabla", "campo", "catalogo", "serie", nombre_columna_df)
      names(expresion_select) <- c("tabla", "campo", "catalogo", "serie", "valor")
      
      # Aplicar el anti_join a cada elemento en "lista_df"
      resultado <- ldply(1:numero_df, function(i){
        resultado <- lista_df[[i]] %>%
          anti_join(catalogo, by = condicion) %>%
          mutate(
            tabla = names(lista_df)[i],
            campo = nombre_columna_df,
            catalogo = nombre_catalogo
          ) %>%
          # Seleccionando columnas de interés
          select_(.dots = expresion_select)
      })
    }}) %>%
    # Uniendo los data frames
    reduce(rbind)
  
  return(resultado)
}

################################################################################

# Función que recibe una lista nombrada de data frames y, por medio de un vector
# que especifica ciertas columnas de cada data frame, permite revisar si tiene
# valores no numéricos.
# lista_df: lista nombrada de data frames
# relacion_columnas_consideradas: vector con nombres y entradas de la forma:
# c("df.columna", ..., ) que indicará qué data frame (y columna) de "lista_df" se
# considerarán para esta revisión.
# ceros_aceptables: parámetro de tipo lógico que indica si los ceros se
# considerarán aceptables o no. Éste parámetro es útil cuando un valor 0 en una
# variable numérica significa NA.

# La función regresa un data frame donde cada registro tiene los campos:
# "tabla", "campo" y "valor", donde cada registro corresponde a un valor que no
# satisface la condición correspondiente.

# Notas:
# 1. Las columnas de cada data frame en "lista_df" deben contener sólo caracteres
# alfanuméricos y guiones bajos.
# 2. Cada data frame en "lista_df" debe contener una columna llamada "serie".
# 3. Las entradas de "relacion_columnas_consideradas" puede ser especificadas de la
# manera ".columna", lo que significa que esa columna en cada data frame debe
# satisfacer la condicion.
# 4. Los valores lógicos / nulos se consideran numéricos.

revisa_columnas_numericas <- function(lista_df, relacion_columnas_consideradas,
  ceros_aceptables = TRUE){
  
  # Definiendo funciones auxiliares que reportarán valores no aceptables (ya sea
  # no numéricos, o no numéricos + 0):
  
  # La siguiente función, dado un dato "valor", regresará "valor" si éste no
  # es (o casteable a) numérico , en otro caso, regresará "__eliminar__"
  reporta_valor_no_numerico <- function(valor){
    
    # La siguiente variable contiene:
    # el valor de "as.numeric(valor)" si se pudo evaluar dicha expresión.
    # un warning o error si la expresión lanzó cualesquiera de las anteriores
    valor_error_warning <- tryCatch(as.numeric(valor),
      error = function(e) e,
      warning = function(w) w)
    
    if(is(valor_error_warning, "error") | is(valor_error_warning, "warning")){
      resultado <- valor
    } else{
      resultado <- "__eliminar__"
    }
    
    return(resultado)
  }
  
  # La siguiente función, dado un dato "valor", regresará "valor" si éste no
  # es (o casteable a) numérico o si es 0, en otro caso, regresará "__eliminar__"
  reporta_valor_no_numerico_0 <- function(valor){
    
    # La siguiente variable contiene:
    # el valor de "as.numeric(valor)" si se pudo evaluar dicha expresión.
    # un warning o error si la expresión lanzó cualesquiera de las anteriores
    valor_error_warning <- tryCatch(as.numeric(valor),
      error = function(e) e,
      warning = function(w) w)
    
    if(is(valor_error_warning, "error") | is(valor_error_warning, "warning")){
      resultado <- valor
    } else{
      if(!is.na(valor_error_warning) & valor_error_warning == 0){
        resultado <- valor
      } else{
        resultado <- "__eliminar__"
      }
    }
    
    return(resultado)
  }
  
  # Transformando "relacion_columnas_consideradas" en un data frame para su fácil
  # manipulación:
  relacion_columnas_consideradas_df <- data_frame(
    df.columna = relacion_columnas_consideradas
  ) %>%
    # Quedándonos sólo con renglones distintos:
    distinct() %>%
    separate(df.columna, into = c("nombre_df", "nombre_columna_df"), sep = "\\.")
  
  # Verificando que todos los data frames estén en lista_df:
  data_frames_considerados <- relacion_columnas_consideradas_df %>%
    # Filtrando los casos donde no se especifica un data frame
    filter(nombre_df != "") %>%
    pull(nombre_df) %>%
    unique()
  
  # Si hay data frames escritos en "relacion_columnas_condicion" que no se
  # encuentran en "lista_df", entonces parar la ejecución:
  if(sum(data_frames_considerados %in% names(lista_df)) != length(data_frames_considerados)){
    "Hay data frames especificados en 'relacion_columnas_consideradas' que no se " %>%
      paste0("encuentran en 'lista_df'") %>%
      stop()
  }
  
  # Ahora se pasará a interpretar cada renglón de "relacion_columnas_consideradas_df"
  # para realizar la revisión correspondiente.
  resultado <- apply(relacion_columnas_consideradas_df, 1, function(x){
    
    # Obteniendo valores de cada renglón del data frame
    nombre_df <- x["nombre_df"]
    nombre_columna_df <- x["nombre_columna_df"]
    
    # Si el data frame es especificado explícitamente:
    if(nombre_df != ""){
      
      df <- lista_df[[nombre_df]]
      # Si la columna correspondiente no se encuentra en el data frame especificado
      if(!(nombre_columna_df %in% colnames(df))){
        paste0("La columna ", nombre_columna_df, " no se encuentra en el data frame ",
          nombre_df) %>%
          stop()
      }
      
      # Obteniendo los valores de la columna de interés del df
      valores_columna_df <- df[[nombre_columna_df]] %>%
        unique()
      
      # Obteniendo los valores de "df.columna" que no cumplen la condición especificada
      if(ceros_aceptables == TRUE){
        evaluacion_valores_columna_df <- laply(valores_columna_df, reporta_valor_no_numerico)
      } else{
        evaluacion_valores_columna_df <- laply(valores_columna_df, reporta_valor_no_numerico_0)
      }
      
      valores_columnas_df_no_aceptables <- evaluacion_valores_columna_df[
        evaluacion_valores_columna_df != "__eliminar__"]
      
      # Generando el data frame de valores no aceptables
        resultado <- data_frame(
          "tabla" = rep(nombre_df, length(valores_columnas_df_no_aceptables)),
          "campo" = rep(nombre_columna_df, length(valores_columnas_df_no_aceptables)),
          "valor" = valores_columnas_df_no_aceptables
        )

      # Si el data frame no es especificado explícitamente, aplicar la revisión
      # a la columna "nombre_columna_df" de cada data frame.
      
    } else{
      
      resultado <- ldply(1:length(lista_df), function(i){
        
        # Encontrando el data frame de trabajo y su nombre:
        df <- lista_df[[i]]
        nombre_df <- names(lista_df)[i]
        
        # Si la columna correspondiente no se encuentra en el data frame especificado
        if(!(nombre_columna_df %in% colnames(df))){
          paste0("La columna ", nombre_columna_df, " no se encuentra en el data frame ",
            nombre_df) %>%
            stop()
        }
        
        # Obteniendo los valores de la columna de interés del df
        valores_columna_df <- df[[nombre_columna_df]] %>%
          unique()
        
        # Obteniendo los valores de "df.columna" que no cumplen la condición especificada
        if(ceros_aceptables == TRUE){
          evaluacion_valores_columna_df <- laply(valores_columna_df, reporta_valor_no_numerico)
        } else{
          evaluacion_valores_columna_df <- laply(valores_columna_df, reporta_valor_no_numerico_0)
        }
        
        valores_columnas_df_no_aceptables <- evaluacion_valores_columna_df[
          evaluacion_valores_columna_df != "__eliminar__"]
        
        # Generando el data frame de valores no aceptables
        resultado <- data_frame(
          "tabla" = rep(nombre_df, length(valores_columnas_df_no_aceptables)),
          "campo" = rep(nombre_columna_df, length(valores_columnas_df_no_aceptables)),
          "valor" = valores_columnas_df_no_aceptables
        )
        
        return(resultado)
      })
    }
    
    return(resultado)
  }) %>%
  # Uniendo los data frames
  reduce(rbind)
  
  return(resultado)
}

################################################################################

# Función que recibe una lista de data frames y, por medio de un vector que
# especifica un filtro de registros de interés en dichos data frames, regresa una
# tabla con los series de los registros que cumplen alguna de las condiciones
# establecidas.
# lista_df: lista nombrada de data frames
# relacion_columnas_valores: vector con condiciones de la forma:
# c("df.columna" = "valor") 
# La función regresa un data frame donde cada registro tiene los campos:
# "tabla", "campo", "valor" y "serie" y corresponde a un registro donde es válida
# alguna de las condiciones especificadas
# Notas:
# 1. Los nombres de las columnas de cada data frame en "lista_df" deben contener
# sólo caracteres alfanuméricos y guiones bajos.
# 2. Cada data frame en "lista_df" debe contener una columna llamada "serie".
# 3. Las entradas de "relacion_columnas_valores" puede ser especificadas de la
# manera c(".columna" = "valor"), lo que significa que cualquier registro que
# cumpla con dicha condición.
# 4. Esta función es útil para crear listas de valores a corregir en los data
# data frames especificados.

revisa_columnas_valores <- function(lista_df, relacion_columnas_valores){
  
  # Calculando la longitud de "lista_df" que será de utilidad posteriormente
  numero_df <- length(lista_df)
  
  # Transformando "relacion_columnas_valores" en un data frame para su fácil
  # manipulación:
  relacion_columnas_valores_df <- data_frame(
    df.columna = names(relacion_columnas_valores),
    valor = relacion_columnas_valores
  ) %>%
    # Quedándonos sólo con renglones distintos:
    distinct() %>%
    separate(df.columna, into = c("nombre_df", "nombre_columna_df"), sep = "\\.")
  
  # Verificando que todos los data frames estén en "lista_df":
  data_frames_considerados <- relacion_columnas_valores_df %>%
    # Filtrando los casos donde no se especifica un data frame
    filter(nombre_df != "") %>%
    pull(nombre_df) %>%
    unique() 
  
  # Si hay data frames escritos en "relacion_columnas_valores" que no se encuentran
  # en "lista_df", entonces parar la ejecución:
  if(sum(data_frames_considerados %in% names(lista_df)) != length(data_frames_considerados)){
    "Hay data frames especificados en 'relacion_columnas_valores' que no se " %>%
      paste0("encuentran en 'lista_df'") %>%
      stop()
  }
  
  # Ahora se pasará a interpretar cada renglón de "relacion_columnas_valores_df"
  # para realizar los joins correspondientes.
  resultado <- apply(relacion_columnas_valores_df, 1, function(x){
    
    # Obteniendo valores de cada renglón del data frame
    nombre_df <- x["nombre_df"]
    nombre_columna_df <- x["nombre_columna_df"]
    valor <- x["valor"]

    # Si el data frame es especificado explícitamente:
    if(nombre_df != ""){
      
      df <- lista_df[[nombre_df]]

      # Si la columna correspondiente no se encuentra en el data frame especificado
      if(!(nombre_columna_df %in% colnames(df))){
        paste0("La columna ", nombre_columna_df, " no se encuentra en el data frame ",
               nombre_df) %>%
          stop()
      }
      
      # Si se pasaron los dos filtros anteriores, hacer el filtro adecuado.
      
      # Generando la expresión para el filter_.
      expresion_filter <- ifelse(is.na(valor), paste0("is.na(", nombre_columna_df, ")"),
        paste0(nombre_columna_df, "== \"", valor, "\""))
      
      # Generando la expresión para el select_.
      # Se generarán las columnas "tabla" y "campo"
      expresion_select <- list("tabla", "campo", nombre_columna_df, "serie")
      names(expresion_select) <- c("tabla", "campo", "valor", "serie")
      
      resultado <- df %>%
        filter_(expresion_filter) %>%
        mutate(
          tabla = nombre_df,
          campo = nombre_columna_df
        ) %>%
        # Seleccionando columnas de interés
        select_(.dots = expresion_select)
      
    } else{
      
      # Generando la expresión para el filter_.
      expresion_filter <- ifelse(is.na(valor), paste0("is.na(", nombre_columna_df, ")"),
        paste0(nombre_columna_df, "== \"", valor, "\""))
      
      # Generando la expresión para el select_.
      # Se generarán las columnas "tabla" y "campo"
      expresion_select <- list("tabla", "campo", nombre_columna_df, "serie")
      names(expresion_select) <- c("tabla", "campo", "valor", "serie")
      
      # Aplicar el filter a cada elemento en "lista_df"
      resultado <- ldply(1:numero_df, function(i){
        resultado <- lista_df[[i]] %>%
          filter_(expresion_filter) %>%
          mutate(
            tabla = names(lista_df)[i],
            campo = nombre_columna_df
          ) %>%
          # Seleccionando columnas de interés
          select_(.dots = expresion_select)
      })
    }}) %>%
    # Uniendo los data frames
    reduce(rbind)
  
  return(resultado)
}

################################################################################
      
# Función que recibe una lista nombrada de data frames y una lista llaves naturales
# para cada data frame. La función identifica los renglones duplicados de cada
# data frame con respecto a su llave correspondiente.
# lista_df: lista nombrada de data frames
# lista_llaves_naturales: una lista de la forma list("df1" = c("col1", "col2"),...)
# que especifique la llave natural correspondiente a cada data frame.
# tipo_resultado: "completo", regresa los registros duplicados completos
#                 "llaves_naturales", regresa sólo las columnas de "serie" y
#                 las especificadas como llaves naturales para ese data frame.
# La función regresa una lista de data frames, cada elemento de ésta contiene
# los registros duplicados de un data frame especificado en  "lista_llaves_naturales".
# Nota:
# "lista_llaves_naturales" puede ser de menor tamaño que "lista_df". En este
# caso, los data frames no especificados en "lista_llaves_naturales" no serán
# tomados en cuenta

encuentra_duplicados <- function(lista_df, lista_llaves_naturales,
  tipo_resultado = "llaves_naturales"){
  
  if(!tipo_resultado %in% c("completo", "llaves_naturales")){
    stop("\"tipo_resultado\" debe ser \"completo\" o \"llaves_naturales\"")
  }
  
  # Revisando que todos los data_frames especificados en "lista_llaves_naturales"
  # Se encuentren en "lista_df"
  if(sum(names(lista_llaves_naturales) %in% names(lista_df)) !=
      length(lista_llaves_naturales)){
    paste0("Hay data frames especificados en \"lista_llaves_naturales\" que no ",
      "se encuentran en \"lista_df\"") %>%
      stop()
  } else{
    
    resultado <- llply(1:length(lista_llaves_naturales), function(i){
      
      # Obteniendo un data frame de interés de la lista
      nombre_df <- names(lista_llaves_naturales)[i]
      df <- lista_df[[nombre_df]]
      
      # Revisando que las columnas especificadas en "lista_llaves_naturales[[i]]"
      # se encuentren en df:
      nombres_columnas_llave_natural_df <- lista_llaves_naturales[[i]]
      if(sum(nombres_columnas_llave_natural_df %in% names(df)) !=
          length(nombres_columnas_llave_natural_df)){
        paste0("El data frame \"", nombre_df, "\" no contiene alguna de las siguientes ",
          "columnas: \"", paste0(nombres_columnas_llave_natural_df, collapse = "\",
            \""), "\"") %>%
          stop()
      } else{
        
        # Encontrando los valores de la llave natural que están repetidos
        valores_duplicados_llave_natural <- df %>%
          select_(.dots = nombres_columnas_llave_natural_df) %>%
          filter(duplicated(.)) %>%
          # Se eliminan los que posiblemente están repetidos más de una vez.
          unique()
        
        registros_duplicados <- df %>%
          inner_join(valores_duplicados_llave_natural,
            by = nombres_columnas_llave_natural_df)
        
        if(tipo_resultado == "completo"){
          resultado <- registros_duplicados
        } else{
          
          columnas_interes <- c("serie", nombres_columnas_llave_natural_df)
          resultado <- registros_duplicados %>%
            select_(.dots = columnas_interes)
        }
        
        return(resultado)
      }
    })
    
    # Nombrando los data_frames de duplicados:
    names(resultado) <- names(lista_llaves_naturales)
    return(resultado)
  }
}

################################################################################
# Funciones auxiliares sobre data frames
################################################################################

# Función auxiliar a la hora de programar, que regresa los nombres de las columnas
# de un df que contienen cierto string.
# df: data frame de interés
# x: string de interés
# La función regresa un vector con los nombres de las columnas de df que contienen
# "string".
encuentra_columnas <- function(df, x){
  nombres_columnas <- names(df) %>%
    sort()
  indice <- stri_detect_fixed(nombres_columnas, x, case_insensitive = TRUE)
  return(nombres_columnas[indice])
}

#################################################################################

# Función auxiliar para revisar qué archivos contienen determinada columna
# df: data frame que contiene "columna" y otra columna llamada "archivo_origen"
# nombre_columna: nombre de la columna a revisar
# La función regresa un vector con los nombres de los archivos que contienen la
# columna de interés

obtiene_archivos_columna <- function(df, nombre_columna){
  resultado <- df %>%
    filter_(paste0("!is.na(", nombre_columna, ")")) %>%
    pull(archivo_origen) %>%
    unique()
  return(resultado)
}

################################################################################

# Función auxiliar a la hora de revisar los datos, para crear una tabla para cada
# columna de un data frame, y así visualizar valores a corregir
# df: data frame cuyas valores en cada columna se quieren revisar
# La función regresa una lista de tablas con los valores de cada columna de df.
revisa_valores <- function(df){
  # Obteniendo nombres de las columnas en el data frame
  nombres_columnas <- colnames(df) %>%
    sort()
  
  # Asignando nombres para el llply:
  names(nombres_columnas) <- nombres_columnas
  
  # Generando una lista con tablas de valores, una para cada columna
  resultado <- llply(nombres_columnas, function(x){
    resultado <- df[[x]] %>%
      table(useNA = "ifany")
    return(resultado)
  })
  
  return(resultado)
}

################################################################################

# Función auxiliar para intercambiar un valor por otro de determinada columna
# en un data frame (mientras exista la columna).
# df: data frame de interés
# nombre_columna: nombre de la columna de interés.
# valor_anterior: valor de la columna a sustituir (puede ser NA).
# valor_nuevo
# la función regresa un data frame, por lo que se puede pipear.
cambia_valor_columna <- function(df, nombre_columna, valor_anterior, valor_nuevo){
  
  # Si la columna está en df, se cambia el valor de la misma, de lo contrario,
  # el df se deja idéntico y se envía un warning
  if(nombre_columna %in% names(df)){
    
    # Si valor_anterior es NA, se debe hacer un procedimiento distinto
    if(is.na(valor_anterior)){
      df[[nombre_columna]][is.na(df[[nombre_columna]])] <- valor_nuevo
    } else{
      df[[nombre_columna]] <- ifelse(
        !is.na(df[[nombre_columna]]) & df[[nombre_columna]] == valor_anterior,
        valor_nuevo, df[[nombre_columna]]
      )
    }
  }else{
    warning("La columna especificada no está en el data frame")
  }
  return(df)
}

################################################################################

# Función auxiliar para sustituir el valor de un campo para todos los registros
# de un data frame que cumplan una condición especificada.
# df: data frame de interés
# condicion: string de código de R que especifica una condición
# sobre los registros del data frame, en notación de dplyr. Ejemplo: "col == 'algo'"
# nombre_columna: nombre de la columna de interés
# valor_nuevo: valor nuevo que tendrá cada registro que cumpla la condición en
# la columna de interés.
# la función regresa un data frame, por lo que se puede pipear.
cambia_valor_columna_condicion <- function(df, condicion, nombre_columna, valor_nuevo){
  
  # Si la columna está en df, se cambia el valor de la misma, de lo contrario,
  # el df se deja idéntico y se envía un warning
  if(nombre_columna %in% names(df)){
    
    # Poniéndole comillas a "valor_nuevo" si es un string.
    if(is.character(valor_nuevo))
      valor_nuevo <- paste0("'", valor_nuevo, "'")
    
    # Generando la expresión para el mutate_:
    expresion <- paste0(
      "ifelse(", condicion, ", ",
      valor_nuevo, ", ",
      nombre_columna, ")"
    )
    
    # Asignando nombres a la expreión porque el mutate_ los necesita para asignar
    # nombres a las nuevas variables
    names(expresion) <- nombre_columna
    
    # Creando el resultado:
    resultado <- df %>%
      mutate_(.dots = expresion)

  }else{
    warning("La columna especificada no está en el data frame")
    resultado <- df
  }
  return(resultado)
}

################################################################################

# Función para eliminar columnas vacías (de puros NA's) de un data frame
# df: dataframe a eliminar columnas vacías
# La función regresa el data frame sin columnas vacías
elimina_columnas_vacias <- function(df){
  
  # Obteniendo columnas a eliminar
  columnas_eliminar <- df %>%
    # Convirtiendo todas los valores de todas las columnas a TRUE si el valor
    # correspondiente es NA y FALSE e.o.c
    mutate_all(is.na) %>%
    # Calculando la cantidad de NA0s que tiene cada columa
    summarise_all(sum) %>%
    # Viendo si la columna tiene puros NA's, en dicho caso, el resultado será
    # TRUE (columna a eliminar)
    mutate_all(
      funs(. == nrow(df))
    ) %>%
    # Poniendo los nombres de las columnas en una misma variable
    gather("columna", "eliminar") %>%
    filter(eliminar) %>%
    # Generando expresión para el select_
    mutate(
      expresion = paste0("-", columna)
    ) %>%
    pull(expresion)
  
  # Eliminando dichas columnas del df
  if(length(columnas_eliminar) == 0)
    # Si no se debe eliminar ninguna columna
    resultado <- df
  else
    resultado <- df %>%
      select_(.dots = columnas_eliminar)
  
  return(resultado)
}

################################################################################

# Función para transformar un conjunto de columna a tipo numérico o entero:
# df: data_frame cuyas columna se quieren transformar
# relacion_variables_tipo: vector con nombres y valores de la forma:
#   c("columna" = "valor"), que indica qué tipo de datos debe tener cada 
#   columna enlistada. "valor" puede ser sólo "integer" o "numeric".
# warnings: TRUE para mostrarlas y FALSE para eliminarlas
# La función regresa df con los tipos de datos nuevos para las columnas
# especificadas. Si "columna" no se puede castear a los tipos especificados,
# por ejemplo, si contiene caracteres, se introducirán NA's y se lanzarán
# warnings si así se desea.

# Notas: 
# - Castear de tipo double a entero no genera warnings, por ejemplo,
#   as.integer(pi).
# - Recordar que todo integer es numeric y todo double es numeric, pero integer
#   y double son mutuamente excluyentes. as.numeric() castea a double por default.

mutate_numeric_integer <- function(df, relacion_variables_tipo, warnings = TRUE){
  
  variables <- names(relacion_variables_tipo)
  condiciones <- relacion_variables_tipo
  
  if(!all(condiciones %in% c("integer", "numeric"))){
    stop("Todas las condiciones especificadas deben ser o 'numeric' o 'integer'")
  }
  
  # Formando la expresión para el mutate_
  expresion <- paste0("as.", condiciones, "(", variables, ")") %>%
    as.list()
  
  # Asignando nombres a la expresión porque el mutate_ los necesita para asignar
  # nombres a las nuevas variables
  names(expresion) <- variables
  
  if(warnings == TRUE){
    resultado <- mutate_(df, .dots = expresion)
  } else{
    resultado <- suppressWarnings(mutate_(df, .dots = expresion))
  }
  
  return(resultado)
}

################################################################################

# Función para transformar un conjunto de variables a tipo lógico:
# df: data frame cuyas variables se quieren transformar
# ...: nombres de las variables a transformar
mutate_logical <- function(df, ...){
  # Obteniendo variables como un vector
  variables <- list(...) %>%
    as.character() 
  
  # Formando la expresión para el mutate_
  expresion <- paste0("as.logical(", variables, ")") %>%
    as.list()
  
  # Asignando nombres a la expresión porque el mutate_ los necesita para asignar
  # nombres a las nuevas variables
  names(expresion) <- variables
  
  resultado <- df %>%
    mutate_(.dots = expresion)
  
  return(resultado)
}

################################################################################

# Función auxiliar para transformar NA's en "" para columnas de tipo caracter.
# df: data frame de interés
# El resultado es un data frame con los NA's sustituídos por "" en columnas de
# tipo caracter.
# Nota: como es muy útil trabajar con NA's en R, esta transformación se recomienda
# realizarla hasta tener las tablas finales, listas para introducir en la base de
# datos.
cambia_na_strings_vacios <- function(df){
  resultado <- df %>%
    mutate_if(
      is.character, funs(ifelse(is.na(.), "", .))
    )
  return(resultado)
}

################################################################################

# Función auxiliar para generar una llave numérica a partir de una llave natural:
# df: data frame al que se le agregará una columna con una llave numérica
# a partir de una llave natural (definida a partir de una o varias columnas
# que contiene). Es decir, dos registros tendrán el mismo número si y sólo si
# tienen los mismos valores en las columnas de la llave natural.
# nombre_columna_llave: nombre de la columna que contendrá la llave numérica
# nombres_columnas_llave_natural: vector con los nombres de las variables que
# definen la llave natural.
# El resultado es el df con la llave generada como una nueva columna.
# Si "nombres_columnas_llave_natural" es de longitud 0, la llave generada
# será simplemente una cuenta de todos los renglones, en el orden en que aparecen
# Nota: la función crea la llave natural ordenando por las columnas en las que
# se basa.
genera_llave <- function(df, nombre_columna_llave, nombres_columnas_llave_natural){
  
  if(length(nombres_columnas_llave_natural) > 0){
    
    # Generando la expresión para ordenar primero el df por los factores, ya
    # que estos se generarán en orden de aparición (forcats::as_factor())
    expresion_arrange_ = nombres_columnas_llave_natural
    
    # Generando la expresión para el mutate_:
    expresion_mutate_ = paste0(
      "paste0(",
      paste0(".data$", nombres_columnas_llave_natural) %>%
        paste0(collapse = ", \"_\", "),
      ") ",
      # forcats::as_factor() crea los factores en el órden de aparición, sin ordenar
      # primero (lo cuál causaba problemas de "11" < "2" al usar as.factor())
      "%>% as_factor() %>% as.integer()"
    )
    
    # Asignando el nombre de la columna que contendrá la llave numérica a la expresión
    # porque el mutate_ lo requiere
    names(expresion_mutate_) <- nombre_columna_llave
    
    resultado <- df %>%
      # generando una columna para poder regresar el data frame a su orden original
      # después de generar las llaves
      mutate(
        orden_aux_funcion_genera_llave = 1:nrow(.)
        ) %>%
      arrange_(.dots = expresion_arrange_) %>%
      mutate_(.dots = expresion_mutate_) %>%
      arrange(orden_aux_funcion_genera_llave) %>%
      select(-orden_aux_funcion_genera_llave)
    
  } else{
    # Si no se pasan columnas que especifiquen una llave natural:
    
    expresion_mutate_ = "1:nrow(.)"
    names(expresion_mutate_) <- nombre_columna_llave
    
    resultado <- df %>%
      mutate_(.dots = expresion_mutate_)
  }
  
  return(resultado)
}

################################################################################

# La siguiente función es un wrapper de "genera_llave()", que permite,
# dado un data frame con datos que se dividirán en tablas, generar las llaves
# numéricas de cada una de las tablas futuras.
# Parámetros:
# - df: una tabla de datos que se segmentará en distintas tablas. Para cada
# tabla a crear, df debe contener una columna lógica que indique qué registros
# se utilizarán para crearla.
# - prefijo_columnas_indicadoras: los nombres de las columnas lógicas anteriores
# deberán ser de la forma: prefijo_columnas_indicadoras"nombre_tabla_nueva",
# para ser fácilmente identificables.
# - sufijo_columnas_llaves_numericas: para cada tabla, el nombre de la columna 
# que corresponderá a la llave numérica generada será
# "nombre_tabla_nueva"sufijo_columnas_llaves_numericas
# - relacion_tablas_columnas_llaves: una lista que especificará qué
# columnas se utilizarán para crear una llave numérica de qué tabla. Es decir,
# las columnas donde cada combinación de valores representa un registro
# distinto de dicha tabla. Estos valores estarán especificados de la forma:
# list("nombre_tabla_nueva" = c("nombre_columna_1", "nombre_columna_2", ...))

# La función regresa un data frame que corresponde al de entrada, anexando las
# columnas correspondientes a las llaves numéricas de las tablas futuras.
# El procedimiento seguido para obtener este data frame es el siguiente:
# 1. Se obtienen las columnas lógicas que determinan qué registros se usarán
# para generar la nueva tabla con ayuda de "prefijo_columnas_indicadoras" y
# "nombre_tabla_nueva".
# 2. Se toma un subconjunto de "df" que contenga sólo los registros especificados
# en dicha columna.
# 3. Se genera la llave numérica de la nueva tabla usando los registros y
# columnas especificados (para los registros no especificados esta llave vale NA).
# Notas:
# - Las llaves numéricas se generan en el orden especificado, por lo que para
# generar la llave de una tabla se puede hacer referencia a la llave de una tabla
# anterior. De hecho, por simplicidad, esto es lo más recomendable.
# - La función "genera_llave()" ordena los registros por las columnas especificadas
# antes de calcular la llave numérica, para que ésta sea lo más natural posible

genera_llaves_varias_tablas <- function(df, prefijo_columnas_indicadoras,
  sufijo_columnas_llaves_numericas = ".id", relacion_tablas_columnas_llaves){
  
  nombres_columnas_df <- colnames(df)
  nombres_tablas_nuevas <- names(relacion_tablas_columnas_llaves)
  
  # Generando los nombres de las columnas indicadoras y de llaves numéricas
  # a partir de los nombres de las tablas nuevas:
  nombres_columnas_indicadoras <- paste0(prefijo_columnas_indicadoras,
    nombres_tablas_nuevas)
  nombres_columnas_llaves_numericas <- paste0(nombres_tablas_nuevas,
    sufijo_columnas_llaves_numericas)
  
  # Revisando que todas las columnas indicadoras se encuentren en df
  if(!all(nombres_columnas_indicadoras %in% nombres_columnas_df)){
    stop(paste0("Existen nombres de tablas en 'relacion_tablas_columnas_llaves' ",
      "sin columnas indicadoras asociadas"))
  }
  
  # Revisando que todas las columnas especificadas como parte de las llaves
  # naturales de cada tabla se encuentren en "df", o bien, se encuentren en:
  # nombres_columnas_llaves_numericas (ya que la función está diseñada para poder
  # construir una llave numérica con la salida de una anterior)
  l_ply(1:length(relacion_tablas_columnas_llaves), function(i){
    if(!all(relacion_tablas_columnas_llaves[[i]] %in%
        base::union(nombres_columnas_df, nombres_columnas_llaves_numericas))){
      stop(paste0("Algunas colummas que definen la llave natural de ",
        nombres_tablas_nuevas[i], " no se encontraron en df"))
    }
  })
  
  # Creando las llaves numéricas de cada tabla nueva y nombrándolas apropiadamente.
  # Estas columnas se generarán utilizando un "for" debido a que se va actualizando
  # un data frame y no se desea utilizar una variable global
  data_frame_columnas_llaves_numericas <- df
  
  for(i in 1:length(nombres_tablas_nuevas)){
    
    nombre_tabla_actual <- nombres_tablas_nuevas[[i]]
    nombre_columna_indicadora <- nombres_columnas_indicadoras[i]
    nombre_columna_llave_numerica <- nombres_columnas_llaves_numericas[i]
    nombres_columnas_llave_natural <- relacion_tablas_columnas_llaves[[i]]
    
    print(paste0("Generando llave numérica para la tabla: '", nombre_tabla_actual, "'"))
    
    # Generando la llave numérica de la i-ésima tabla usando únicamente los
    # registros indicados por la i-ésima columna indicadora
    data_frame_columnas_llaves_numericas <- ddply(data_frame_columnas_llaves_numericas,
      nombre_columna_indicadora, function(df){
        
        # Generando variable para seleccionar registros de interés
        indicadora <- unique(df[[nombre_columna_indicadora]])
        
        # No usar do.call porque es muy lenta con data frames muy grandes
        if(indicadora){
          resultado <- genera_llave(df, nombre_columna_llave_numerica,
            nombres_columnas_llave_natural)
        } else{
          
          # Generando la expresión para el mutate_
          expresion_mutate_ <- NA
          names(expresion_mutate_) <- nombre_columna_llave_numerica
          
          resultado <- df %>%
            mutate_(.dots = expresion_mutate_)
        }
        return(resultado)
      })
  }
  
  return(data_frame_columnas_llaves_numericas)
}

################################################################################

# Función auxiliar para generar una tabla a partir de una columna de llave 
# y un vector nombrado de columnas adicionales. Para calcular el valor de cada
# columna adicional correspondiente a un valor de "nombre_columna_llave" se
# utilizará el primer valor encontrado.
# df: df que contiene la columna "nombre_columna_llave" y las columnas a incluir en
# la nueva tabla.
# nombre_columna_llave: nombre de la columna que será una llave de la tabla nueva.
# nombre_nuevo_columna_llave: el nombre que tendrá la llave en el nuevo data frame
# vector_columnas_adicionales: vector nombrado de columnas adicionales a incluir
# en la tabla nueva.
# Para cada elemento en el vector, el "nombre" corresponde al nombre de la columna
# en el data frame nuevo, y el valor al nombre de ésta en el df anterior..
# La función regresa la tabla generada con las especificaciones anteriores
genera_tabla_1 <- function(df, nombre_columna_llave, nombre_nuevo_columna_llave,
  vector_columnas_adicionales = c()){
  
  # Generando la expresión para el group_by:
  expresion_group_by_ <- nombre_columna_llave
  
  # Generando la expresión para el rename (de la columna de la llave)
  expresion_rename_ <- nombre_columna_llave
  names(expresion_rename_) <- nombre_nuevo_columna_llave
  
  # Hay dos casos distintos: si vector_columnas_adicionales es vacío o no:
  if(length(vector_columnas_adicionales) > 0){
    
    # Generando la expresión para el summarise:
    expresion_summarise_ <- paste0("first(.data$",
      vector_columnas_adicionales, ")")
    
    # Asignando nombres de variables en la nueva tabla:
    names(expresion_summarise_) <- names(vector_columnas_adicionales)
    
    # Generando resultado
    resultado <- df %>%
      group_by_(.dots = expresion_group_by_) %>%
      summarise_(
        .dots = expresion_summarise_
      ) %>%
      ungroup() %>%
      rename_(
        .dots = expresion_rename_
      )
    
  } else{
    # Generando resultado en el caso de "vector_columnas_adicionales" sea vacío,
    # en este caso, la tabla tendrá como única columna "nombre_columna_llave"
    resultado <- df %>%
      group_by_(.dots = expresion_group_by_) %>%
      summarise() %>%
      ungroup() %>%
      rename_(
        .dots = expresion_rename_
      )
  }
  
  return(resultado)
}

################################################################################

# Función auxiliar para generar una tabla a partir de una columna de llave 
# y un vector nombrado de columnas adicionales. Para calcular el valor de cada
# columna adicional correspondiente a un valor de "nombre_columna_llave" se
# utilizará la moda del valor de dicha columna en el nivel correspondiente de
# "nombre_columna_llave".
# df: df que contiene la columna "nombre_columna_llave" y las columnas a incluir en
# la nueva tabla.
# nombre_columna_llave: nombre de la columna que será una llave de la tabla nueva.
# nombre_nuevo_columna_llave: el nombre que tendrá la llave en el nuevo data frame
# lista_columnas_adicionales: lista nombrada de columnas adicionales a incluir
# en la tabla nueva.
# Para cada elemento en el vector, el "nombre" corresponde al nombre de la columna
# en el data frame nuevo, y el valor al nombre de ésta en el df anterior..
# La función regresa la tabla generada con las especificaciones anteriores
# Notas:
# - La diferencia entre "genera_tabla()" y "genera_tabla_2()" es que "genera_tabla"
# utiliza el primer valor encontrado para asignar el valor de cada campo para
# cada nivel de "nombre_columna_llave", y "genera_tabla_2" utiliza el valor más
# frecuente. Cabe destacar que "genera_tabla_2" es más lenta que "genera_tabla".
# - No debe haber columnas llamadas "n" en df.
genera_tabla_2 <- function(df, nombre_columna_llave, nombre_nuevo_columna_llave,
 vector_columnas_adicionales){
  
  # Generando la expresión para el rename (de la columna de la llave), que se
  # usará al final del ddply
  expresion_rename_ <- nombre_columna_llave
  names(expresion_rename_) <- nombre_nuevo_columna_llave
  
  resultado <- df %>%
    group_by_(nombre_columna_llave) %>%
    # Para cada grupo:
    do(
      # Se utilizará el hecho de que "vector_columnas_adicionales" está nombrada
      # para asignar el nuevo nombre de las columnas a "resultado".
      map(vector_columnas_adicionales, function(columna, df_sub){
          resultado <- df_sub %>%
            # Seleccionando la columna de interés
            group_by_(columna) %>%
            # Calculando la moda
            summarise(
              n = n()
            ) %>%
            ungroup() %>%
            arrange(desc(n)) %>%
            # Sacándolo como un tipo de datos primitivo
            select_(columna) %>%
            slice(1) %>%
            pull()
        return(resultado)
      }, .) %>%
        #convirtiendo a data frame
        as_data_frame(.)
    ) %>%
    ungroup() %>%
    # al final del ddply, hay que renombrar "nombre_columna_llave"
    # a "nuevo_nombre_columna_llave"
    rename_(.dots = expresion_rename_)
  
  return(resultado)
}

################################################################################

# La siguiente función es un wrapper de "genera_tabla_1()" y genera_tabla_2()",
# que permite, dado un data frame con datos que se dividirán en tablas, y una
# columna por llave numérica correspondiente a tabla nueva, formar las nuevas
# tablas con ciertas columnas especificadas para cada una.

# Parámetros:
# - df: una tabla de datos que se segmentará en distintas tablas. Para cada
# tabla a crear, "df" debe contener una columna de llave numérica, es decir, una
# columna donde cada valor distinto represente un registro distinto.
# - sufijo_columnas_llaves_numericas: los nombres de columnas con las llaves
# anteriores deberán ser de la forma "nombre_tabla_nueva"sufijo_columnas_llaves_numericas,
# para ser fácilmente localizables.
# - nombre_nuevo_columnas_llaves_numericas: el nuevo nombre que tendrán las
# columnas correspondientes a las llaves numéricas en su nueva tabla.
# - relacion_tablas_columnas_funciones: una lista que especificará qué columnas
# contendrá cada tabla, además de la función que se utilizará para agregar los
# datos en caso de que se encuentren varias combinaciones por valor de la llave.
# Esta lista será especificada como sigue:
# list("nombre_tabla_nueva#numero_funcion_agregacion" = c(
#   "nombre_nuevo_columna_1" = "nombre_columna_1",
#   "nombre_nuevo_columna_2" = nombre_columna_2",
#   ...), ...)
# Donde "numero_funcion_agregacion" = i, si se quiere utilizar la función
# "genera_tabla_i()".
#
# La función regresa una lista nombrada que contiene las tablas nuevas generadas
# con las especificaciones anteriores. Cabe destacar que la función anterior está
# diseñada para trabajar en conjunto con "genera_llaves_varias_tablas". Por este
# motivo, a la hora de integrar cualquier tabla, se ignoran los registros que
# tienen NA en la llave correspondiente.
genera_tablas <- function(df, sufijo_columnas_llaves_numericas = ".id",
  nombre_nuevo_columnas_llaves_numericas = "id", relacion_tablas_columnas_funciones){
  
  nombres_columnas_df <- colnames(df)
  nombres_tablas_nuevas <- (names(relacion_tablas_columnas_funciones) %>%
      stri_match_first_regex("(.*)#.*"))[,2]
  numeros_funciones_agregacion <- as.numeric((names(relacion_tablas_columnas_funciones) %>%
      stri_match_first_regex(".*#(.*)"))[,2])
  
  # Generando los nombres de las columnas que contienen las llaves numéricas
  # a partir de los nombres de las tablas nuevas:
  nombres_columnas_llaves_numericas <- paste0(
    nombres_tablas_nuevas, sufijo_columnas_llaves_numericas)
  
  # Revisando que los nombres de las tablas nuevas y de las funciones de agregación
  # estén correctamente formados.
  
  if(NA %in% nombres_tablas_nuevas){
    stop("Alguna tabla no está correctamente nombrada.")
  }
  
  if(!all(numeros_funciones_agregacion %in% c(1,2))){
    stop("Algún número de función de agregación no es válido")
  }
  
  # Revisando que todas las columnas correpondientes a llaves se encuentren en df
  if(!all(nombres_columnas_llaves_numericas %in% nombres_columnas_df)){
    stop(paste0("No se encontraron las columnas de llaves numéricas asociadas ",
      "a algunas tablas nuevas especificadas."))
  }
  
  # Revisando que todas las columnas especificadas como parte de las tablas nuevas
  # se encuentren en "df"
  l_ply(1:length(relacion_tablas_columnas_funciones), function(i){
    if(!all(relacion_tablas_columnas_funciones[[i]] %in% nombres_columnas_df)){
      stop(paste0("Algunas colummas que se desea formen parte de la tabla ",
        nombres_tablas_nuevas[i], " no se encontraron en df"))
    }
  })
  
  # Cortando las tablas nuevas utilizando la especificación correspondiente a
  # cada una.
  
  resultado <- llply(1:length(relacion_tablas_columnas_funciones), function(i){
    
    nombre_tabla_actual <- nombres_tablas_nuevas[i]
    numero_funcion_agregacion <- numeros_funciones_agregacion[i]
    nombre_columna_llave_numerica <- nombres_columnas_llaves_numericas[i]
    nombre_nuevo_columna_llave_numerica <- nombre_nuevo_columnas_llaves_numericas
    vector_columnas_adicionales <- relacion_tablas_columnas_funciones[[i]]
    
    print(paste0("Generando la tabla: '", nombre_tabla_actual, "'"))

    # Generando la expresión para el filter_:
    expresion_filter_ <- paste0("!is.na(", nombre_columna_llave_numerica, ")")
    
    # Generando la i-ésima tabla:
    
    if(numero_funcion_agregacion == 1){
      resultado <- df %>%
        filter_(expresion_filter_) %>%
        genera_tabla_1(nombre_columna_llave_numerica, nombre_nuevo_columna_llave_numerica,
          vector_columnas_adicionales)
    } else{
      resultado <- df %>%
        filter_(expresion_filter_) %>%
        genera_tabla_2(nombre_columna_llave_numerica, nombre_nuevo_columna_llave_numerica,
          vector_columnas_adicionales)
    }
    return(resultado)
  })
  
  names(resultado) <- nombres_tablas_nuevas
  return(resultado)
}

################################################################################
# Funciones auxiliares sobre vectores
################################################################################

# Función para que, dado un vector de strings (frases), capitalice la primera
# letra de cada palabra de cada una de sus entradas, y las otras letras las minimiza.
# vec: Vector cuyas entradas son strings (frases)
# La función regresa vec con cada palabra en cada entrada capitalizada apropiadamente.
# Además, quita espacios en blanco al inicio y término de cada string
simple_cap <- function(vec){
  
  # Quitando NA's para que la función funcione como se espera, pero guardando
  # sus posiciones para volver a insertarlas al finalizar la función:
  posicion_NAs <- is.na(vec)
  vec[posicion_NAs] <- ""
  
  lista_palabras_por_string <- vec %>%
    # Quitando espacios en blanco
    stri_trim_both() %>%
    # Cortando cada entrada del vector por palabras
    stri_split_coll(pattern = " ")
  
  # Capitalizando cada letra de inicio de cada palabra y minimizando las otras
  resultado <- llply(lista_palabras_por_string, function(x){
    
    # Capitalizando primera letra de cada palabra de x
    primera_letra_palabras <- stri_sub(x, from = 1, to = 1) %>%
      toupper()
    # Minimizando las otras letras
    otras_letras <- stri_sub(x, from = 2) %>%
      tolower()
    # Formando de nuevo las palabras
    palabras <- paste0(primera_letra_palabras, otras_letras)
    # Formando de nuevo el string
    resultado <- paste(palabras, collapse = " ")
    return(resultado)
  }) %>%
    as.character()
  
  # Sustituyendo NA's de nuevo
  resultado[posicion_NAs] <- NA
  return(resultado)
}

################################################################################

# Función auxiliar para estandarizar lo más posible strings escritos de manera
# distinta: dado un vector de strings, a cada una de sus entradas la función le
# aplica lo siguiente:
# 1. lo pasa a minúsculas.
# 2. Le elimina espacios vacíos al principio y al final.
# 3. Le quita acentos a vocales y cambia "ñ" por "ni".
# 4. Cambia: comas, puntos, dos puntos, puntos y comas, diagonales, símbolos de
# interrogación, admiración, paréntesis, guiones altos y espacio vacíos por guiones
# bajos.
# 5. Después del procedimiento anterior, si encuentra dos o más guines bajos pegados
# los colapsa en uno solo
# vec: vector de strings a estandarizar
# La función regresa un vector de la misma longitud que vec, pero habiéndole
# aplicado el procedimiento anterior.
estandariza_strings <- function(vec){
  resultado <- vec %>%
    tolower() %>%
    stri_trim_both() %>%
    stri_replace_all_coll(
      c("á","é","í","ó","ú","ñ",",",".",":",";","/","¿","?","¡","!","(",")","-"," "),
      c("a","e","i","o","u","ni","_","_","_","_","_","_","_","_","_","_","_","_","_"),
      vectorize_all = FALSE) %>%
    stri_replace_all_regex("_+", "_")
  
  return(resultado)
}