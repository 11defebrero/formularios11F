
#' Genera campo id en el dataset de acuerdo al timestamp de rellenado del formulario
#'
#' @param dataset tabla con los datos de formulario de googlesheets
#' @param prefix prefijo opcional que añadir a la ID
#'
#' @return dataset con un campo adicional para la id
genera_id <- function(dataset, prefix="") {
  # Genero id : mes dia hora minuto segundo (todo junto)
  dataset$id <- format(dataset$timestamp, paste0(prefix, "%m%d%H%M%OS"))
  # compruebo que la id no está duplicada
  if(sum(duplicated(dataset$id))>0){
    print("Cuidado, hay algún duplicado en el campo id que se acaba de generar")
  }
  return(dataset)
}



#' Limpia campos especificados de un dataset
#' Aplica las funciones de corrección y validación correspondientes a cada campo
#' y completa la información de fallos encontrados en los datos.
#'
#' @param dataset datos a limpiar
#' @param campos nombres de los campos a limpiar
#' @param col_fallos nombre de la columna del dataset que almacena los fallos
#'
#' @return dataset con datos limpios y reporte de fallos
limpia_campos <- function(dataset, campos, col_fallos) {

  for (campo in campos) {

    cat("Procesando ", campo, "... ", sep = "")

    fun_corrige_campo <- FUNCIONES_PROCESAMIENTO_CAMPOS[[campo]]$fun_corrige
    dataset[[campo]] <- fun_corrige_campo(dataset[[campo]])

    fun_es_valido_campo <- FUNCIONES_PROCESAMIENTO_CAMPOS[[campo]]$fun_valida
    validez <- fun_es_valido_campo(dataset[[campo]])

    dataset[[col_fallos]][validez == FALSE] <- paste0(dataset[[col_fallos]][validez == FALSE], campo, ". ")

    cat("ok\n")
  }

  return(dataset)
}



#' Geolocaliza centros según sus datos postales
#' De momento lo único que hace es mirar las coordenadas del código postal
#' Si hay algún fallo de geolocalización informa en la columna de fallo
#'
#' @param dataset datos a geolocalizar
#' @param codpostales_file_id ID de Google Drive del fichero donde está el listado de códigos postales válidos
#' @param col_codpostal nombre de la columna con el código postal
#' @param col_municipio nombre de la columna con el municipio
#' @param col_provincia nombre de la columna con la provincia
#' @param col_comautonoma nombre de la columna con la comunidad autónoma
#' @param col_fallo nombre de la columna donde se registran los fallos
#'
#' @return dataset con nuevas columnas lon, lat con las coordenadas
#'
geolocaliza_centros <- function(dataset, col_codpostal,
                                col_municipio, col_provincia, col_comautonoma, col_fallo) {

  codpostales <- get_drive_sheet(file_id = get_codpostales_file_id()) %>%
    dplyr::filter(startsWith(codpostal, id_provincia)) %>%
    dplyr::transmute(codpostal, lon, lat,
                     codpostales_provincia = provincia,
                     codpostales_ccaa = ccaa) %>%
    unique()

  names(codpostales)[1] <- col_codpostal

  dataset <- dataset %>% dplyr::left_join(codpostales, by = col_codpostal)

  solicitudes_new <- solicitudes_new %>% dplyr::left_join(codpostales, by = col_codpostal)

  dataset[[col_fallo]][is.na(dataset[[col_fallo]])] <- ""

  cp_noexiste <- is.na(dataset$codpostales_provincia)
  dataset[[col_fallo]][cp_noexiste] <- paste0(dataset[[col_fallo]][cp_noexiste], "CP no listado. ")

  cp_otraprov <- !is.na(dataset$codpostales_provincia) & dataset[[col_provincia]] != dataset$codpostales_provincia
  dataset[[col_fallo]][cp_otraprov] <- paste0(dataset[[col_fallo]][cp_otraprov], "CP de otra provincia. ")

  cp_otraccaa <- !is.na(dataset$codpostales_ccaa) & dataset[[col_comautonoma]] != dataset$codpostales_ccaa
  dataset[[col_fallo]][cp_otraccaa] <- paste0(dataset[[col_fallo]][cp_otraccaa], "CP de otra comunidad autónoma. ")


  dataset$codpostales_provincia <- NULL
  dataset$codpostales_ccaa <- NULL

  return(dataset)
}



#' Indica el ID original en los registros duplicados
#' El original es el primer registro
#'
#' @param dataset dataset a comprobar
#' @param campos_combrobacion nombres de columnas a incluir en la comprobación
#'
#' @return vector con ID original si es duplicado, NA si no
#'
original_vs_duplicados <- function(dataset, campos_combrobacion) {

  original_ids <- dataset %>%
    dplyr::group_by_at(campos_combrobacion) %>%
    dplyr::mutate(
      original = id[1] # El original es el primer registro
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      original = ifelse(id == original, NA, original)
    ) %>%
    dplyr::pull(original)

  return(original_ids)
}

