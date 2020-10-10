

COLS_CHARLAS_CONCERTADAS <- c(
  "id",
  "procesado", "fallos", "fallos_geolocalizacion", "fallos_validacion",
  "timestamp",
  "email",
  "es_charla_solicitada", "referencia_centro", "centro",
  "com_autonoma", "provincia", "localidad", "codpostal",
  "ponente", "institucion_ponente", "email_ponente",
  "titulo_charla", "tipo_charla", "niveles", "n_charlas", "n_alumnos",
  "unidad_divulgacion",
  "lon", "lat"
)



#' Get charlas concertadas (fichero original)
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset original de charlas concertadas
#'         con datos tal cual están en Google Drive
#'         y nombres de columnas adaptados
#' @export
#'
get_charlas_concertadas_original <- function(file_id, filename=NULL) {

  get_drive_sheet(file_id, filename) %>%
    dplyr::transmute(
      procesado = "",
      fallos = "",
      fallos_geolocalizacion = "",
      fallos_validacion = "",
      timestamp = as.POSIXct(`Marca temporal`, format="%d/%m/%Y %T"),
      email = `Dirección de correo electrónico`,
      es_charla_solicitada = `¿Está el centro educativo con el que has contactado en la lista de 'Charlas Solicitadas'?`,
      referencia_centro = `Código de referencia de la solicitud`,
      centro = coalesce(`Nombre del centro (check)`, `Nombre del centro`),
      com_autonoma = `Comunidad Autónoma`,
      provincia = coalesce(`Provincia (check)`, `Provincia`),
      localidad = `Localidad`,
      codpostal = `Código Postal`,
      ponente = `Nombre y apellidos ponente(s)`,
      institucion_ponente = `Institución ponente(s)`,
      email_ponente = `e-mail de contacto de ponente (no se publicará en la web)`,
      titulo_charla = `Título de la charla`,
      tipo_charla = `Tipo de charla`,
      niveles = `Cursos/s en el/los que se impartirá la charla`,
      n_charlas = `Número de charlas que se van a impartir en este centro con este título`,
      n_alumnos = `Número aproximado de estudiantes que asistirán a las charlas`,
      unidad_divulgacion = `Si se ha concertado esta charla a través de un proyecto o unidad de divulgación o igualdad, por favor, indícalo:`
    ) %>%
    genera_id(prefix="C") %>%
    dplyr::select(setdiff(COLS_CHARLAS_CONCERTADAS, c("lon", "lat")))

}



#' Get charlas concertadas (fichero limpio)
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset limpio de charlas concertadas
#' @export
#'
get_charlas_concertadas_limpio <- function(file_id, filename=NULL) {

  get_drive_sheet(file_id, filename) %>%
    dplyr::select(COLS_CHARLAS_CONCERTADAS) %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, format="%d/%m/%Y %T"),
      referencia_centro = stringr::str_pad(referencia_centro, width=10, pad=0, side="left"),
      codpostal = stringr::str_pad(codpostal, width=5, pad=0, side="left"),
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )

}



#' Upload charlas concertadas (fichero limpio)
#'
#' @param dataset dataset limpio de charlas concertadas
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#'
#' @export
#'
upload_charlas_concertadas_limpio <- function(dataset, file_id) {

  dataset %>%
    dplyr::mutate(
      timestamp = format(timestamp, format="%d/%m/%Y %T")
    ) %>%
    dplyr::select(COLS_CHARLAS_CONCERTADAS) %>%
    upload_drive_sheet(file_id)

}



#' Limpia campos del dataset de charlas concertadas
#'
#' @param dataset datos de charlas concertadas
#'
#' @return dataset de charlas concertadas procesado
#' @export
#'
limpia_charlas_concertadas <- function(dataset) {

  dataset <- dataset %>% limpia_campos(campos="es_charla_solicitada", col_fallos="fallos")

  nolimpiar_comunes <- c("id", "procesado", "fallos", "fallos_validacion", "fallos_geolocalizacion",
                         "timestamp", "es_charla_solicitada", "lon", "lat")

  dataset_solicitadas <- dataset %>%
    filter(es_charla_solicitada == "Sí") %>%
    limpia_campos(campos = setdiff(COLS_CHARLAS_CONCERTADAS,
                                   c(nolimpiar_comunes, "com_autonoma", "localidad", "codpostal")),
                  col_fallos = "fallos")

  dataset_externas <- dataset %>%
    filter(es_charla_solicitada == "No") %>%
    limpia_campos(campos = setdiff(COLS_CHARLAS_CONCERTADAS,
                                   c(nolimpiar_comunes, "referencia_centro")),
                  col_fallos = "fallos")

  rbind(dataset_solicitadas, dataset_externas) %>%
    arrange(timestamp)
}



#' Marca duplicados del dataset de charlas concertadas, en la columna "procesado"
#'
#' @param dataset dataset con los datos de los charlas concertadas en bruto
#'
#' @return dataset corregido
#' @export
marca_duplicados_charlas_concertadas <- function(dataset) {
  dataset %>%
    dplyr::mutate(
      procesado = ifelse(es_duplicada_charla_concertada(dataset), paste0("DUPLICADO / ", procesado), procesado)
    )
}



#' Indica las peticiones de contacto con centro que son duplicados,
#' manteniendo la última como la correcta
#'
#' @param dataset dataset con los datos de los contactos con centros
#'
#' @return vector lógico indicando si cada petición es un duplicado o no
es_duplicada_charla_concertada <- function(dataset) {
  duplicated(dataset[c("centro", "ponente", "titulo_charla")], fromLast=TRUE)
}



#' Completa la información de ubicación de las charlas concertadas.
#' Para las que vienen de solicitudes, valida que tengan referencia y nombre de centro y provincia concordantes.
#'
#' @param dataset datos de charlas concertadas
#' @param solicitudes datos de solicitudes de charlas
#'
#' @return dataset de contactos con centros con columna "fallos_validacion" que indica si la petición es correcta
#' @export
#'
completa_info_charlas_concertadas <- function(dataset, solicitudes) {

  dataset_solicitadas <- dataset %>%
    dplyr::filter(es_charla_solicitada == "Sí") %>%
    dplyr::select(- com_autonoma, -localidad, -codpostal)

  dataset_externas <- dataset %>%
    dplyr::filter(es_charla_solicitada == "No") %>%
    dplyr::mutate(fallos_validacion = "")

  solicitudes %>%
    dplyr::select(
      referencia_centro = id, nombre_centro_oficial = centro,
      provincia_oficial = provincia, com_autonoma, localidad, codpostal
    ) %>%
    dplyr::right_join(dataset_solicitadas, by="referencia_centro") %>%
    dplyr::mutate(
      fallos_validacion = ifelse(centro != nombre_centro_oficial, "Nombre centro no válido.", ""),
      fallos_validacion = ifelse(provincia != provincia_oficial, paste(fallos_validacion, "Provincia no válida."), fallos_validacion),
      fallos_validacion = ifelse(is.na(nombre_centro_oficial), "Referencia no válida.", fallos_validacion),
      nombre_centro_oficial = NULL,
      provincia_oficial = NULL
    ) %>%
    rbind(dataset_externas) %>%
    dplyr::arrange(timestamp)

}


#' Geolocaliza los centros del dataset de charlas concertadas
#'
#' @param dataset datos de charlas concertadas
#'
#' @return dataset de charlas concertadas geolocalizado
#' @export
#'
geolocaliza_charlas_concertadas <- function(dataset) {

  geolocaliza_centros(dataset,
                      col_codpostal = "codpostal",
                      col_municipio = "localidad",
                      col_provincia = "provincia",
                      col_comautonoma = "com_autonoma",
                      col_fallo = "fallos_geolocalizacion")

}



