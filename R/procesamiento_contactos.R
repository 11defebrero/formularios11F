

COLS_CONTACTOS_CENTROS <- c(
  "id",
  "procesado", "fallos",
  "timestamp",
  "referencia_centro", "centro", "email_centro",
  "nombre", "email", "telefono",
  "mensaje",
  "validacion"
)



#' Get contactos con centros (fichero original)
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset original de contactos con centros
#'         con datos tal cual están en Google Drive
#'         y nombres de columnas adaptados
#' @export
#'
get_contactos_centros_original <- function(file_id, filename=NULL) {

  get_drive_sheet(file_id, filename) %>%
    dplyr::transmute(
      procesado = "",
      fallos = "",
      timestamp = as.POSIXct(`Marca temporal`, format="%d/%m/%Y %T"),
      email = `Dirección de correo electrónico`,
      referencia_centro = `Referencia de la solicitud`,
      nombre = `Nombre y apellidos`,
      telefono = `Teléfono de contacto`,
      mensaje = `Mensaje para el centro`,
      centro = `Nombre del centro`
    ) %>%
    genera_id(prefix="M") %>%
    dplyr::select(setdiff(COLS_CONTACTOS_CENTROS, c("email_centro", "validacion")))

}



#' Get contactos con centros (fichero limpio)
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset limpio de contactos con centros
#' @export
#'
get_contactos_centros_limpio <- function(file_id, filename=NULL) {

  get_drive_sheet(file_id, filename) %>%
    dplyr::select(COLS_CONTACTOS_CENTROS) %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, format="%d/%m/%Y %T")
    )

}



#' Upload contactos con centros (fichero limpio)
#'
#' @param dataset dataset limpio de contactos con centros
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#'
#' @export
#'
upload_contactos_centros_limpio <- function(dataset, file_id) {

  dataset %>%
    dplyr::mutate(
      timestamp = format(timestamp, format="%d/%m/%Y %T")
    ) %>%
    dplyr::select(COLS_CONTACTOS_CENTROS) %>%
    upload_drive_sheet(file_id)

}



#' Limpia campos del dataset de contactos con centros
#'
#' @param dataset datos de contactos con centros
#'
#' @return dataset de contactos con centros procesado
#' @export
#'
limpia_contactos_centros <- function(dataset) {

  limpia_campos(dataset,
                campos = setdiff(COLS_CONTACTOS_CENTROS,
                                 c("id", "procesado", "fallos",
                                   "timestamp", "email_centro", "validacion")),
                col_fallos = "fallos")

}



#' Marca duplicados del dataset de contactos con centros, en la columna "procesado"
#'
#' @param dataset dataset con los datos de los contactos con centros en bruto
#'
#' @return dataset corregido
#' @export
marca_duplicados_contactos_centros <- function(dataset) {
  dataset %>%
    dplyr::mutate(
      procesado = ifelse(es_duplicado_contacto_centro(dataset), "DUPLICADO", procesado)
    )
}



#' Indica las peticiones de contacto con centro que son duplicados,
#' manteniendo la última como la correcta
#'
#' @param dataset dataset con los datos de los contactos con centros
#'
#' @return vector lógico indicando si cada petición es un duplicado o no
es_duplicado_contacto_centro <- function(dataset) {
  duplicated(dataset[c("email", "referencia_centro", "centro")], fromLast=TRUE)
}



#' Valida que las peticiones de contacto tengan referencia y nombre de centro concordantes.
#'
#' @param dataset datos de contactos con centros
#' @param solicitudes datos de solicitudes de charlas
#'
#' @return dataset de contactos con centros con columna "validacion" que indica si la petición es correcta
#' @export
#'
completa_info_contactos_centros <- function(dataset, solicitudes) {

  solicitudes %>%
    dplyr::select(referencia_centro = id, nombre_centro_oficial = centro, email_centro = email) %>%
    dplyr::right_join(dataset, by="referencia_centro") %>%
    dplyr::mutate(
      validacion = !is.na(email_centro) & centro == nombre_centro_oficial,
      nombre_centro_oficial = NULL
    )

}




