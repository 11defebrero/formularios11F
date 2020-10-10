

COLS_SOLICITUDES_CHARLAS <- c(
  "id",
  "procesado", "fallos", "fallos_geolocalizacion",
  "timestamp",
  "nombre", "email", "centro",
  "niveles", "tipos", "aforo",
  "videollamada", "ingles",
  "com_autonoma", "provincia", "localidad", "direccion", "codpostal",
  "web", "telefono",
  "comentario",
  "lon", "lat",
  "pdup"
)



#' Get solicitudes charlas (fichero original)
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset original de solicitudes de charlas
#'         con datos tal cual están en Google Drive
#'         y nombres de columnas adaptados
#' @export
#'
get_solicitudes_charlas_original <- function(file_id, filename=NULL) {

  get_drive_sheet(file_id, filename) %>%
    dplyr::transmute(
      procesado = "",
      fallos = "",
      fallos_geolocalizacion = "",
      timestamp = as.POSIXct(Timestamp, format="%d/%m/%Y %T"),
      nombre = `Nombre y apellidos de la persona que hace la solicitud`,
      email = `Email address`,
      centro = `Nombre del centro educativo`,
      niveles = `Nivel o niveles para los que se solicita la charla`,
      tipos = `¿Qué tipo de charlas preferís?`,
      aforo = Aforo,
      videollamada = `¿Estaríais interesados/as en una charla via Skype?`,
      ingles = `¿Podría ser en inglés?`,
      com_autonoma = `Comunidad autónoma`,
      provincia = Provincia,
      localidad = Localidad,
      direccion = `Dirección (nombre y número de la vía)`,
      codpostal = `Código postal`,
      web = `Página web del centro educativo`,
      telefono = `Teléfono de contacto`,
      comentario = `¿Hay algo más que quieras añadir?`
    ) %>%
    genera_id() %>%
    dplyr::select(setdiff(COLS_SOLICITUDES_CHARLAS, c("lon", "lat", "pdup")))

}



#' Get solicitudes charlas (fichero limpio)
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset limpio de solicitudes de charlas
#' @export
#'
get_solicitudes_charlas_limpio <- function(file_id, filename=NULL) {

  get_drive_sheet(file_id, filename) %>%
    dplyr::select(COLS_SOLICITUDES_CHARLAS) %>%
    dplyr::mutate(
      id = stringr::str_pad(id, width=10, pad=0, side="left"),
      timestamp = as.POSIXct(timestamp, format="%d/%m/%Y %T"),
      codpostal = stringr::str_pad(codpostal, width=5, pad=0, side="left"),
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )

}



#' Upload solicitudes charlas (fichero limpio)
#'
#' @param dataset dataset limpio de solicitudes de charlas
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#'
#' @export
#'
upload_solicitudes_charlas_limpio <- function(dataset, file_id) {

  dataset %>%
    dplyr::mutate(
      timestamp = format(timestamp, format="%d/%m/%Y %T")
    ) %>%
    dplyr::select(COLS_SOLICITUDES_CHARLAS) %>%
    upload_drive_sheet(file_id)

}



#' Limpia campos del dataset de solicitudes de charlas
#'
#' @param dataset datos de solicitudes de charlas
#'
#' @return dataset de solicitudes de charlas procesado
#' @export
#'
limpia_solicitudes_charlas <- function(dataset) {

  limpia_campos(dataset,
                campos = setdiff(COLS_SOLICITUDES_CHARLAS,
                                 c("id", "procesado", "fallos", "fallos_geolocalizacion",
                                   "timestamp", "lon", "lat", "pdup")),
                col_fallos = "fallos")

}



#' Geolocaliza los centros del dataset de solicitudes de charlas
#'
#' @param dataset datos de solicitudes de charlas
#'
#' @return dataset de solicitudes de charlas geolocalizado
#' @export
#'
geolocaliza_solicitudes_charlas <- function(dataset) {

  geolocaliza_centros(dataset,
                      col_codpostal = "codpostal",
                      col_municipio = "localidad",
                      col_provincia = "provincia",
                      col_comautonoma = "com_autonoma",
                      col_fallo = "fallos_geolocalizacion")

}



#' Elimina las solicitudes de charlas que ya han sido concertadas
#'
#' @param dataset datos de solicitudes de charlas
#' @param concertadas dataset de charlas concertadas
#'
#' @return dataset de solicitudes de charlas restantes (aún no concertadas)
#' @export
#'
elimina_solicitudes_charlas_concertadas <- function(dataset, concertadas) {

concertadas2 <- concertadas %>%
    dplyr::select(id = referencia_centro, niveles = niveles) %>%
    dplyr::mutate(estado = "concertada")

concertadas_extendida <- tidyr::separate_rows(concertadas2, niveles, sep = ", ")

solicitudes_restantes_extendida <- tidyr::separate_rows(solicitudes_restantes, niveles, sep = ", ")

# Quiero todas las solicitudes
todas <- merge(solicitudes_restantes_extendida, concertadas_extendida, all.x = T)
todas$estado[is.na(todas$estado)] <- "pendiente"
solicitadas_pendientes <- todas[todas$estado == "pendiente", ]

solicitadas_pendientes <- tidyr::pivot_wider(solicitadas_pendientes, names_from = niveles, values_from = niveles)

solicitadas_niveles <- solicitadas_pendientes[ ,
                                               c("Infantil 3 años", "Infantil 4 años", "Infantil 5 años",
            "1º Primaria",  "2º Primaria", "3º Primaria", "4º Primaria", "5º Primaria", "6º Primaria",
            "1º ESO", "2º ESO", "3º ESO", "4º ESO",
            "1º Bachillerato", "2º Bachillerato",
            "Formación profesional", "Adultos", "Otros") ]

solicitadas_niveles <- tidyr::unite(solicitadas_niveles, col = "niveles", sep=", ",
                                    remove=T, na.rm =T)

solicitadas_restantes <- cbind(solicitadas_pendientes[ , 1:23], solicitadas_niveles)
solicitadas_restantes <- solicitadas_restantes[ ,
                                        c("id", "procesado", "fallos",  "fallos_geolocalizacion",
                                          "timestamp", "nombre", "email", "centro",
                                          "niveles", "tipos", "aforo", "videollamada",
                                          "ingles", "com_autonoma", "provincia",  "localidad",
                                          "direccion", "codpostal", "web", "telefono",
                                          "comentario", "lon", "lat", "pdup") ]

return(solicitadas_restantes)

  # concertadas %>%
  #   dplyr::select(id = referencia_centro, niveles_concertados = niveles) %>%
  #   dplyr::right_join(dataset, by="id") %>%
  #   tidyr::replace_na(list(niveles_concertados = "")) %>%
  #   dplyr::mutate(
  #     niveles = strsplit(niveles, split=", "),
  #     niveles_concertados = strsplit(niveles_concertados, split=", "),
  #     niveles = mapply(function(x, y) paste(setdiff(x, y), collapse=", "), niveles, niveles_concertados),
  #     niveles_concertados = NULL
  #   ) %>%
  #   dplyr::filter(niveles != "")
  # todo el código comentado es lo que había antes
}

