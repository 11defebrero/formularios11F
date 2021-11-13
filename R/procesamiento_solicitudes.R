

COLS_SOLICITUDES_CHARLAS <- c(
  "id",
  "procesado", "fallos", "fallos_geolocalizacion", "id_referencia",
  "timestamp",
  "nombre", "email", "centro",
  "niveles", "tipos", "aforo",
  "formato", "herramientas_online", "idioma",
  "com_autonoma", "provincia", "localidad", "direccion", "codpostal",
  "web",
  "comentario",
  "lon", "lat"
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
get_solicitudes_charlas_original <- function(file_id, filename = NULL) {

  data <- get_drive_sheet(file_id, filename) %>%
    dplyr::transmute(
      procesado = "",
      fallos = "",
      fallos_geolocalizacion = "",
      id_referencia = "",
      timestamp = as.POSIXct(`Marca temporal`, format="%d/%m/%Y %T"),
      email = `Dirección de correo electrónico`,
      nombre = `Nombre y apellidos de la persona que hace la solicitud`,
      centro = `Nombre del centro educativo`,
      web = `Página web del centro educativo`,
      com_autonoma = `Comunidad autónoma`,
      provincia = Provincia,
      localidad = Localidad,
      direccion = `Dirección (nombre y número de la vía)`,
      codpostal = `Código postal`,
      niveles = `Nivel o niveles para los que se solicita la charla`,
      tipos = `¿Qué tipo de charlas preferís?`,
      aforo = `Número estimado de asistentes`,
      formato = `¿Cómo os gustaría que se realizara la charla?`,
      herramientas_online = `En caso de ser online, ¿qué herramienta os gustaría que se utilizara preferentemente?`,
      idioma = `Preferiblemente, ¿en qué idioma os gustaría recibir la charla?`,
      # telefono = `Teléfono de contacto`,
      tratdatos = `Tratamiento de datos`,
      comentario = `¿Hay algo más que quieras añadir?`
    ) %>%
    genera_id() %>%
    dplyr::select(setdiff(COLS_SOLICITUDES_CHARLAS, c("lon", "lat")))

  return(data)
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

  data <- get_drive_sheet(file_id, filename) %>%
    dplyr::select(COLS_SOLICITUDES_CHARLAS) %>%
    dplyr::mutate(
      id = stringr::str_pad(id, width=10, pad=0, side="left"),
      timestamp = as.POSIXct(timestamp, format="%d/%m/%Y %T"),
      codpostal = stringr::str_pad(codpostal, width=5, pad=0, side="left"),
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )

  return(data)

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
                                 c("id", "procesado",
                                   "fallos", "fallos_geolocalizacion", "id_referencia",
                                   "timestamp", "lon", "lat")),
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

#' Marca duplicados del dataset de solicitudes de charlas, en la columna "procesado"
#'
#' @param dataset datos de solicitudes de charlas
#'
#' @return dataset corregido
#' @export
marca_duplicados_solicitudes_charlas <- function(dataset) {

  campos_duplicado <- c("nombre", "email", "centro", "niveles", "tipos")
  campos_posible <- c("email")

  dataset %>%
    dplyr::mutate(
      id_referencia = referencia_duplicado_solicitud_charla(dataset, campos_duplicado),
      id_referencia_posible = referencia_duplicado_solicitud_charla(dataset, campos_posible),
      duplicado_seguro = !is.na(id_referencia),
      duplicado_posible = !is.na(id_referencia_posible),
      id_referencia = dplyr::coalesce(id_referencia, id_referencia_posible),
      id_referencia_posible = NULL
    )

}

#' Indica las solicitudes de charla que son duplicados,
#' manteniendo la última como la correcta
#'
#' @param dataset datos de solicitudes de charlas
#'
#' @return vector lógico indicando si cada solicitud es un duplicado o no
es_duplicado_solicitud_charla <- function(dataset, campos_combrobacion) {
  !is.na(referencia_duplicado_solicitud_charla(dataset, campos_combrobacion))
}

#' Indica las solicitudes de charla que son duplicados, devolviendo el ID de la correcta (la última)
#'
#' @param dataset datos de solicitudes de charlas
#'
#' @return vector con ID de la solicitud correcta si es duplicado, NA si no
referencia_duplicado_solicitud_charla <- function(dataset, campos_combrobacion) {

  orden_inicial <- dataset$id
  dataset_ordenado <- dataset %>% arrange(desc(timestamp)) # La última solicitud es la correcta
  original_ids <- original_vs_duplicados(dataset_ordenado, campos_combrobacion)
  original_ids_orden_inicial <- original_ids[match(orden_inicial, dataset_ordenado$id)]

  return(original_ids_orden_inicial)
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

solicitadas_niveles <- solicitadas_pendientes[ , OPCIONES_NIVEL]

solicitadas_niveles <- tidyr::unite(solicitadas_niveles, col = "niveles", sep=", ",
                                    remove=T, na.rm =T)

solicitadas_restantes <- cbind(solicitadas_pendientes[ , 1:23], solicitadas_niveles)
solicitadas_restantes <- solicitadas_restantes[ , COLS_SOLICITUDES_CHARLAS ]


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
