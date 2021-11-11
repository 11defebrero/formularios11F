
COLS_ACTIVIDADES_LIMPIO <- c(
  "id", "procesado", "fallos_validacion", "fallos_geo",
  "clase_actividad",
  "timestamp", "email", "nombre", "titulo", "es_centro",

  # localización, compartidos en centros educativos y otras presenciales
  "codpostal", "localidad", "provincia", "com_autonoma", "lon", "lat",

  # exclusivas actividades centros educativos
  "centro",

  # compartidas centro educativo y otras
  "tipo" , "web",  "des",

  # exclusivos otras
  "es_presencial",

  # comunes presencial y no presencial
  "audiencia", "email2",  "telf" ,
  "fecha" , "hora_inicio" ,  "hora_fin",
  "organiza",  "patrocina", "imagen",

  # exclusivas presencial
  "reserva", "espacio", "direccion" # distintas
)

COLS_ACTIVIDADES_CENTROS <- c(
  "id", "timestamp", "email", "nombre",
  "titulo", "es_centro",

  # centro educativo
  "centro",  "codpostal", "localidad", "provincia",
  "com_autonoma",  "tipo", "web",  "des" ,
  "lon", "lat"
)

COLS_OTRAS_PRESENCIALES <- c(
  "id", "timestamp", "email", "nombre",
  "titulo", "es_centro", "es_presencial",

  # presencial
  "tipo",  "audiencia",  "reserva",  "email2",  "telf" , "des",
  "web",  "hora_inicio" ,  "hora_fin", "espacio", "direccion",  "codpostal", "localidad",
  "provincia",  "com_autonoma",   "organiza",  "patrocina", "imagen",
  "lon", "lat"
)

COLS_OTRAS_NO_PRESENCIALES <- c(
  "id",  "timestamp", "email", "nombre",
  "titulo", "es_centro", "es_presencial",

  # no presencial
  "tipo",  "audiencia", "email2", "telf", "des", "web", "fecha",
  "hora_inicio", "hora_fin", "organiza", "patrocina", "imagen"
)


#' Get actividades (fichero original)
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset original de actividades
#'         con datos tal cual están en Google Drive
#'         y nombres de columnas adaptados
#' @export
#'
get_actividades_original <- function(file_id, filename=NULL) {

  get_drive_sheet(file_id, filename) %>%
    dplyr::transmute(
      procesado = "",
      fallos_validacion = "",
      fallos_geo = "",
      clase_actividad = "",
      timestamp = as.POSIXct(`Timestamp`, format="%d/%m/%Y %T"),
      email = `Email address`,
      nombre = `Tu nombre (no se publicará en la web)`,
      titulo = `Título de la actividad (100 caracteres máximo)`,
      es_centro = ifelse(`La actividad está organizada por`== "Un centro educativo (no universitario)", "Sí", "No"),

      # datos de localizacion comunes
      codpostal = ifelse(!is.na(`Código postal`),`Código postal`,`Código postal_1`),
      localidad = ifelse(!is.na(`Municipio`),`Municipio`,`Municipio_1`),
      provincia =	ifelse(!is.na(`Provincia`),`Provincia`,`Provincia_1`),
      com_autonoma = ifelse(!is.na(`Comunidad Autónoma`),`Comunidad Autónoma`,`Comunidad Autónoma_1`),

      # centros educativos
      centro = `Nombre del centro educativo`,

      # otros: presenciales y no presenciales
      es_presencial = ifelse(`Es una actividad...` == "Presencial", "Sí",
                             ifelse(`Es una actividad...` == "No presencial (web, programa de radio, etc)", "No", NA)),

      audiencia = ifelse(es_presencial == "Sí", `Público`, `Dirigido a`),
      email2 = ifelse(es_presencial == "Sí", `Email de información y/o reservas`, `Email de información`),
      telefono = ifelse(es_presencial == "Sí", `Teléfono de contacto (opcional)_1`,`Teléfono de contacto (opcional)`),
      fecha = ifelse(es_presencial == "Sí", `Fecha_1`, `Fecha`),
      hora_inicio = ifelse(es_presencial == "Sí", `Hora de inicio_1`, `Hora de inicio`),
      hora_fin = ifelse(es_presencial == "Sí", `Hora de fin_1`, `Hora de fin`),
      reserva = `Reserva`,
      espacio = `Espacio donde se va a desarrollar la actividad`,
      direccion = `Dirección (nombre y número de la vía)`, # solo en presenciales

      organiza = ifelse(es_centro == "No" & es_presencial == "Sí", `Organiza_1`, `Organiza`),

      patrocina = ifelse(es_centro == "No" & es_presencial == "Sí",
                         `Entidades patrocinadoras y/o colaboradoras_1`,
                         `Entidades patrocinadoras y/o colaboradoras`),

      imagen = ifelse(es_centro == "No" & es_presencial == "Sí", `Imagen_1`, `Imagen`),

      tipo = ifelse(es_centro == "Sí", `Tipo de actividad programada`,
                    ifelse(es_presencial == "Sí",`Tipo de actividad_1`,`Tipo de actividad`)),

      web = ifelse(es_centro == "Sí", `Enlace a web del centro donde se explique la actividad`,
                   ifelse(es_presencial == "Sí", `Página web de la actividad_1`, `Página web de la actividad` )),

      des = ifelse(es_centro == "Sí", `Descripción de la actividad (500 caracteres máximo)`,
                   ifelse(es_presencial == "Sí",`Descripción de la actividad (1000 caracteres máximo)_1`,
                          `Descripción de la actividad (1000 caracteres máximo)`))
    ) %>%
    mutate(
      clase_actividad = case_when(
        es_centro == "Sí" ~ "CENTRO11F",
        es_centro == "No" & es_presencial == "Sí" ~ "PRESENCIAL",
        es_centro == "No" & es_presencial == "No" ~ "NO PRESENCIAL"
      ),
      fecha = ifelse(
        fecha == "No aplica", NA,
        sapply(stringr::str_extract_all(gsub("20[0-9]{2}(,|$)", "", fecha), "[0-9]+"),
               paste, collapse=", ")
      )
    ) %>%
    genera_id(prefix="A") %>%
    dplyr::select(setdiff(COLS_ACTIVIDADES_LIMPIO, c("lon", "lat")))

}


#' Get actividades (fichero limpio)
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset limpio de actividades
#' @export
#'
get_actividades_limpio <- function(file_id, filename=NULL) {

  get_drive_sheet(file_id, filename) %>%
    dplyr::select(COLS_ACTIVIDADES_LIMPIO) %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp),
      codpostal = stringr::str_pad(codpostal, width=5, pad=0, side="left"),
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )

}

#' Upload actividades (fichero limpio)
#'
#' @param dataset dataset limpio de contactos con centros
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#'
#' @export
#'
upload_actividades_limpio <- function(dataset, file_id, filename = NULL) {

  dataset %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, format="%d/%m/%Y %T"),
      codpostal = stringr::str_pad(codpostal, width=5, pad=0, side="left"),
      lon = as.numeric(lon),
      lat = as.numeric(lat)
      ) %>%
    dplyr::select(COLS_ACTIVIDADES_LIMPIO) %>%
    upload_drive_sheet(file_id, filename)

}


#' Limpia campos del dataset de actividades
#'
#' @param dataset datos de actividades (de todas: centros, otras presenciales y otras no presenciales)
#'
#' @return dataset de actividades procesado
#' @export
#'
limpia_actividades <- function(dataset) {

  campos_no_validan <- c("id", "procesado", "fallos_validacion", "fallos_geo",
                         "clase_actividad",
                         "timestamp", "lon", "lat",
                         "fecha", "hora_inicio", "hora_fin"
                         )

  campos_comunes_todas <- c("email", "nombre", "titulo", "es_centro",
                            "tipo" , "web",  "des")
  campos_comunes_centrosypresenciales <- c("codpostal", "localidad", "provincia", "com_autonoma")
  campos_comunes_otras <- c("es_presencial", "audiencia", "email2",  "telf" ,
                            "organiza",  "patrocina", "imagen")

dataset <- dataset %>%
    limpia_campos(campos = campos_comunes_todas,
                  col_fallos = "fallos_validacion")

comunes_centrosypresenciales <- dataset %>%
    filter(es_centro == "Sí" | es_presencial == "Sí") %>%
    limpia_campos(campos = campos_comunes_centrosypresenciales,
                  col_fallos = "fallos_validacion")

dataset <- rbind( comunes_centrosypresenciales, dataset[!dataset$id %in% comunes_centrosypresenciales$id,])

comunes_otras <- dataset %>%
  filter(es_centro == "No") %>%
  limpia_campos(campos = campos_comunes_otras,
                  col_fallos = "fallos_validacion")

dataset <- rbind( comunes_otras, dataset[!dataset$id %in% comunes_otras$id,])

centros <- dataset %>%
    filter(es_centro == "Sí") %>%
    limpia_campos(campos = setdiff(COLS_ACTIVIDADES_CENTROS,
                                            c(campos_comunes_todas,
                                              campos_comunes_centrosypresenciales,
                                              campos_no_validan)),
                           col_fallos = "fallos_validacion")

dataset <- rbind( centros, dataset[!dataset$id %in% centros$id,])

presenciales <- dataset %>%
    filter(es_presencial == "Sí") %>%
    limpia_campos(campos = setdiff(COLS_OTRAS_PRESENCIALES,
                                          c(campos_comunes_todas,
                                            campos_comunes_centrosypresenciales,
                                            campos_comunes_otras,
                                            campos_no_validan)),
                col_fallos = "fallos_validacion")

dataset <- rbind(presenciales, dataset[!dataset$id %in% presenciales$id,])

no_presenciales <- dataset %>%
    filter(es_centro == "No", es_presencial== "No") %>%
    limpia_campos(campos = setdiff(COLS_OTRAS_NO_PRESENCIALES,
                                          c(campos_comunes_todas,
                                            campos_comunes_otras,
                                            campos_no_validan)),
                col_fallos = "fallos_validacion")

dataset <- rbind(no_presenciales, dataset[!dataset$id %in% no_presenciales$id,])

}


#' Marca duplicados del dataset de contactos con centros, en la columna "procesado"
#'
#' @param dataset dataset con los datos de los contactos con centros en bruto
#'
#' @return dataset con los duplicados marcados
#' @export
marca_duplicados_actividades <- function(dataset) {
  dataset %>%
    dplyr::mutate(
      procesado = ifelse(es_duplicado_actividad(dataset), "DUPLICADO", procesado)
    )
}


#' Indica las actividades que están duplicadas,
#' manteniendo la última como la correcta
#'
#' @param dataset dataset con los datos de las actividades
#'
#' @return vector lógico indicando si cada petición es un duplicado o no
es_duplicado_actividad <- function(dataset) {
  duplicated(dataset[c("email", "titulo", "clase_actividad", "nombre", "titulo", "es_centro",
                       "codpostal", "localidad", "centro", "tipo" , "web"
                       )], fromLast = TRUE)
}


#' Geolocaliza los centros del dataset de solicitudes de charlas
#'
#'
#' @param dataset datos de actividades
#'
#' @return dataset de actividades geolocalizado
#' @export
#'
geolocaliza_actividades <- function(dataset) {
  geolocaliza_centros(dataset,
                      col_codpostal = "codpostal",
                      col_municipio = "localidad",
                      col_provincia = "provincia",
                      col_comautonoma = "com_autonoma",
                      col_fallo = "fallos_geo")
}

