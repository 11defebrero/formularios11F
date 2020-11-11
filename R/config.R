

#' Get drive json
#'
#' @param file_id ID de config.json
#' @param filename Nombre del fichero temporal que se descarga de drive.
#'
#' @return datos de configuracion, fundamentalmente ids de sheets y formularios en drive
#'
get_drive_json <- function(file_id, filename = NULL) {
  
  tmpfile <- ifelse(!is.null(filename), filename, tempfile() )
  
  googledrive::drive_download(file = googledrive::as_id(file_id),
                              path = tmpfile,
                              overwrite = TRUE, verbose = FALSE)
  
  json_content <- rjson::fromJSON(file=filename)
  
  if (is.null(filename)) file.remove(tmpfile)
  
  return(json_content)
}

#' Leer fichero de configuración de la edición
#'
#' @param config_file ruta al fichero JSON de configuración
#'
#' @return objeto de configuración
#' @export
leer_config <- function(config_file) {

  config1 <- rjson::fromJSON(file=config_file)
  config2 <- get_drive_json_config(file_id = config1$ids_googledrive$id_config_file, "config/config_temp_file.json")
  Sys.setenv("11F_CODPOSTALES_FILE_ID" = config2$ids_googledrive$codpostales)
  
  return(config2)

}

get_codpostales_file_id <- function() {

  Sys.getenv("11F_CODPOSTALES_FILE_ID")

}
