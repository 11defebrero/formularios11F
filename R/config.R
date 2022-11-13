

#' Leer fichero de configuración de la edición
#'
#' @param config_file ruta al fichero JSON de configuración
#'
#' @return objeto de configuración
#' @export
leer_config <- function(config_file) {

  config1 <- rjson::fromJSON(file = config_file)
  config2 <- get_drive_json(file_id = config1$ids_googledrive$id_config_file,
                            "config/config_temp_file.json")
  Sys.setenv("11F_CODPOSTALES_FILE_ID" = config2$ids_googledrive$codpostales)

  return(config2)

}

get_codpostales_file_id <- function() {

  Sys.getenv("11F_CODPOSTALES_FILE_ID")

}
