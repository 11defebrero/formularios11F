

#' Leer fichero de configuración de la edición
#'
#' @param config_file ruta al fichero JSON de configuración
#'
#' @return objeto de configuración
#' @export
leer_config <- function(config_file) {

  config <- rjson::fromJSON(file=config_file)

  Sys.setenv("11F_CODPOSTALES_FILE_ID" = config$id_sheets_googledrive$codpostales)

  return(config)

}

get_codpostales_file_id <- function() {

  Sys.getenv("11F_CODPOSTALES_FILE_ID")

}
