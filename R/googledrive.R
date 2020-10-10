

#' Get drive sheet
#'
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#' @param filename Opcional. Nombre del fichero (incluyendo extensión ".csv") para guardar el dataset en disco.
#'
#' @return dataset descargado tal cual está en Google Drive
#'
get_drive_sheet <- function(file_id, filename = NULL) {

  tmpfile <- ifelse(!is.null(filename), filename, paste0(tempfile(), ".csv"))

  googledrive::drive_download(file = googledrive::as_id(file_id),
                              path = tmpfile,
                              type = "csv",
                              overwrite = TRUE, verbose = FALSE)

  data <- readr::read_csv(tmpfile,
                          col_types = readr::cols(.default=readr::col_character()),
                          locale = readr::locale(encoding="UTF-8"),
                          na = c("NA", "no aplica", "")
                          )

  if (is.null(filename)) file.remove(tmpfile)

  return(data)
}



#' Upload drive sheet
#'
#' @param dataset dataset a subir
#' @param file_id ID de Google Drive de la spreadsheet con los datos
#'
upload_drive_sheet <- function(dataset, file_id, filename = NULL) {

  tmpfile <- ifelse(!is.null(filename), filename, paste0(tempfile(), ".csv"))

  write.csv(dataset, tmpfile, row.names=FALSE, quote=TRUE, na="")

  googledrive::drive_update(file = googledrive::as_id(file_id),
                            media = tmpfile,
                            verbose = FALSE)

  if (is.null(filename)) file.remove(tmpfile)

  message("\nDone.")
}
