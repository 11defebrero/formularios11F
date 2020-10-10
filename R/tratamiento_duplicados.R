

#' Elimina duplicados del dataset de solicitudes
#'
#' @param dataset dataset con los datos de las solicitudes en bruto
#'
#' @return dataset corregido
#' @export
elimina_duplicados <- function(dataset) {
  # Campos que comprueba: "nombre"  "email"   "centro"  "niveles" "tipos" "telefono"
  campos_combrobacion <- c("nombre", "email", "centro", "niveles", "tipos", "telefono")
  paste(sum(duplicated(dataset[, campos_combrobacion], fromLast = TRUE)), "duplicados")
  # Elimino los duplicados
  dataset <- dataset[!duplicated(dataset[, campos_combrobacion], fromLast = TRUE), ]
  return(dataset)
}


#' Marca duplicados en el dataset de solicitudes
#'
#' @param dataset tabla con los datos de solicitudes de googlesheets
#'
#' @return dataset marcado con DUP
#' @export
marca_duplicados <- function(dataset) {
  # Marca los posibles duplicados: email duplicado
  dataset$pdup <- duplicated(dataset[, "email"], fromLast = TRUE) | duplicated(dataset[, "email"], fromLast = FALSE)
  return(dataset)
}
