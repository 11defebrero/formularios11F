

#' Validación campo de nombre
#'
#' @param nombres vector de nombres
#'
#' @return vector de booleans que indica si cada nombre es válido o no
es_valido_nombre <- function(nombres) {
  !grepl("@", nombres) & # Posibles emails
    !grepl("[0-9]", nombres) # Posibles teléfonos
}



#' Validación campo de email
#'
#' @param email vector de email
#'
#' @return vector de booleans que indica si cada email es válido o no
es_valido_email <- function(email) {
  grepl("^[^ ]+@[^ ]+[.][^ ]+$", email)
}



#' Validación campo de centro
#'
#' @param centros vector de centros
#'
#' @return vector de booleans que indica si cada centro es válido o no
es_valido_centro <- function(centros) {
  rep(TRUE, length(centros))
}


#' Validación campo de nivel
#'
#' @param niveles vector de niveles
#'
#' @return vector de booleans que indica si cada enumeración de niveles es válida o no
es_valido_niveles <- function(niveles) {
  rep(TRUE, length(niveles))
}


#' Validación campo de tipo
#'
#' @param tipos vector de tipos
#'
#' @return vector de booleans que indica si cada enumeración de tipos es válida o no
es_valido_tipos <- function(tipos) {
  rep(TRUE, length(tipos))
}


#' Validación campo de aforo
#'
#' @param aforos vector de aforos
#'
#' @return vector de booleans que indica si cada aforo es válido o no
es_valido_aforo <- function(aforos) {
  rep(TRUE, length(aforos))
}


#' Validación campo de videollamada
#'
#' @param opciones_pres_online vector de opciones de videollamada
#'
#' @return vector de booleans que indica si cada opción de videollamada es válida o no
es_valido_pres_online <- function(pres_online) {
  rep(TRUE, length(pres_online))
}


#' Validación campo de herramientas online
#'
#' @param herramientas_online vector de herramientas online
#'
#' @return vector de booleans que indica si cada enumeración de herramientas online es válida o no
es_valido_herramientas_online <- function(herramientas_online) {
  rep(TRUE, length(herramientas_online))
}


#' Validación campo de inglés
#'
#' @param opciones_ingles vector de opciones de charla en inglés
#'
#' @return vector de booleans que indica si cada opción de charla en inglés es válida o no
es_valido_idioma <- function(idioma) {
  rep(TRUE, length(idioma))
}


#' Validación campo de comunidad autónoma
#'
#' @param com_autonomas vector de comunidades autónomas
#'
#' @return vector de booleans que indica si cada comunidad autónoma es válida o no
es_valido_comunidad_autonoma <- function(com_autonomas) {
  com_autonomas %in% OPCIONES_CCAA_INE
}


#' Validación campo de provincia
#'
#' @param provincias vector de provincias
#'
#' @return vector de booleans que indica si cada provincia es válida o no
es_valido_provincia <- function(provincias) {
  provincias %in% OPCIONES_PROVINCIA_INE
}


#' Validación campo de localidad
#'
#' @param localidades vector de localidades
#'
#' @return vector de booleans que indica si cada localidad es válida o no
es_valido_localidad <- function(localidades) {
  !grepl("@", localidades) & # Posibles emails
    !grepl("[0-9]", localidades) # Posibles teléfonos
}


#' Validación campo de direccion
#'
#' @param direcciones vector de direcciones
#'
#' @return vector de booleans que indica si cada direccion es válida o no
es_valido_direccion <- function(direcciones) {
  rep(TRUE, length(direcciones))
}


#' Validación campo de código postal
#'
#' @param cod_postales vector de códigos postales
#'
#' @return vector de booleans que indica si cada código postal es válido o no
es_valido_codigo_postal <- function(cod_postales) {
  grepl("[0-9]{5}", cod_postales) &
    as.numeric(cod_postales) > 1000 &
    as.numeric(cod_postales) < 53000
}


#' Validación campo de web
#'
#' @param webs vector de webs
#'
#' @return vector de booleans que indica si cada web es válida o no
es_valido_web <- function(webs) {
  web_existe <- rep(FALSE, times=length(webs))
  for (i in which(!is.na(webs))) {
    try({
      web_req <- httr::GET(webs[i])
      web_existe[i] <- TRUE
    }, silent=TRUE)
  }
  return(web_existe | is.na(webs))
}


#' Validación campo de teléfono
#'
#' @param telefonos vector de teléfonos
#'
#' @return vector de booleans que indica si cada teléfono es válido o no
es_valido_telefono <- function(telefonos) {
  is.na(telefonos) | grepl("[0-9]{9}", telefonos)
}


#' Validación campo de comentario
#'
#' @param comentarios vector de comentarios
#'
#' @return vector de booleans que indica si cada comentario es válido o no
es_valido_comentario <- function(comentarios) {
  rep(TRUE, length(comentarios))
}



#' Validación campo de referencia
#'
#' @param referencias vector de referencias
#'
#' @return vector de booleans que indica si cada formato de referencia es válido o no
es_valida_referencia <- function(referencias) {
  grepl("^[0-9]{10}$", referencias)
}



#' Validación tipo de charla concertada
#'
#' @param tipos vector de tipos de charla
#'
#' @return vector de booleans que indica si cada tipo de charla es válido o no
es_valido_tipo_charla <- function(tipos) {
  tipos %in% c("Presencial", "Videollamada")
}


#' Validación número entero
#'
#' @param tipos vector de números enteros
#'
#' @return vector de booleans que indica si cada número es válido o no
es_valido_numero <- function(numeros) {
  numeros_int <- suppressWarnings(as.integer(numeros))
  !is.na(numeros_int) & numeros_int %% 1 == 0
}


#' Validación contestación Sí/No
#'
#' @param tipos vector de contestaciones
#'
#' @return vector de booleans que indica si cada contestación es válida o no
es_valido_si_no <- function(si_no) {
  si_no %in% c("Sí", "No")
}


#' Validación campo imagen
#'
#' @param imagenes vector de aforos
#'
#' @return vector de booleans que indica si cada aforo es válido o no
es_valido_imagen <- function(imagenes) {
  rep(TRUE, length(imagenes))
}

