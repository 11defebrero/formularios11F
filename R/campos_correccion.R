

#' Corrección campo de nombre
#'
#' @param nombres vector de nombres
#'
#' @return vector de nombres corregidos
corrige_nombre <- function(nombres) {
  stringr::str_to_title(nombres, locale="es")
}



#' Corrección campo de email
#'
#' @param email vector de email
#'
#' @return vector de emails corregidos
corrige_email <- function(email) {
  tolower(email)
}



#' Corrección campo de centro
#'
#' @param centros vector de centros
#'
#' @return vector de centros corregidos
corrige_centro <- function(centros) {

  centros <- toupper(centros)
  centros <- chartr('ÁÉÍÓÚ','AEIOU', centros)

  centros <- gsub('[[:punct:]]+','', centros)
  centros <- gsub('  ',' ', centros)
  centros <- gsub("I[.]? ?E[.]? ?S[.]?O[.]?", "IESO", centros)
  centros <- gsub("I[.]? ?E[.]? ?S[.]?", "IES", centros)
  centros <- gsub("C[.]? ?E[.]? ?I[.]? ?P[.]?", "CEIP", centros)
  centros <- gsub("C[.]? ?R[.]? ?A[.]?", "CRA", centros)
  centros <- gsub("C[.]? ?P[.]?", "CP", centros)
  centros <- gsub("C[.]? ?P[.]? ?E[.]? ?B[.]?", "CPEB", centros)
  centros <- gsub("C[.]? ?E[.]? ?P[.]? ?A[.]?", "CEPA", centros)

  return(centros)

}



#' Corrección campo de nivel
#'
#' @param niveles vector de niveles
#'
#' @return vector de enumeraciones de niveles corregidas
corrige_niveles <- function(niveles) {
  return(niveles)
}



#' Corrección campo de tipo
#'
#' @param tipos vector de tipos
#'
#' @return vector de enumeraciones de tipos corregidas
corrige_tipos <- function(tipos) {
  return(tipos)
}



#' Corrección campo de aforo
#'
#' @param aforos vector de aforos
#'
#' @return vector de aforos corregidos
corrige_aforo <- function(aforos) {
  return(aforos)
}



#' Corrección campo de videollamada
#'
#' @param opciones_videollamada vector de opciones de videollamada
#'
#' @return vector de opciones de videollamada corregidas
corrige_videollamada <- function(opciones_videollamada) {
  return(opciones_videollamada)
}



#' Corrección campo de inglés
#'
#' @param opciones_ingles vector de opciones de charla en inglés
#'
#' @return vector de opciones de charla en inglés corregidas
corrige_ingles <- function(opciones_ingles) {
  return(opciones_ingles)
}



#' Corrección campo de comunidad autónoma
#'
#' @param com_autonomas vector de comunidades autónomas
#'
#' @return vector de comunidades autónomas corregidas
corrige_comunidad_autonoma <- function(com_autonomas) {
  return(com_autonomas)
}



#' Corrección campo de provincia
#'
#' @param provincias vector de provincias
#'
#' @return vector de provincias corregidas
corrige_provincia <- function(provincias) {
  return(provincias)
}



#' Corrección campo de localidad
#'
#' @param localidades vector de localidades
#'
#' @return vector de localidades corregidas
corrige_localidad <- function(localidades) {

  localidades <- toupper(localidades)

  # Corrección errores comunes
  localidades <- dplyr::case_when(
    # localidades == "ALICANTE" ~ "ALICANTE/ALACANT",
    # localidades == "ALACANT" ~ "ALICANTE/ALACANT",
    localidades == "ALMERIA" ~ "ALMERÍA",
    localidades == "CACERES" ~ "CÁCERES",
    localidades == "CADIZ" ~ "CÁDIZ",
    localidades == "CASTELLÓ" ~ "CASTELLÓ DE LA PLANA",
    localidades == "CASTELLON" ~ "CASTELLÓN DE LA PLANA",
    localidades == "CASTELLÓN" ~ "CASTELLÓN DE LA PLANA",
    localidades == "CORDOBA" ~ "CÓRDOBA",
    localidades == "CORUÑA" ~ "A CORUÑA",
    localidades == "LAS PALMAS" ~ "LAS PALMAS DE GRAN CANARIA",
    localidades == "LEON" ~ "LEÓN",
    localidades == "MALAGA" ~ "MÁLAGA",
    localidades == "VALENCIA" ~ "VALÈNCIA",
    TRUE ~ localidades
  )

  return(localidades)

}



#' Corrección campo de direccion
#'
#' @param direcciones vector de direcciones
#'
#' @return vector de direccions corregidas
corrige_direccion <- function(direcciones) {
  return(direcciones)
}



#' Corrección campo de código postal
#'
#' @param cod_postales vector de códigos postales
#'
#' @return vector de código postales corregidos
corrige_codigo_postal <- function(cod_postales) {
  stringr::str_pad(cod_postales, width=5, pad="0")
}



#' Corrección campo de web
#'
#' @param webs vector de webs
#'
#' @return vector de webs corregidas
corrige_web <- function(webs) {
  return(webs)
}

#' Corrección campo de teléfono
#'
#' @param telefonos vector de teléfonos
#'
#' @return vector de teléfonos corregidos
corrige_telefono <- function(telefonos) {
  gsub("^\\+34", "", gsub(" ", "", telefonos))
}



#' Corrección campo de comentario
#'
#' @param comentarios vector de comentarios
#'
#' @return vector de comentarios corregidos
corrige_comentario <- function(comentarios) {
  return(comentarios)
}



#' Corrección campo de referencia
#'
#' @param referencias vector de referencias
#'
#' @return vector de referencias corregidas
corrige_referencia <- function(referencias) {
  stringr::str_pad(referencias, width=10, pad=0, side="left")
}



#' Corrección tipo de charla concertada
#'
#' @param tipos vector de tipos de charla
#'
#' @return vector de tipos de charla corregidos
corrige_tipo_charla <- function(tipos) {
  return(tipos)
}



#' Corrección número entero
#'
#' @param tipos vector de números enteros
#'
#' @return vector de números corregido
corrige_numero <- function(numeros) {
  return(numeros)
}



#' Corrección contestación Sí/No
#'
#' @param tipos vector de contestaciones
#'
#' @return vector contestaciones corregidas
corrige_si_no <- function(si_no) {
  gsub("Si", "Sí", stringr::str_to_title(si_no))
}


#' Corrección campo link de imagen
#'
#' @param imagenes vector de links a drive
#'
#' @return vector de imagenes corregidas
corrige_imagen <- function(imagenes) {
  gsub("open?id=", "uc?export=view&id=", imagenes, fixed=TRUE)
}


