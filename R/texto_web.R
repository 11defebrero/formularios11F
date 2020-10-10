
#' Genera link en HTML
#'
#' @param texto texto a mostrar
#' @param url dirección a la que enlaza
#' @param name indica si crear una etiqueta 'name' o 'href'
#' @param new_tab indica si abrir el link en una nueva pestaña
#'
#' @return código HTML para el link
#' @export
html_link <- function(texto, url, name=FALSE, new_tab=FALSE) {
  paste0("<a ",
         if (name) { "name" } else { "href" }, "='", url, "' ",
         "target='", if (new_tab) { "_blank" } else { "_self" }, "'>",
         texto, "</a>")
}



#' Genera link a referencia
#'
#' @param id número de referencia
#' @param texto texto a mostrar
#' @param base_url url de la página de referencias
#'
#' @return código HTML para el link
#' @export
link_referencia <- function(id, texto, base_url) {
  html_link(texto, paste0(base_url, "#referencia-", id), name=FALSE)
}



#' Crea nueva sección para referencia en la página
#'
#' @param id número de referencia
#' @param texto texto a mostrar
#'
#' @return código HTML para el link
seccion_referencia <- function(id, texto) {
  html_link(texto, paste0("referencia-", id), name=TRUE)
}



#' Genera texto a mostrar en la web con información de la solicitud
#'
#' @param info información de la solicitud (lista o dataframe)
#' @param base_url url de la página de referencias
#'
#' @return texto con formato listo para imprimir
#' @export
texto_solicitud_charla <- function(info) {

  paste0(
    "\n\n",
    "##### **Referencia**: ", seccion_referencia(info$id, texto=info$id), "\n\n",
    "**Centro**: ", ifelse(is.na(info$web), info$centro, html_link(info$centro, info$web, new_tab=TRUE)), "\n\n",
    "**Comunidad Autónoma**: ", info$com_autonoma, "\n\n",
    "**Provincia**: ", info$provincia, "\n\n",
    "**Localidad**: ", info$codpostal, ", ", info$localidad, "\n\n",
    "**Dirección**: ", info$direccion, "\n\n",
    "**Niveles solicitados**: ", info$niveles, "\n\n",
    "**Tipos de charla solicitados**: ", info$tipos, "\n\n",
    "**Aforo**: ", info$aforo, "\n\n",
    "**Puede ser por videollamada**: ", info$videollamada, "\n\n",
    "**Puede ser en inglés**: ", info$ingles, "\n\n",
    ifelse(is.na(info$comentario), "",
           paste0("**Comentarios**: ", info$comentario, "\n\n"))
  )

}


#' Genera popup a mostrar en el mapa de la web con información de la solicitud
#'
#' @param info información de la solicitud (lista o dataframe)
#' @param base_url url de la página de referencias
#'
#' @return popup con formato listo para mostrar en el mapa
#' @export
popup_solicitud_charla <- function(info, base_url) {

  paste0(
    link_referencia(info$id, texto=info$centro, base_url=base_url), "<br/>",
    info$direccion, ", ", info$codpostal, ", ", info$localidad
  )

}


#' Genera texto a mostrar en la web con información de la charla concertada
#'
#' @param info información de la charla concertada (lista o dataframe)
#' @param base_url url de la página de referencias
#'
#' @return texto con formato listo para imprimir
#' @export
texto_charla_concertada <- function(info) {

  paste0(
    "\n\n",
    "##### **Referencia**: ", seccion_referencia(info$id, texto=info$id), "\n\n",
    "**Centro**: ", info$centro, "\n\n",
    "**Comunidad Autónoma**: ", info$com_autonoma, "\n\n",
    "**Provincia**: ", info$provincia, "\n\n",
    "**Localidad**: ", info$codpostal, ", ", info$localidad, "\n\n",
    "**Ponente**: ", info$ponente, "\n\n",
    "**Institución**: ", info$institucion_ponente, "\n\n",
    "**Título de la charla**: ", info$titulo_charla, "\n\n",
    "**Nivel**: ", info$niveles, "\n\n",
    "**Tipo de charla**: ", info$tipo_charla, "\n\n",
    "**Número de veces que se impartirá esta charla en el centro**: ", info$n_charlas, "\n\n",
    "**Número aproximado de estudiantes que asistirán**: ", info$n_alumnos, "\n\n",
    "**¿Charla concertada mediante proyecto/unidad de divulgación o igualdad?** ",
    ifelse(is.na(info$unidad_divulgacion), "No", info$unidad_divulgacion), "\n\n"
  )

}


#' Genera popup a mostrar en el mapa de la web con información de la charla concertada
#'
#' @param info información de la charla concertada (lista o dataframe)
#' @param base_url url de la página de referencias
#'
#' @return popup con formato listo para mostrar en el mapa
#' @export
popup_charla_concertada <- function(info, base_url) {

  paste0(
    link_referencia(info$id, texto=info$centro, base_url=base_url), "<br/>",
    info$ponente, "<br/>",
    info$titulo_charla
  )

}


#' Genera texto a mostrar en la web con información de la actividad de Centro11F
#'
#' @param info información de la actividad Centro11F (lista o dataframe)
#' @param base_url url de la página de referencias
#'
#' @return texto con formato listo para imprimir
#' @export
texto_centro11f <- function(info) {

  paste0(
    "\n\n",
    "##### **Referencia**: ", seccion_referencia(info$id, texto=info$id), "\n\n",
    "**Centro**: ", ifelse(is.na(info$web), info$centro, html_link(info$centro, info$web, new_tab=TRUE)), "\n\n",
    "**Comunidad Autónoma**: ", info$com_autonoma, "\n\n",
    "**Provincia**: ", info$provincia, "\n\n",
    "**Localidad**: ", info$codpostal, ", ", info$localidad, "\n\n",
    "**Tipo de actividad**: ", info$tipo, "\n\n",
    "**Título**: ", info$titulo, "\n\n",
    "**Descripción**: ", info$des, "\n\n"
  )

}


#' Genera popup a mostrar en el mapa de la web con información de la actividad de Centro11F
#'
#' @param info información de la charla actividad Centro11F (lista o dataframe)
#' @param base_url url de la página de referencias
#'
#' @return popup con formato listo para mostrar en el mapa
#' @export
popup_centro11f <- function(info, base_url) {

  paste0(
    link_referencia(info$id, texto=info$centro, base_url=base_url), "<br/>",
    info$titulo, "<br/>",
    info$tipo
  )

}


#' Genera popup a mostrar en el mapa de la web con información de la actividad presencial
#'
#' @param info información de la charla actividad presencial (lista o dataframe)
#' @param base_url url de la página de referencias
#'
#' @return popup con formato listo para mostrar en el mapa
#' @export
popup_actividad_presencial <- function(info) {

  paste0(
    html_link(texto=info$titulo, url=info$procesado, new_tab=TRUE), "<br/>",
    info$tipo, "<br/>",
    info$organiza,
    ifelse(is.na(info$fecha), "", paste0("<br/>", info$fecha))
  )

}

