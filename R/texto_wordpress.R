
#' Genera la cabecera del post de wordpress de la actividad
#'
#' @param info información de la actividad (lista)
#'
#' @return cabecera del post
#' @export
cabecera_actividad <- function(info) {

  info$localidad <- stringr::str_to_title(info$localidad)

  paste0(
    "**",
    info$tipo, ".",
    ifelse(info$clase_actividad == "PRESENCIAL", paste0(
      " ", info$espacio, ", ",
      ifelse(info$localidad != info$provincia, paste0(info$localidad, ", "), ""),
      info$provincia, "."
    ), ""),
    ifelse(!is.na(info$fecha), paste0(" ", info$fecha, " de febrero."), ""),
    "**"
  )

}


#' Genera el texto del post de wordpress de la actividad
#'
#' @param info información de la actividad (lista o dataframe)
#'
#' @return texto del post
#' @export
texto_actividad <- function(info) {

  info$localidad <- stringr::str_to_title(info$localidad)

  paste0(
    info$des, "\n\n\n",
    "**Dirigido a**: ", info$audiencia, "\n",
    ifelse(!is.na(info$hora_inicio), paste0("**Hora comienzo**: ", info$hora_inicio, "\n"), ""),
    ifelse(!is.na(info$hora_inicio), paste0("**Hora finalización**: ", info$hora_fin, "\n"), ""),
    ifelse(info$clase_actividad == "PRESENCIAL", paste0(
      "**Dirección**: ", paste0(
        info$direccion, ", ", info$codpostal, ", ",
        ifelse(info$localidad != info$provincia, paste0(info$localidad, ", "), ""),
        info$provincia, ", ", info$com_autonoma
      ), "\n",
      "**Reserva**: ", info$reserva, "\n"
    ), ""),
    "**Organiza**: ", info$organiza, "\n",
    ifelse(!is.na(info$patrocina), paste0("**Patrocina**: ", info$patrocina, "\n"), ""),
    "**Email de contacto**: ", info$email2, "\n",
    ifelse(!is.na(info$telf), paste0("**Teléfono de contacto**: ", info$telf, "\n"), ""),
    ifelse(!is.na(info$web), paste0("**Más información**: ", info$web, "\n"), "")
  )

}


#' Genera tags para etiquetar el post de wordpress de la actividad
#'
#' @param info información de la actividad (lista)
#' @param edicion año de la edición actual
#'
#' @return vector con las tags
#' @export
tags_actividad <- function(info, edicion) {

  tags_fecha <- if (!is.na(info$fecha)) {
    paste0(strsplit(info$fecha, ", ")[[1]], "F", edicion)
  }

  tags_lugar <- if(info$clase_actividad == "PRESENCIAL") {
    c(info$provincia, info$com_autonoma)
  }

  tags_reserva <- if(info$clase_actividad == "PRESENCIAL" & !is.na(info$reserva)) {
    if (info$reserva == "Entrada libre") {
      "entrada libre"
    } else if (info$reserva == "Reserva posible pero no necesaria") {
      "reserva posible"
    } else if (info$reserva == "Requiere reserva") {
      "con reserva"
    }
  }

  tags_publico <- if (grepl("público", info$audiencia)) {
    "abierto al público"
  }

  tags_edad <- if (grepl("infantil", info$audiencia)) {
    c("infantil", "familiar")
  } else if (grepl("familiar", info$audiencia)) {
    "familiar"
  }

  tags_tipo <- strsplit(info$tipo, ", ")[[1]]

  tags <- c(
    tags_tipo, tags_lugar,
    tags_fecha, edicion,
    tags_reserva, tags_publico, tags_edad
  )

  return(tags)
}

