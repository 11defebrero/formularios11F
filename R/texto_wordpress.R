#' Genera el cuerpo del mail que se envía a wordpress
#'
#' @param info información de la actividad (lista)
#'
#' @return cuerpo del post
#' @export
cuerpo_mail_actividad <- function(info) {

  imagen <- imagen_actividad(info)
  cabecera <- cabecera_actividad(info)
  texto <- texto_actividad(info)
  category <- paste0("[category actividades ", config$edicion, "]")
  tags2 <- paste0("[tags ", toString(paste0(tags)), "]")
  status <- "[status pending]"
  fin <- "[end]"

  return(paste("<p>", imagen,"</p>",
               "<p>", cabecera, "</p>",
               "<p>", texto, "</p>",
               "<p>", category, "</p>",
               "<p>", tags2, "</p>",
               "<p>", status, "</p>",
               "<br>", fin)
  )

}


#' Genera la cabecera del post de wordpress de la actividad
#'
#' @param info información de la actividad (lista)
#'
#' @return cabecera del post
#' @export
cabecera_actividad <- function(info) {

  info$localidad <- stringr::str_to_title(info$localidad)

  paste0(
    "<b>",
    info$tipo, ".",
    ifelse(info$clase_actividad == "PRESENCIAL", paste0(
      " ", info$espacio, ", ",
      ifelse(info$localidad != info$provincia, paste0(info$localidad, ", "), ""),
      info$provincia, "."
    ), ""),
    ifelse(!is.na(info$fecha), paste0(" ", info$fecha, " de febrero."), ""),
    "</b>"
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

  gsub("\n", "<br>",
    paste0(
    info$des, "<br> <br> <br>",
    "<b>Dirigido a</b>: ", info$audiencia, "<br>",
    ifelse(!is.na(info$hora_inicio), paste0("<b>Hora comienzo</b>: ", info$hora_inicio, "<br>"), ""),
    ifelse(!is.na(info$hora_inicio), paste0("<b>Hora finalización</b>: ", info$hora_fin, "<br>"), ""),
    ifelse(info$clase_actividad == "PRESENCIAL", paste0(
      "<b>Dirección</b>: ", paste0(
        info$direccion, ", ", info$codpostal, ", ",
        ifelse(info$localidad != info$provincia, paste0(info$localidad, ", "), ""),
        info$provincia, ", ", info$com_autonoma
      ), "<br>",
      "<b>Reserva</b>: ", info$reserva, "<br>"
    ), ""),
    "<b>Organiza</b>: ", info$organiza, "<br>",
    ifelse(!is.na(info$patrocina), paste0("<b>Patrocina</b>: ", info$patrocina, "<br>"), ""),
    "<b>Email de contacto</b>: ", info$email2, "<br>",
    ifelse(!is.na(info$telefono), paste0("<b>Teléfono de contacto</b>: ", info$telefono, "<br>"), ""),
    ifelse(!is.na(info$web), paste0("<b>Más información</b>: ", info$web, "<br>"), "")
  )
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

imagen_actividad <- function(info){

  paste0("<img src=\"",
         info$imagen,
         "\" alt=\"Imagen de la actividad con título ",
         info$titulo,
         "\" align=\"center\" width=\"80%\" scale=\"0\">")

  #   <img src="images/dinosaur.jpg"
  # alt="La cabeza y el torso de un esqueleto de dinosaurio;
  #          tiene una cabeza grande con dientes largos y afilados">

}

