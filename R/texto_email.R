

#' Genera texto a mostrar en el email con información de la persona que quiere contactar
#'
#' @param info información de la petición de contacto (lista o dataframe)
#'
#' @return texto con formato listo para imprimir
#' @export
texto_email_contacto <- function(info) {

  paste0(
    "\n\n",
    "Nombre: ", info$nombre, "\n\n",
    "Email: ", info$email, "\n\n",
    ifelse(is.na(info$telefono), "", paste0("Teléfono: ", info$telefono, "\n\n")),
    "Mensaje:\n\n", info$mensaje, "\n\n"
  )

}



#' Genera texto a mostrar en el email de fallo con los datos introducidos del formulario
#'
#' @param info información de la petición de contacto (lista o dataframe)
#'
#' @return texto con formato listo para imprimir
#' @export
texto_email_fallo <- function(info) {

  paste0(
    "\n\n",
    "Referencia: ", info$referencia_centro, "\n\n",
    "Centro: ", info$centro, "\n\n",
    "Tu nombre: ", info$nombre, "\n\n",
    "Tu email: ", info$email, "\n\n",
    ifelse(is.na(info$telefono), "", paste0("Tu teléfono: ", info$telefono, "\n\n")),
    "Tu mensaje:\n\n", info$mensaje, "\n\n"
  )

}


