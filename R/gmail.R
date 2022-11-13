

#' Autenticación de la cuenta de Gmail
#'
#' @param credentials_path ruta al fichero JSON de credenciales
#'
#' @export
gmail_acceso <- function(credentials_path) {

  gmailr::gm_auth_configure(path=credentials_path)
  gmailr::gm_auth()

}


#' Send email
#'
#' @param to email to
#' @param from email from
#' @param reply_to reply email to
#' @param subject email subject
#' @param body email body
#'
#' @export
gmail_envia_email <- function(to, from, reply_to, subject, body) {

  gmailr::gm_mime(`Reply-To` = reply_to) %>%
    gmailr::gm_to(to) %>%
    gmailr::gm_from(from) %>%
    gmailr::gm_subject(subject) %>%
    gmailr::gm_text_body(body) %>%
    gmailr::gm_send_message()
    # gmailr::gm_create_draft()

#    mime_msg <- strwrap(as.character(html_msg))

}

#' Send email con html
#'
#' @param to email to
#' @param from email from
#' @param reply_to reply email to
#' @param subject email subject
#' @param body email body with html
#'
#' @export
gmail_envia_email_html <- function(to, from, reply_to, subject, body) {

  gmailr::gm_mime(`Reply-To` = reply_to) %>%
    gmailr::gm_to(to) %>%
    gmailr::gm_from(from) %>%
    gmailr::gm_subject(subject) %>%
    gmailr::gm_html_body(body) %>%
    gmailr::gm_send_message()
    # gmailr::gm_create_draft()

  #    mime_msg <- strwrap(as.character(html_msg))

}
