

FUNCIONES_PROCESAMIENTO_CAMPOS <- list(
  "aforo" = list(
    "fun_corrige" = function(...) corrige_aforo(...),
    "fun_valida" = function(...) es_valido_aforo(...)
  ),
  "centro" = list(
    "fun_corrige" = function(...) corrige_centro(...),
    "fun_valida" = function(...) es_valido_centro(...)
  ),
  "codpostal" = list(
    "fun_corrige" = function(...) corrige_codigo_postal(...),
    "fun_valida" = function(...) es_valido_codigo_postal(...)
  ),
  "com_autonoma" = list(
    "fun_corrige" = function(...) corrige_comunidad_autonoma(...),
    "fun_valida" = function(...) es_valido_comunidad_autonoma(...)
  ),
  "comentario" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "direccion" = list(
    "fun_corrige" = function(...) corrige_direccion(...),
    "fun_valida" = function(...) es_valido_direccion(...)
  ),
  "email" = list(
    "fun_corrige" = function(...) corrige_email(...),
    "fun_valida" = function(...) es_valido_email(...)
  ),
  "email_centro" = list(
    "fun_corrige" = function(...) corrige_email(...),
    "fun_valida" = function(...) es_valido_email(...)
  ),
  "email_ponente" = list(
    "fun_corrige" = function(...) corrige_email(...),
    "fun_valida" = function(...) es_valido_email(...)
  ),
  "es_charla_solicitada" = list(
    "fun_corrige" = function(...) corrige_si_no(...),
    "fun_valida" = function(...) es_valido_si_no(...)
  ),
  "ingles" = list(
    "fun_corrige" = function(...) corrige_ingles(...),
    "fun_valida" = function(...) es_valido_ingles(...)
  ),
  "institucion_ponente" = list(
    "fun_corrige" = function(...) corrige_centro(...),
    "fun_valida" = function(...) es_valido_centro(...)
  ),
  "localidad" = list(
    "fun_corrige" = function(...) corrige_localidad(...),
    "fun_valida" = function(...) es_valido_localidad(...)
  ),
  "mensaje" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "n_alumnos" = list(
    "fun_corrige" = function(...) corrige_numero(...),
    "fun_valida" = function(...) es_valido_numero(...)
  ),
  "n_charlas" = list(
    "fun_corrige" = function(...) corrige_numero(...),
    "fun_valida" = function(...) es_valido_numero(...)
  ),
  "niveles" = list(
    "fun_corrige" = function(...) corrige_niveles(...),
    "fun_valida" = function(...) es_valido_niveles(...)
  ),
  "nombre" = list(
    "fun_corrige" = function(...) corrige_nombre(...),
    "fun_valida" = function(...) es_valido_nombre(...)
  ),
  "ponente" = list(
    "fun_corrige" = function(...) corrige_nombre(...),
    "fun_valida" = function(...) es_valido_nombre(...)
  ),
  "provincia" = list(
    "fun_corrige" = function(...) corrige_provincia(...),
    "fun_valida" = function(...) es_valido_provincia(...)
  ),
  "referencia_centro" = list(
    "fun_corrige" = function(...) corrige_referencia(...),
    "fun_valida" = function(...) es_valida_referencia(...)
  ),
  "telefono" = list(
    "fun_corrige" = function(...) corrige_telefono(...),
    "fun_valida" = function(...) es_valido_telefono(...)
  ),
  "tipo_charla" = list(
    "fun_corrige" = function(...) corrige_tipo_charla(...),
    "fun_valida" = function(...) es_valido_tipo_charla(...)
  ),
  "tipos" = list(
    "fun_corrige" = function(...) corrige_tipos(...),
    "fun_valida" = function(...) es_valido_tipos(...)
  ),
  "titulo_charla" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "unidad_divulgacion" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "videollamada" = list(
    "fun_corrige" = function(...) corrige_videollamada(...),
    "fun_valida" = function(...) es_valido_videollamada(...)
  ),
  "herramientas_online" = list(
    "fun_corrige" = function(...) corrige_herramientas_online(...),
    "fun_valida" = function(...) es_valido_herramientas_online(...)
  ),
  "web" = list(
    "fun_corrige" = function(...) corrige_web(...),
    "fun_valida" = function(...) es_valido_web(...)
  ),

  # actividades - solo los campos que quiera validar

  "es_centro" = list(
    "fun_corrige" = function(...) corrige_si_no(...),
    "fun_valida" = function(...) es_valido_si_no(...)
  ),
  "es_presencial" = list(
    "fun_corrige" = function(...) corrige_si_no(...),
    "fun_valida" = function(...) es_valido_si_no(...)
  ),
  "titulo" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "des" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "email2" = list(
    "fun_corrige" = function(...) corrige_email(...),
    "fun_valida" = function(...) es_valido_email(...)
  ),
  "audiencia" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "organiza" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "patrocina" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "espacio" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "reserva" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "telf" = list(
    "fun_corrige" = function(...) corrige_telefono(...),
    "fun_valida" = function(...) es_valido_telefono(...)
  ),
  "tipo" = list(
    "fun_corrige" = function(...) corrige_comentario(...),
    "fun_valida" = function(...) es_valido_comentario(...)
  ),
  "imagen" = list(
    "fun_corrige" = function(...) corrige_imagen(...),
    "fun_valida" = function(...) es_valido_imagen(...)
  )
)



