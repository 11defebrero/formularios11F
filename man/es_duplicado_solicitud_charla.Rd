% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procesamiento_solicitudes.R
\name{es_duplicado_solicitud_charla}
\alias{es_duplicado_solicitud_charla}
\title{Indica las solicitudes de charla que son duplicados,
manteniendo la última como la correcta}
\usage{
es_duplicado_solicitud_charla(dataset, campos_combrobacion)
}
\arguments{
\item{dataset}{datos de solicitudes de charlas}
}
\value{
vector lógico indicando si cada solicitud es un duplicado o no
}
\description{
Indica las solicitudes de charla que son duplicados,
manteniendo la última como la correcta
}
