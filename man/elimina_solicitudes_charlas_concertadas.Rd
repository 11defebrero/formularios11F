% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procesamiento_solicitudes.R
\name{elimina_solicitudes_charlas_concertadas}
\alias{elimina_solicitudes_charlas_concertadas}
\title{Elimina las solicitudes de charlas que ya han sido concertadas}
\usage{
elimina_solicitudes_charlas_concertadas(dataset, concertadas)
}
\arguments{
\item{dataset}{datos de solicitudes de charlas}

\item{concertadas}{dataset de charlas concertadas}
}
\value{
dataset de solicitudes de charlas restantes (aún no concertadas)
}
\description{
Elimina las solicitudes de charlas que ya han sido concertadas
}
