% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procesamiento_datasets.R
\name{limpia_campos}
\alias{limpia_campos}
\title{Limpia campos especificados de un dataset
Aplica las funciones de corrección y validación correspondientes a cada campo
y completa la información de fallos encontrados en los datos.}
\usage{
limpia_campos(dataset, campos, col_fallos)
}
\arguments{
\item{dataset}{datos a limpiar}

\item{campos}{nombres de los campos a limpiar}

\item{col_fallos}{nombre de la columna del dataset que almacena los fallos}
}
\value{
dataset con datos limpios y reporte de fallos
}
\description{
Limpia campos especificados de un dataset
Aplica las funciones de corrección y validación correspondientes a cada campo
y completa la información de fallos encontrados en los datos.
}
