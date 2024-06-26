% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_ebsco_import.R
\name{prepare_ebsco_import}
\alias{prepare_ebsco_import}
\title{Prepare EBSCO files}
\usage{
prepare_ebsco_import(files, type = "article")
}
\arguments{
\item{files}{Character vector. List of paths to the csv files to be imported.}

\item{type}{Bibtype to be imported. Defaut to "article".}
}
\value{
A clean list of references which can be imported with bibliogR interface.
}
\description{
Format csv exports from EBSCO to be imported in references.
}
\author{
Nicolas Mangin
}
