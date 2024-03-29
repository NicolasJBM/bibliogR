% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_reference_form.R
\name{make_reference_form}
\alias{make_reference_form}
\title{Create a reference edition form}
\usage{
make_reference_form(id, refkey, type, references)
}
\arguments{
\item{id}{Character. ID of the module to connect the user interface to the appropriate server side.}

\item{refkey}{Character. Bib key of the reference to edit.}

\item{type}{Character. Bib type of the reference to edit.}

\item{references}{Tibble. List of references.}
}
\value{
A form with all the fields which should or can be filled for each type.
}
\description{
Function creating a shiny form allowing the user to edit old or add new references in the main reference database.
}
\author{
Nicolas Mangin
}
