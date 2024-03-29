% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_bib_file.R
\name{make_bib_file}
\alias{make_bib_file}
\title{Write a bib file}
\usage{
make_bib_file(
  source_folders = NULL,
  references = NULL,
  destination_folder = NULL,
  file_name = "references.bib"
)
}
\arguments{
\item{source_folders}{Character. Paths to the folders where the .Rmd files are.}

\item{references}{Tibble. List of references.}

\item{destination_folder}{Character. Path to the folder where the .bib file should be written.}

\item{file_name}{Character. Name of the .bib file.}
}
\value{
Write a .bib files with all the references quoted in the .Rmd files
}
\description{
Function listing references cited in rmarkdown or quarto document and creating the corresponding .bibtex reference file.
}
\author{
Nicolas Mangin
}
