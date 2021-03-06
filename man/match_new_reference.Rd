% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_new_reference.R
\name{match_new_reference}
\alias{match_new_reference}
\title{Identify Potential Matches in References}
\usage{
match_new_reference(
  tmpkey,
  journal,
  year,
  title,
  author,
  method = "osa",
  references
)
}
\arguments{
\item{tmpkey}{Character string. Temporary key.}

\item{journal}{Character string. Journal of the document to be matched.}

\item{year}{Integer. Year of the document to be matched.}

\item{title}{Character string. Title of the document to be matched.}

\item{author}{Character string. Author of the document to be matched.}

\item{method}{Character string. Method for stringdist.}

\item{references}{Tibble. Initial database of references.}
}
\value{
A match between keys.
}
\description{
Determine whether a reference to be added is already in
the initial list of references
}
\author{
Nicolas Mangin
}
