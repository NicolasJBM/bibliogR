#' @name get_references
#' @title Load specified references files.
#' @author Nicolas Mangin
#' @description Function applying the proper importation function and pre-cleaning the references to import.
#' @param name Character. Name of the original file.
#' @param path Character. Path to the temporary file where the references to import are.
#' @return References from one file.
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect
#' @export



get_references <- function(name, path){
  
  title <- NULL
  
  if (stringr::str_detect(name, "xlsx$")){
    newref <- readxl::read_excel(path, col_types = "text")
  } else {
    newref <- readr::read_csv(path, col_types = "c")
  }
  expected_columns <- c(
    "bibtype",
    "author", "title", "journal", "year", "month", "volume", "number", "pages", "publisher",
    "booktitle", "editor", "institution", "school", "address", "edition", "note",
    "doi", "url", "abstract", "keywords", "isbn"
  )
  keep <- base::intersect(base::names(newref), expected_columns)
  newref[, keep] |>
    dplyr::mutate_all(base::as.character) |>
    dplyr::filter(title != "", !base::is.na(title))
}
