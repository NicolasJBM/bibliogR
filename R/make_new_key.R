#' @name make_new_key
#' @title Make a new bib key
#' @author Nicolas Mangin
#' @description Function creating a new key for a reference.
#' @param name Character. Author, institution, or school
#' @param year Year of publication.
#' @param keys Character vector. List of already used keys.
#' @return A new bib key.
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @export


make_new_key <- function(name, year, keys) {
  newkeys <- name |>
    stringr::str_remove(pattern = ",(.+)$") |>
    stringr::str_remove(pattern = "-") |>
    base::paste0(year) |>
    stringr::str_remove_all("[[:punct:]]") |>
    stringr::str_remove_all("['-]") |>
    stringr::str_remove_all(" ")
  newkeys <- base::paste0(
    newkeys,
    increments <- c(
      "",
      letters,
      base::unlist(
        base::lapply(
          utils::combn(
            letters,
            2,
            simplify = FALSE
          ),
          paste0,
          collapse = ""
        )
      )
    )
  )
  addkey <- base::setdiff(newkeys, keys)[[1]]
  return(addkey)
}
