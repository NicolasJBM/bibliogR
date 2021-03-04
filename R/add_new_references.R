#' @name add_new_references
#' @title Append new references
#' @author Nicolas Mangin
#' @description Add imported references with unique keys to the initial list of references.
#' @param complement dataframe. References to be added
#' @param references dataframe. References to which the references are to be added
#' @return A dataframe of references
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate_all
#' @importFrom utils combn
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar


add_new_references <- function(complement, references) {
  complement <- dplyr::mutate_all(complement, as.character)
  references <- dplyr::mutate_all(references, as.character)

  make_key <- function(name, year, keys) {
    newkeys <- name %>%
      stringr::str_remove(pattern = ",(.+)$") %>%
      stringr::str_remove(pattern = "-") %>%
      paste0(year) %>%
      stringr::str_remove_all("[[:punct:]]") %>%
      stringr::str_remove_all("['-]") %>%
      stringr::str_remove_all(" ")
    newkeys <- paste0(
      newkeys,
      increments <- c(
        "",
        letters,
        unlist(
          lapply(
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
    addkey <- setdiff(newkeys, keys)[[1]]
    return(addkey)
  }

  complement$key <- ""
  complement$order <- ""
  addition <- nrow(complement)
  progress_bar <- utils::txtProgressBar(min = 0, max = addition, style = 3, char = "=")

  for (i in 1:addition) {
    utils::setTxtProgressBar(progress_bar, value = i)
    keys <- references$key
    addref <- complement[i, ]
    addref$key <- make_key(addref$author, addref$year, keys)
    references <- dplyr::bind_rows(references, addref)
  }

  references$order <- seq_len(nrow(references))
  close(progress_bar)
  return(references)
}
