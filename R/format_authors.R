#' @name format_authors
#' @title Format Author Names
#' @author Nicolas Mangin
#' @description Format authors' names so that they fit with bibtex specifications.
#' @param author  Character. String with names of authors. Authors are separated by " and ".
#' @return A character string with all the author names properly formated.
#' @importFrom stringi stri_trans_totitle
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove_all
#' @importFrom dplyr %>%
#' @export


format_authors <- function(author) {
  author <- paste0(" ", lexR::clean_ascii(author))
  author <- stringi::stri_trans_totitle(author) %>%
    stringr::str_replace_all(" Van Den ", " van_den_") %>%
    stringr::str_replace_all(" Van Der ", " van_der_") %>%
    stringr::str_replace_all(" Van De ", " van_de_") %>%
    stringr::str_replace_all(" Van ", " van_") %>%
    stringr::str_replace_all(" Von ", " von_") %>%
    stringr::str_replace_all(" De ", " de_") %>%
    stringr::str_replace_all(" Dos ", " dos_") %>%
    stringr::str_replace_all(" Jr\\. ", "") %>%
    stringr::str_replace_all(" Sr\\. ", "") %>%
    stringr::str_replace_all(" Ii", "") %>%
    stringr::str_replace_all(" Iii", "") %>%
    trimws()
  author <- stringr::str_split(author, pattern = " And ")
  author <- lapply(author, stringr::str_split, pattern = " ")
  author <- author[[1]]
  for (i in seq_len(length(author))) {
    if (stringr::str_detect(author[[i]][1], ",")) {
      first <- stringr::str_replace(author[[i]][1], ",", "")
      last <- author[[i]][-1]
    } else {
      first <- author[[i]][length(author[[i]])]
      last <- author[[i]][-length(author[[i]])]
    }
    author[[i]] <- paste0(first, ", ", paste(last, collapse = " "))
  }
  author <- paste(unlist(author), collapse = " and ") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_remove_all("^.\\., ")

  return(author)
}
