#' @name clean_string
#' @title Clean and simplify strings
#' @author Nicolas Mangin
#' @description Format authors' names so that they fit with bibtex specifications.
#' @param x  Character. String which should be cleaned
#' @param simplify Logical. Whether only letters should be kept.
#' @return A character string with only letters
#' @importFrom lexR clean_tags
#' @importFrom lexR clean_spaces
#' @importFrom lexR clean_ascii
#' @importFrom lexR clean_letters
#' @importFrom stringr str_remove_all
#' @importFrom stringr fixed
#' @importFrom dplyr %>%
#' @export


clean_string <- function(x, simplify = FALSE) {
  x %>%
    unlist() %>%
    as.character() %>%
    lexR::clean_tags() %>%
    lexR::clean_spaces() %>%
    lexR::clean_ascii() %>%
    stringr::str_remove_all(stringr::fixed("\\")) %>%
    stringr::str_remove_all(stringr::fixed("{")) %>%
    stringr::str_remove_all(stringr::fixed("}"))
  if (simplify == TRUE) x <- lexR::clean_letters(x)
  return(x)
}
