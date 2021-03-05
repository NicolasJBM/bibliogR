#' @name filter_references
#' @title Filter the reference based on input
#' @author Nicolas Mangin
#' @description
#' Filter a dataset based on a filter value given by the user for a variable
#' according to the type of filter.
#' @param dataset    Dataframe of tibble. References to filter.
#' @param variable     Character string. Variable used for filtering.
#' @param filter_value
#' Regex pattern, character string, numeric vector,
#' or numeric value. Dependent on the the filter type
#' @param filter_type
#'  Character string. Specify whether the filter is a regex "pattern",
#'  a "selection" string, a numeric "range" or a numeric "value".
#' @return A properly formatted string og references
#' @importFrom dplyr filter
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect


filter_references <- function(dataset,
                              variable,
                              filter_value = NULL,
                              filter_type = "pattern") {
  stopifnot(
    filter_type %in% c("selection", "pattern", "range", "value")
  )

  if (filter_value[1] == "" | nrow(dataset) == 0) {
    dataset
  } else {
    if (filter_type == "selection") {
      dplyr::filter(
        dataset,
        dataset[, variable] == filter_value
      )
    } else if (filter_type == "pattern") {
      terms <- stringr::str_to_lower(
        unlist(
          stringr::str_split(
            filter_value,
            " "
          )
        )
      )
      terms <- stringr::str_replace_all(terms, "_", " ")
      base <- dataset
      for (term in terms) {
        base <- dplyr::filter(
          base,
          stringr::str_detect(
            stringr::str_to_lower(
              unlist(
                base[, variable]
              )
            ),
            term
          )
        )
      }
      base
    } else if (filter_type == "range") {
      dplyr::filter(
        dataset,
        dataset[, variable] >= filter_value[1],
        dataset[, variable] <= filter_value[2]
      )
    } else {
      dplyr::filter(
        dataset,
        dataset[, variable] == filter_value[1]
      )
    }
  }
}
