#' @name format_citations
#' @title Format citations for insertion
#' @author Nicolas Mangin
#' @description
#' Bring all citations together in a properly formatted string to be inserted.
#' @param citations Character vector. Keys of the references to be inserted.
#' @param pages     Character string. Pages if specified.
#' @return A properly formatted string og references
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect


format_citations <- function(citations, format, pages) {
  pg <- dplyr::case_when(
    stringr::str_detect(pages, "-") ~ "pp.",
    TRUE ~ "p."
  )
  dplyr::case_when(
    length(citations) == 1 &
      format == "create" &
      pages == "" ~
    paste0("[@", citations[[1]], "]"),
    length(citations) == 1 &
      format == "create" &
      pages != "" ~
    paste0("[@", citations[[1]], ", ", pg, pages, "]"),
    length(citations) >= 2 &
      format == "create" ~
    paste0("[@", paste(citations, collapse = "; @"), "]"),
    length(citations) == 1 &
      format == "add" &
      pages == "" ~
    paste0("; @", citations[[1]]),
    length(citations) == 1 &
      format == "add" &
      pages != "" ~
    paste0("; @", citations[[1]], ", ", pg, pages),
    length(citations) >= 1 &
      format == "add" ~
    paste0("; @", paste(citations, collapse = "; @")),
    TRUE ~ paste0("@", citations[[1]])
  )
}
