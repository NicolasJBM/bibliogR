#' @name gen_bib
#' @title Generate the Reference File
#' @author Nicolas Mangin
#' @description
#' Create the .bib reference file of the markdown document in which
#' the function is embedded and add it in the working directory.
#' Place this function in the setup of the markdown document.
#' @param wdir         Character string. Path to the desired working directory.
#' @param keys
#' Character vector. List of bibtex keys to export as a bibtex database.
#' @param journal_list
#' Logical. Whether a list of journals with the number of citations
#' should be produced.
#' @param author_list
#'  Logical. Whether a list of authors with the number of citations
#'  should be produced.
#' @return A bib file in the same folder as the Rmarkdown document.
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate
#' @importFrom furrr future_map
#' @importFrom tidyr unnest
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove_all
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_all
#' @importFrom stringr str_replace_all
#' @importFrom dplyr group_by
#' @importFrom dplyr sample_n
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom utils write.csv
#' @importFrom tibble column_to_rownames
#' @importFrom RefManageR as.BibEntry
#' @importFrom RefManageR WriteBib
#' @importFrom readr read_file
#' @export


gen_bib <- function(wdir = NULL,
                    keys = NULL,
                    journal_list = FALSE,
                    author_list = FALSE) {

  # Bind variables for dplyr
  key <- NULL
  title <- NULL
  abstract <- NULL
  Freq <- NULL
  references <- NULL
  text <- NULL

  # Obtain references
  load(paste0(find.package("bibliogR"), "/references.RData"))

  # Gather citations
  if (!is.null(wdir)) setwd(wdir)
  if (!dir.exists("dat")) dir.create("dat")

  if (is.null(keys)) {
    files <- list.files(getwd())
    rmdfiles <- tibble::tibble(
      rmdfiles = files[stringr::str_detect(files, ".Rmd$")]
    )

    if (nrow(rmdfiles) > 0) {
      content <- rmdfiles %>%
        dplyr::mutate(text = furrr::future_map(rmdfiles, readr::read_file)) %>%
        tidyr::unnest(text)
      content <- paste(as.character(unlist(content$text)), collaspe = " ")
    }

    selection <- content %>%
      stringr::str_extract_all("@\\w+") %>%
      unlist() %>%
      stringr::str_remove_all("@") %>%
      unique()
  } else {
    selection <- keys
  }


  # Create bib file and csv files about journals and authors
  if (length(selection) > 0 & nrow(references) > 0) {
    basebib <- references %>%
      dplyr::filter(key %in% selection) %>%
      as.data.frame()

    bib <- basebib %>%
      dplyr::mutate_all(
        stringr::str_replace_all,
        pattern = "&",
        replacement = "\\\\&"
      ) %>%
      dplyr::mutate(
        title = paste0("{", title, "}"),
        abstract = paste0("{", abstract, "}")
      ) %>%
      unique() %>%
      dplyr::group_by(key) %>%
      dplyr::sample_n(1) %>%
      dplyr::ungroup() %>%
      as.data.frame()
    
    refnbr <- nrow(bib)

    if (journal_list) {
      journal_rank <- bib$journal %>%
        table() %>%
        as.data.frame() %>%
        dplyr::arrange(-Freq)
      utils::write.csv(journal_rank, "journal_rank.csv", row.names = FALSE)
    }

    if (author_list) {
      author_rank <- bib$author %>%
        stringr::str_extract_all("^(.*?), | and(.*?),") %>%
        unlist() %>%
        stringr::str_remove_all(" and ") %>%
        stringr::str_remove_all(",") %>%
        trimws() %>%
        table() %>%
        as.data.frame() %>%
        dplyr::arrange(-Freq)
      utils::write.csv(author_rank, "author_rank.csv", row.names = FALSE)
    }

    bib <- bib %>%
      tibble::column_to_rownames("key") %>%
      RefManageR::as.BibEntry() %>%
      RefManageR::WriteBib(file = "dat/ref.bib", append = FALSE)
  } else refnbr <- 0
  
  return(refnbr)
}
