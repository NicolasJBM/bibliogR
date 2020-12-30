#' @name gen_bib
#' @title Generate the Reference File
#' @author Nicolas Mangin
#' @description Create the .bib reference file of the markdown document in which the function is embedded and add it in the working directory. Place this function in the setup of the markdown document.
#' @param wdir         Character string. Path to the desired working directory.
#' @param keys         Character vector. List of bibtex keys to export as a bibtex database.
#' @param journal_list Logical. Whether a list of journals with the number of citations should be produced.
#' @param author_list  Logical. Whether a list of authors with the number of citations should be produced.
#' @return A bib file in the same folder as the Rmarkdown document.
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble remove_rownames
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr sample_n
#' @importFrom dplyr mutate_all
#' @importFrom tidyr unnest
#' @importFrom furrr future_map
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @importFrom stats na.omit
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom utils installed.packages
#' @importFrom RefManageR ReadBib
#' @importFrom RefManageR WriteBib
#' @importFrom RefManageR as.BibEntry
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

  # Obtain references
  load(paste0(find.package("bibliogR"), "/references.RData"))

  # Gather citations
  if (!is.null(wdir)) setwd(wdir)

  if (is.null(keys)) {
    files <- list.files(getwd())
    rmdfiles <- tibble::tibble(
      rmdfiles = files[stringr::str_detect(files, ".Rmd$")]
    )

    if (nrow(rmdfiles) > 0) {
      content <- rmdfiles %>%
        dplyr::mutate(text = furrr::future_map(rmdfiles, read_file)) %>%
        tidyr::unnest()
      content <- paste(as.character(unlist(content$text)), collaspe = " ")
    }

    selection <- content %>%
      str_extract_all("@\\w+") %>%
      unlist() %>%
      str_remove_all("@") %>%
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
      mutate_all(str_replace_all, pattern = "&", replacement = "\\\\&") %>%
      mutate(
        title = paste0("{", title, "}"),
        abstract = paste0("{", abstract, "}")
      ) %>%
      unique() %>%
      group_by(key) %>%
      sample_n(1) %>%
      ungroup() %>%
      as.data.frame()

    if (journal_list) {
      journal_rank <- bib$journal %>%
        table() %>%
        as.data.frame() %>%
        arrange(-Freq)
      write.csv(journal_rank, "journal_rank.csv", row.names = FALSE)
    }

    if (author_list) {
      author_rank <- bib$author %>%
        stringr::str_extract_all("^(.*?), | and(.*?),") %>%
        unlist() %>%
        stringr::str_remove_all(" and ") %>%
        str_remove_all(",") %>%
        trimws() %>%
        table() %>%
        as.data.frame() %>%
        arrange(-Freq)
      write.csv(author_rank, "author_rank.csv", row.names = FALSE)
    }

    bib <- bib %>%
      split(f = bib$key) %>%
      as.BibEntry() %>%
      WriteBib(file = "ref.bib", append = FALSE)
  }
}
