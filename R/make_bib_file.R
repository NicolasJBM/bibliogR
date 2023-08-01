#' @name make_bib_file
#' @title Write a bib file
#' @author Nicolas Mangin
#' @description Function listing references cited in rmarkdown or quarto document and creating the corresponding .bibtex reference file.
#' @param source_folders Character. Paths to the folders where the .Rmd files are.
#' @param references Tibble. List of references.
#' @param destination_folder Character. Path to the folder where the .bib file should be written.
#' @param file_name Character. Name of the .bib file.
#' @return Write a .bib files with all the references quoted in the .Rmd files
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom purrr pmap
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove_all
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export


make_bib_file <- function(
  source_folders = NULL,
  references = NULL,
  destination_folder = NULL,
  file_name = "references.bib"
) {
  
  text <- NULL
  key <- NULL
  bibtype <- NULL
  author <- NULL
  title <- NULL
  journal <- NULL
  year <- NULL
  month <- NULL
  volume <- NULL
  number <- NULL
  pages <- NULL
  publisher <- NULL
  booktitle <- NULL
  editor <- NULL
  institution <- NULL
  school <- NULL
  address <- NULL
  edition <- NULL
  note <- NULL
  doi <- NULL
  url <- NULL
  abstract <- NULL
  keywords <- NULL
  isbn <- NULL
  issn <- NULL
  jnl <- NULL
  field <- NULL
  
  
  #source_folders <- "/Users/nicolas/Dropbox/5-Education/Courses/management_accounting/materials/9_analyses"
  #destination_folder <- "/Users/nicolas/Dropbox/5-Education/Courses/management_accounting/materials/9_analyses/data"
  
  # Gather citations
  if (base::is.null(source_folders)) source_folders <- base::getwd()
  files <- base::character(0)
  for (path in source_folders){
    files <- c(
      files,
      base::list.files(path, full.names = TRUE, recursive = TRUE)
    )
  }
  rmdfiles <- tibble::tibble(
    rmdfiles = files[stringr::str_detect(files, ".Rmd$|.qmd$")]
  )
  
  bibfile <- dplyr::case_when(
    base::is.null(destination_folder) ~ file_name,
    TRUE ~ base::paste0(destination_folder, "/", file_name)
  )
  
  if (base::nrow(rmdfiles) > 0) {
    content <- rmdfiles |>
      dplyr::mutate(text = purrr::map(rmdfiles, base::readLines)) |>
      tidyr::unnest(text)
    content <- base::paste(
      base::as.character(base::unlist(content$text)),
      collaspe = " "
    )
    
    selection <- content |>
      stringr::str_extract_all("@\\w+") |>
      base::unlist() |>
      stringr::str_remove_all("@") |>
      base::unique() |>
      base::as.character()
    
    # Create bib file and csv files about journals and authors
    if (base::length(base::intersect(selection, references$key)) > 0) {
      references |>
        dplyr::filter(key %in% selection) |>
        tibble::as_tibble() |>
        dplyr::mutate_all(
          stringr::str_replace_all,
          pattern = "&",
          replacement = "\\\\&"
        ) |>
        dplyr::mutate(
          title = base::paste0("{", title, "}"),
          abstract = base::paste0("{", abstract, "}")
        ) |>
        base::unique() |>
        dplyr::group_by(key) |>
        dplyr::sample_n(1) |>
        dplyr::ungroup() |>
        tibble::as_tibble() |>
        dplyr::select(
          bibtype, key, 
          author, title, journal, year, month, volume, number, pages, publisher,
          booktitle, editor, institution, school, address, edition, note,
          doi, url, abstract, keywords, isbn, issn
        ) |>
        dplyr::mutate_all(function(x) base::replace(x, base::is.na(x), "")) |>
        purrr::pmap(bibliogR::make_bib_entry) |>
        base::unlist() |>
        base::writeLines(bibfile, useBytes = TRUE)
    } else {
      base::writeLines("", bibfile, useBytes = TRUE)
    }
  } else {
    base::writeLines("", bibfile, useBytes = TRUE)
  }
}
