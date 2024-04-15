#' @name prepare_ebsco_import
#' @title Prepare EBSCO files
#' @author Nicolas Mangin
#' @description Format csv exports from EBSCO to be imported in references. 
#' @param files Character vector. List of paths to the csv files to be imported.
#' @param type Bibtype to be imported. Defaut to "article".
#' @return A clean list of references which can be imported with bibliogR interface.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_title
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export


prepare_ebsco_import <- function(files, type = "article"){
  
  author <- NULL
  data <- NULL
  journal <- NULL
  pages <- NULL
  pages2 <- NULL
  title <- NULL
  
  files <- files[stringr::str_detect(files, ".csv$")]
  
  references <- tibble::tibble(
    file = files
  ) |>
    dplyr::mutate(data = purrr::map(file, readr::read_csv, col_types = "cccccccccccccccccc")) |>
    dplyr::select(-file) |>
    tidyr::unnest(data) |>
    dplyr::select(
      title = 'Article Title',
      author = 'Author',
      journal = 'Journal Title',
      year = 'Publication Date',
      volume = 'Volume',
      number = 'Issue',
      pages = 'First Page',
      pages2 = 'Page Count',
      doi = 'DOI',
      publisher = 'Publisher',
      bibtype = 'Doctype',
      keywords = 'Keywords',
      abstract = 'Abstract',
      url = 'PLink'
    )
  
  references$year <- base::gsub("\\D", "", references$year)
  if (!base::is.na(type)){
    references <- references[stringr::str_detect(base::tolower(references$bibtype), type),]
  }
  
  references <- references |>
    dplyr::mutate(pages = base::as.numeric(pages2), pages2 = base::as.numeric(pages2)) |>
    dplyr::mutate(
      title = stringr::str_to_title(title),
      author = stringr::str_to_title(author),
      pages = base::paste0(pages, "--", pages + pages2)
    ) |>
    dplyr::select(-pages2) |>
    dplyr::filter(!base::is.na(journal)) |>
    dplyr::mutate(
      author = stringr::str_replace_all(author, "; ", " and "),
      bibtype = type,
    ) |>
    dplyr::filter(!base::is.na(author), base::nchar(author) > 3, stringr::str_detect(author, ","))
  
  return(references)
}
