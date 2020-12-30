#' @name get_new_references
#' @title Prepare New References for Combination
#' @author Nicolas Mangin
#' @description Import and format references from bibtex files.
#' @param file  character. Path to the .bib file.
#' @return A list of references which can be appended to the main file.
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_title
#' @importFrom furrr future_map_chr
#' @importFrom furrr future_map_dbl
#' @importFrom tidyr unite
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom utils read.csv
#' @importFrom RefManageR ReadBib
#' @export

get_new_references <- function(file) {

  # Bind variables
  author <- NULL
  title <- NULL
  abstract <- NULL
  volume <- NULL
  number <- NULL
  keywords <- NULL
  pages <- NULL
  doi <- NULL
  year <- NULL

  # Import the file
  import <- RefManageR::ReadBib(file = file) %>%
    as.data.frame()

  # Add missing variables
  info <- c(
    "key", "order", "bibtype", "author", "title", "journal",
    "jnl", "issn", "field", "year", "volume", "number", "pages",
    "doi", "abstract", "keywords", "url", "publisher", "booktitle",
    "editor", "address", "chapter", "edition", "isbn", "comment",
    "note"
  )
  missing <- setdiff(info, names(import))
  for (name in missing) {
    import[, name] <- ""
  }

  # Clean the references
  import <- import %>%
    tidyr::replace_na(list(keywords = "", subjects = "", abstract = "")) %>%
    dplyr::mutate(
      title = furrr::future_map_chr(title, clean_string, simplify = FALSE),
      author = furrr::future_map_chr(author, bibliogR::format_authors),
      year = furrr::future_map_dbl(
        year,
        function(x) {
          as.numeric(paste(
            unlist(stringr::str_extract_all(x, "[0-9]")),
            collapse = ""
          ))
        }
      ),
      volume = furrr::future_map_chr(
        volume,
        function(x) {
          paste(
            unlist(stringr::str_extract_all(x, "[0-9]")),
            collapse = ""
          )
        }
      ),
      number = furrr::future_map_chr(
        number,
        function(x) {
          paste(
            unlist(stringr::str_extract_all(x, "[0-9]")),
            collapse = ""
          )
        }
      ),
      pages = stringr::str_replace_all(pages, " - ", "--"),
      keywords = furrr::future_map_chr(
        keywords, clean_string, simplify = FALSE),
      abstract = furrr::future_map_chr(
        abstract, clean_string, simplify = FALSE),
      doi = stringr::str_replace_all(doi, "https://doi.org/", "")
    ) %>%
    dplyr::mutate(title = stringr::str_to_title(title))

  # Enforce proper order of variables
  import <- import %>%
    dplyr::select(dplyr::all_of(info))

  return(import)
}
