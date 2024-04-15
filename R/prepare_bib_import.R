#' @name prepare_bib_import
#' @title Prepare bib files
#' @author Nicolas Mangin
#' @description Format .bib files to be imported in references. 
#' @param files Character vector. List of paths to the .bib files to be imported.
#' @param type Bibtype to be imported. Defaut to "article".
#' @return A clean list of references which can be imported with bibliogR interface.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr group_split
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_title
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_to_sentence
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate
#' @export


prepare_bib_import <- function(files, type = "article"){
  
  bibtype <- NULL
  lines <- NULL
  abstract <- NULL
  author <- NULL
  journal <- NULL
  keywords <- NULL
  title <- NULL
  
  files <- files[stringr::str_detect(files, ".bib$")]
  
  nfiles <- base::length(files)
  
  references <- base::list(nfiles)
  
  for (i in base::seq_len(nfiles)){
    file <- files[i]
    tmpfile <- tibble::tibble(
      lines = base::suppressWarnings(base::readLines(file))
    ) |>
      dplyr::mutate(cut = base::as.numeric(stringr::str_detect(lines, "^@"))) |>
      dplyr::mutate(cut = base::cumsum(cut)) |>
      dplyr::group_by(cut) |>
      dplyr::group_split()
    
    tmpfile <- base::lapply(tmpfile, function(x){
      y <- x |>
        dplyr::ungroup() |>
        dplyr::select(-cut) |>
        dplyr::mutate(lines = stringr::str_replace(lines, "^@","bibtype = ")) |>
        dplyr::filter(!stringr::str_detect(lines, "^\\}")) |>
        tidyr::separate(lines, into = c("variable","content"), sep = " = ") |>
        tidyr::pivot_wider(names_from = "variable", values_from = "content")
      y$bibtype[1] <- stringr::str_remove_all(y$bibtype[1], "\\{.+$")
      dplyr::mutate_all(y, function(z){
        z |>
          stringr::str_remove_all("^\\{|\\}|\\,$")
      })
    })
    
    tmpfile <- dplyr::bind_rows(tmpfile)
    references[[i]] <- tmpfile
  }
  
  references <- dplyr::bind_rows(references) |>
    dplyr::filter(base::tolower(bibtype) == type)
  
  keep <- c(
    "title", "author", "journal", "year", "volume", "number", "pages", "pages2",
    "doi", "publisher", "bibtype", "keywords", "abstract", "url"
  )
  
  references <- references[, base::intersect(keep, base::names(references))] |>
    dplyr::mutate(
      title = stringr::str_to_title(title),
      keywords = stringr::str_to_lower(keywords),
      abstracts = stringr::str_to_sentence(abstract)
    ) |>
    dplyr::filter(!base::is.na(journal)) |>
    dplyr::filter(!base::is.na(author), base::nchar(author) > 3, stringr::str_detect(author, ","))
  
  return(references)
}
