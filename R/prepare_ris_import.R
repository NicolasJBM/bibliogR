#' @name prepare_ris_import
#' @title Prepare ris files
#' @author Nicolas Mangin
#' @description Format .ris files to be imported in references. 
#' @param files Character vector. List of paths to the .ris files to be imported.
#' @param type Bibtype to be imported. Defaut to "article".
#' @return A clean list of references which can be imported with bibliogR interface.
#' @export


prepare_ris_import <- function(files, type = "article"){
  
  AB <- NULL
  AU <- NULL
  DO <- NULL
  IS <- NULL
  JO <- NULL
  KW <- NULL
  PB <- NULL
  PY <- NULL
  SP <- NULL
  T1 <- NULL
  TY <- NULL
  UR <- NULL
  VL <- NULL
  SN <- NULL
  abstract <- NULL
  author <- NULL
  bibtype <- NULL
  content <- NULL
  journal <- NULL
  keywords <- NULL
  lines <- NULL
  title <- NULL
  variable <- NULL
  
  files <- files[stringr::str_detect(files, ".ris$")]
  
  nfiles <- base::length(files)
  
  references <- base::list(nfiles)
  
  for (i in base::seq_len(nfiles)){
    file <- files[i]
    tmpfile <- tibble::tibble(
      lines = base::suppressWarnings(base::readLines(file))
    ) |>
      dplyr::mutate(cut = base::as.numeric(stringr::str_detect(lines, "^ER"))) |>
      dplyr::mutate(cut = base::cumsum(cut)) |>
      dplyr::filter(base::nchar(lines) > 0) |>
      dplyr::filter(!stringr::str_detect(lines, "^ER ")) |>
      dplyr::group_by(cut) |>
      dplyr::group_split()
    
    tmpfile <- base::lapply(tmpfile, function(x){
      y <- x |>
        dplyr::ungroup() |>
        dplyr::select(-cut) |>
        tidyr::separate(lines, into = c("variable","content"), sep = "  - ") |>
        dplyr::group_by(variable) |>
        dplyr::summarise(content = base::paste(content, collapse = " ::: ")) |>
        tidyr::pivot_wider(names_from = "variable", values_from = "content")
      
      need <- c(
        "T1", "AU", "JO", "SN", "PY", "VL", "IS", "SP",
        "DO", "PB", "TY", "KW", "AB", "UR"
      )
      
      missing <- base::setdiff(need, base::names(y))
      y[missing] <- base::as.character(NA)
      
      y <- y |>
        dplyr::select(
          title = T1,
          author = AU,
          journal = JO,
          issn = SN,
          year = PY,
          volume = VL,
          number = IS,
          pages = SP,
          doi = DO,
          publisher = PB,
          bibtype = TY,
          keywords = KW,
          abstract = AB,
          url = UR
        ) |>
        dplyr::mutate(
          author = stringr::str_replace_all(author, " ::: ", " and "),
          keywords = stringr::str_replace_all(keywords, " ::: ", "; "),
          bibtype = stringr::str_replace(bibtype, "JOUR", "article")
        )
    })
    
    tmpfile <- dplyr::bind_rows(tmpfile)
    references[[i]] <- tmpfile
  }
  
  references <- dplyr::bind_rows(references) |>
    dplyr::filter(base::tolower(bibtype) == type) |>
    dplyr::mutate(
      title = stringr::str_to_title(title),
      keywords = stringr::str_to_lower(keywords),
      abstract = stringr::str_to_sentence(abstract)
    ) |>
    dplyr::filter(!base::is.na(journal)) |>
    dplyr::filter(!base::is.na(author), base::nchar(author) > 3)
  
  return(references)
}
