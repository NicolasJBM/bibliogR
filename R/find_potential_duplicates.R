#' @name find_potential_duplicates
#' @title Identify potential duplicates in a list of references
#' @author Nicolas Mangin
#' @description Function computing distances between titles and selecting those which are too close as defined by a maximum distance.
#' @param x Tibble. Table with keys and titles.
#' @param distmethod Character. Method to compute distances between titles. Can be: osa, lv, dl, hammig, lcs, qgram, cosine, jaccard, jw, or soundex.
#' @param maxdist Numeric. Threshold to apply. Only titles the distance between which is smaller or equal to this number will be returned for check.
#' @return A numeric vector with the row numbers of the potential duplicates
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom purrr map2_int
#' @importFrom stringdist stringdistmatrix
#' @importFrom stringr str_remove_all
#' @importFrom tibble as_tibble
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr pivot_longer
#' @export


find_potential_duplicates <- function(x, distmethod = "qgram", maxdist = 10){
  
  src <- NULL
  tgt <- NULL
  STR1 <- NULL
  STR2 <- NULL
  distance <- NULL
  
  text <- x$title
  distances <- stringdist::stringdistmatrix(text, text, method = distmethod) |>
    tibble::as_tibble(.name_repair = "minimal")
  base::names(distances) <- base::paste0("V", base::row.names(distances))
  distances <- distances |>
    tibble::rowid_to_column("src") |>
    tidyr::pivot_longer(dplyr::starts_with("V"), names_to = "tgt", values_to = "distance") |>
    dplyr::mutate(tgt = base::as.numeric(stringr::str_remove_all(tgt, "V"))) |>
    dplyr::mutate(
      STR1 = purrr::map2_int(src, tgt, base::min),
      STR2 = purrr::map2_int(src, tgt, base::max)
    ) |>
    dplyr::select(STR1,STR2, distance) |>
    dplyr::filter(STR1 != STR2, distance <= maxdist) |>
    base::unique()
  at_risk <- base::unique(c(distances$STR1, distances$STR2))
  return(at_risk)
}
