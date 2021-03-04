#' @name match_new_reference
#' @title Identify Potential Matches in References
#' @author Nicolas Mangin
#' @description Determine whether a reference to be added is already in the initial list of references
#' @param tmpkey     Character string. Temporary key.
#' @param journal    Character string. Journal of the document to be matched.
#' @param year       Integer. Year of the document to be matched.
#' @param title      Character string. Title of the document to be matched.
#' @param author     Character string. Author of the document to be matched.
#' @param method     Character string. Method for stringdist.
#' @param references Tibble. Initial database of references.
#' @return A match between keys.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate_all
#' @importFrom stringdist stringdist
#' @importFrom future plan
#' @importFrom furrr future_map_dbl


match_new_reference <- function(tmpkey,
                                journal,
                                year,
                                title,
                                author,
                                method = "osa",
                                references) {


  # Bind variables for dplyr
  key <- NULL
  disttitle <- NULL
  nchartitle <- NULL
  distauth <- NULL
  ncharauth <- NULL
  newtitle <- NULL
  newauthor <- NULL

  # Re-assign reference information to avoid conflict in filter
  jrnl <- as.character(journal)
  yr <- as.numeric(year)
  ttl <- as.character(title)
  auth <- as.character(author)

  match <- references %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(journal == jrnl, year == yr) %>%
    dplyr::select(key, title, author) %>%
    dplyr::mutate_all(as.character)

  future::plan("multisession")

  if (nrow(match) > 0) {
    match <- match %>%
      dplyr::mutate(
        disttitle = furrr::future_map_dbl(
          title,
          stringdist::stringdist,
          b = ttl, method = method
        ),
        nchartitle = nchar(title),
        distauth = furrr::future_map_dbl(
          author,
          stringdist::stringdist,
          b = auth, method = method
        ),
        ncharauth = nchar(author)
      ) %>%
      dplyr::mutate(
        disttitle = disttitle / nchartitle,
        distauth = distauth / ncharauth
      ) %>%
      dplyr::filter(disttitle == min(disttitle)) %>%
      dplyr::filter(distauth == min(distauth)) %>%
      dplyr::mutate(
        tmpkey = tmpkey,
        newtitle = ttl,
        newauthor = auth
      ) %>%
      dplyr::select(
        disttitle, distauth,
        tmpkey, key,
        title, newtitle,
        author, newauthor
      )
  } else {
    match <- tibble(
      tmpkey = "", key = "",
      disttitle = 1, distauth = 1,
      title = "", newtitle = "",
      author = "", newauthor = ""
    )
  }

  return(match)
}
