#' @name child_row_table
#' @title Table with collapsible rows (3)
#' @author Nicolas Mangin
#' @description Function formatting references with collapsible rows
#' @param x Data.frame. Table of references.
#' @param pos Integer. Position of the row.
#' @return A data.table with collapsed rows.
#' @importFrom glue glue
#' @export


child_row_table <- function(x, pos = NULL) {
  names_x <- base::paste0(base::names(x), ":")
  text <- "
  var format = function(d) {
    text = '<div><table >' +
  "
  for (i in base::seq_along(pos)) {
    text <- base::paste(text, glue::glue(
      "'<tr>' +
          '<td>' + '{names_x[pos[i]]}' + '</td>' +
          '<td>' + d[{pos[i]}] + '</td>' +
        '</tr>' + "
    ))
  }
  base::paste0(
    text,
    "'</table></div>'
      return text;};"
  )
}
