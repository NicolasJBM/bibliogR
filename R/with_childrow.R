#' @name with_childrow
#' @title Table with collapsible rows (1)
#' @author Nicolas Mangin
#' @description Function formatting references with collapsible rows
#' @param x Data.frame. Table of references.
#' @param vars Character vector. Name of the variables to collapse.
#' @param opts List. List of options.
#' @param ... Other arguments.
#' @return A data.table with collapsed rows.
#' @importFrom DT datatable
#' @importFrom DT JS
#' @importFrom purrr map_chr
#' @export


with_childrow <- function(x, vars = NULL, opts = NULL, ...) {
  names_x <- base::names(x)
  if (base::is.null(vars)) stop("'vars' must be specified!")
  pos <- base::match(vars, names_x)
  if (base::any(purrr::map_chr(x[, pos], typeof) == "list")) {
    stop("list columns are not supported in datatable2()")
  }
  pos <- pos[pos <= base::ncol(x)] + 1
  base::rownames(x) <- NULL
  if (base::nrow(x) > 0) x <- base::cbind(" " = "&oplus;", x)
  # options
  opts <- c(
    opts,
    base::list(
      columnDefs = base::list(
        base::list(visible = FALSE, targets = c(0, pos)),
        base::list(
          orderable = FALSE, className = "details-control", targets = 1
        ),
        base::list(className = "dt-left", targets = 1:3),
        base::list(className = "dt-right", targets = 4:base::ncol(x))
      )
    )
  )
  DT::datatable(
    x,
    ...,
    escape = -2,
    options = opts,
    callback = DT::JS(bibliogR::callback(x = x, pos = c(0, pos)))
  )
}

