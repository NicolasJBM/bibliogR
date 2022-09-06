#' @name callback
#' @title Table with collapsible rows (2)
#' @author Nicolas Mangin
#' @description Function formatting references with collapsible rows
#' @param x Data.frame. Table of references.
#' @param pos Integer. Position of the row.
#' @return A data.table with collapsed rows.
#' @export


callback <- function(x, pos = NULL) {
  part1 <- "table.column(1).nodes().to$().css({cursor: 'pointer'});"
  part2 <- bibliogR::child_row_table(x, pos = pos)
  part3 <- "
   table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&ominus;');
    }
  });"
  base::paste(part1, part2, part3)
}

