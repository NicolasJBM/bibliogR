#' @name create_reference_file
#' @title Create a reference file
#' @author Nicolas Mangin
#' @description
#' Function to initialize a properly formated xlsx file to gather references.
#' @return A properly formatted reference.xlsx file in hte working directory.
#' @importFrom WriteXLS WriteXLS
#' @export


create_reference_file <- function() {
  references <- NULL
  load(paste0(find.package("bibliogR"), "/references.RData"))
  WriteXLS::WriteXLS(references, paste0("references_", Sys.Date(),".xlsx"))
}
