#' @name make_bib_entry
#' @title Write an entry for a bibtex file
#' @author Nicolas Mangin
#' @description Function taking one row of a reference list and writing the corresponding entry.
#' @param bibtype Character.
#' @param key Character.
#' @param author Character.
#' @param year Character.
#' @param title Character.
#' @param journal Character.
#' @param volume Character.
#' @param number Character.
#' @param pages Character.
#' @param doi Character.
#' @param url Character.
#' @param publisher Character.
#' @param booktitle Character.
#' @param editor Character.
#' @param address Character.
#' @param chapter Character.
#' @param edition Character.
#' @param isbn Character.
#' @return A character vector in which each elements is a row of a bib entry.
#' @export


make_bib_entry <- function(
  bibtype,
  key,
  author,
  year,
  title,
  journal,
  volume,
  number,
  pages,
  doi,
  url,
  publisher,
  booktitle,
  editor,
  address,
  chapter,
  edition,
  isbn
){
  c(
    base::paste0("@", bibtype, "{", key, ","),
    base::paste0('  bibtype = {', bibtype, '},'),
    base::paste0('  key = {', key, '},'),
    base::paste0('  author = {', author, '},'),
    base::paste0('  year = {', year, '},'),
    base::paste0('  title = {', title, '},'),
    base::paste0('  journal = {', journal, '},'),
    base::paste0('  volume = {', volume, '},'),
    base::paste0('  number = {', number, '},'),
    base::paste0('  pages = {', pages, '},'),
    #base::paste0('  doi = {', doi, '},'),
    base::paste0('  url = {', url, '},'),
    base::paste0('  publisher = {', publisher, '},'),
    base::paste0('  booktitle = {', booktitle, '},'),
    base::paste0('  editor = {', editor, '},'),
    base::paste0('  address = {', address, '},'),
    base::paste0('  chapter = {', chapter, '},'),
    base::paste0('  edition = {', edition, '},'),
    base::paste0('  isbn = {', isbn, '},'),
    "}",
    ''
  )
}
