#' @name make_bib_entry
#' @title Write an entry for a bibtex file
#' @author Nicolas Mangin
#' @description Function taking one row of a reference list and writing the corresponding entry which will be added to a reference file.
#' @param bibtype Character.
#' @param key Character.
#' @param author Character.
#' @param title Character.
#' @param journal Character.
#' @param year Character.
#' @param month Character.#' 
#' @param volume Character.
#' @param number Character.
#' @param pages Character.
#' @param publisher Character.
#' @param booktitle Character.
#' @param editor Character.
#' @param institution Character.
#' @param school Character.
#' @param address Character.
#' @param edition Character.
#' @param note Character.
#' @param doi Character.
#' @param url Character.
#' @param abstract Character.
#' @param keywords Character.
#' @param isbn Character.
#' @param issn Character.
#' @return A character vector in which each elements is a row of a bib entry.
#' @export


make_bib_entry <- function(
    bibtype,
    key, 
    author,
    title,
    journal,
    year,
    month,
    volume,
    number,
    pages,
    publisher,
    booktitle,
    editor,
    institution,
    school,
    address,
    edition,
    note,
    doi,
    url,
    abstract,
    keywords,
    isbn,
    issn
){
  c(
    base::paste0("@", bibtype, "{", key, ","),
    if (!base::is.na(author)) base::paste0('  author = {', author, '},') else NA,
    if (!base::is.na(title)) base::paste0('  title = {', title, '},') else NA,
    if (!base::is.na(journal)) base::paste0('  journal = {', journal, '},') else NA,
    if (!base::is.na(year)) base::paste0('  year = {', year, '},') else NA,
    if (!base::is.na(month)) base::paste0('  month = {', month, '},') else NA,
    if (!base::is.na(volume)) base::paste0('  volume = {', volume, '},') else NA,
    if (!base::is.na(number)) base::paste0('  number = {', number, '},') else NA,
    if (!base::is.na(pages)) base::paste0('  pages = {', pages, '},') else NA,
    if (!base::is.na(publisher)) base::paste0('  publisher = {', publisher, '},') else NA,
    if (!base::is.na(booktitle)) base::paste0('  booktitle = {', booktitle, '},') else NA,
    if (!base::is.na(editor)) base::paste0('  editor = {', editor, '},') else NA,
    if (!base::is.na(institution)) base::paste0('  editor = {', institution, '},') else NA,
    if (!base::is.na(school)) base::paste0('  editor = {', school, '},') else NA,
    if (!base::is.na(address)) base::paste0('  address = {', address, '},') else NA,
    if (!base::is.na(edition)) base::paste0('  edition = {', edition, '},') else NA,
    if (!base::is.na(note)) base::paste0('  note = {', note, '},') else NA,
    #if (!base::is.na(doi)) base::paste0('  doi = {', doi, '},') else NA,
    #if (!base::is.na(url)) base::paste0('  url = {', url, '},') else NA,
    #if (!base::is.na(abstract)) base::paste0('  abstract = {', abstract, '},') else NA,
    #if (!base::is.na(keywords)) base::paste0('  keywords = {', keywords, '},') else NA,
    #if (!base::is.na(isbn)) base::paste0('  isbn = {', isbn, '},') else NA,
    #if (!base::is.na(issn)) base::paste0('  issn = {', issn, '},') else NA,
    "}",
    ''
  ) |>
    stats::na.omit()
}
