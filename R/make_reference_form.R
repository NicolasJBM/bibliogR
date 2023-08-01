#' @name make_reference_form
#' @title Create a reference edition form
#' @author Nicolas Mangin
#' @description Function creating a shiny form allowing the user to edit old or add new references in the main reference database.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param refkey Character. Bib key of the reference to edit.
#' @param type Character. Bib type of the reference to edit.
#' @param references Tibble. List of references.
#' @return A form with all the fields which should or can be filled for each type.
#' @importFrom dplyr filter
#' @importFrom shiny NS
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny selectizeInput
#' @importFrom shiny textAreaInput
#' @importFrom shiny textInput
#' @export


make_reference_form <- function(id, refkey, type, references){
  
  ns <- shiny::NS(id)
  
  if (refkey %in% references$key){
    selected <- dplyr::filter(references, key == refkey)
    bibtype <- selected$bibtype[[1]]
    key <- selected$key[[1]]
    author <- selected$author[[1]]
    title <- selected$title[[1]]
    journal <- selected$journal[[1]]
    year <- selected$year[[1]]
    month <- selected$month[[1]]
    volume <- selected$volume[[1]]
    number <- selected$number[[1]]
    pages <- selected$pages[[1]]
    publisher <- selected$publisher[[1]]
    booktitle <- selected$booktitle[[1]]
    editor <- selected$editor[[1]]
    institution <- selected$institution[[1]]
    school <- selected$school[[1]]
    address <- selected$address[[1]]
    edition <- selected$edition[[1]]
    note <- selected$note[[1]]
    doi <- selected$doi[[1]]
    url <- selected$url[[1]]
    abstract <- selected$abstract[[1]]
    keywords <- selected$keywords[[1]]
    isbn <- selected$isbn[[1]]
    issn <- selected$issn[[1]]
    jnl <- selected$jnl[[1]]
    field <- selected$field[[1]]
  } else {
    bibtype <- type
    key <- NA
    author <- NA
    title <- NA
    journal <- NA
    year <- NA
    month <- NA
    volume <- NA
    number <- NA
    pages <- NA
    publisher <- NA
    booktitle <- NA
    editor <- NA
    institution <- NA
    school <- NA
    address <- NA
    edition <- NA
    note <- NA
    doi <- NA
    url <- NA
    abstract <- NA
    keywords <- NA
    isbn <- NA
    issn <- NA
    jnl <- NA
    field <- NA
  }
  
  
  # Create fields
  
  refbibtype <- shiny::tags$h4(bibtype)
  refkey <- shiny::tags$h4(key)
  refauthor <- shiny::textAreaInput(ns("refauthor"), "author: (lastname1, firstname1 and lastname2, firstname2 and...)", value = author, width = "100%", height = "50px")
  reftitle <- shiny::textAreaInput(ns("reftitle"), "title:", value = title, width = "100%", height = "100px")
  refjournal <- shiny::selectizeInput(
    ns("refjournal"), "journal:",
    choices = c(NA, stats::na.omit(base::sort(base::unique(references$journal)))),
    selected = journal,
    multiple = FALSE,
    options = base::list(create = TRUE),
    width = "100%"
  )
  refyear <- shiny::textInput(ns("refyear"), "year:", value = year, width = "100%")
  refmonth <- shiny::textInput(ns("refmonth"), "month:", value = month, width = "100%")
  refvolume <- shiny::textInput(ns("refvolume"), "volume:", value = volume, width = "100%")
  refnumber <- shiny::textInput(ns("refnumber"), "number:", value = number, width = "100%")
  refpages <- shiny::textInput(ns("refpages"), "pages (00--11):", value = pages, width = "100%")
  refpublisher <- shiny::textInput(ns("refpublisher"), "publisher:", value = publisher, width = "100%")
  refbooktitle <- shiny::textInput(ns("refbooktitle"), "booktitle:", value = booktitle, width = "100%")
  refeditor <- shiny::textAreaInput(ns("refeditor"), "editor: (lastname1, firstname1 and lastname2, firstname2 and...)", value = editor, width = "100%", height = "50px")
  refinstitution <- shiny::textInput(ns("refinstitution"), "institution:", value = institution, width = "100%")
  refschool <- shiny::textInput(ns("refschool"), "school:", value = school, width = "100%")
  refaddress <- shiny::textInput(ns("refaddress"), "address:", value = address, width = "100%")
  refedition <- shiny::textInput(ns("refedition"), "edition:", value = edition, width = "100%")
  refnote <- shiny::textAreaInput(ns("refnote"), "note:", value = note, width = "100%", height = "50px")
  refdoi <- shiny::textInput(ns("refdoi"), "doi:", value = doi, width = "100%")
  refurl <- shiny::textInput(ns("refurl"), "url:", value = url, width = "100%")
  refabstract <- shiny::textAreaInput(ns("refabstract"), "abstract:", value = abstract, width = "100%", height = "300px")
  refkeywords <- shiny::textAreaInput(ns("refkeywords"), "keywords:", value = keywords, width = "100%", height = "50px")
  refisbn <- shiny::textInput(ns("refisbn"), "isbn:", value = isbn, width = "100%")
  refissn <- shiny::textInput(ns("refissn"), "issn:", value = issn, width = "100%")
  
  # Organize fields in the interface based on the type
  
  if (bibtype == "article") {
    #mandatory: author, title, journal, year
    #optional: volume, number, pages, month, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey,
        reftitle, refauthor, refyear, refjournal
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refvolume, refnumber, refpages, refmonth, refnote 
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refpublisher, refbooktitle, refeditor, refinstitution,
        refschool, refaddress, refedition, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "book") {
    #mandatory: author, title, publisher, year
    #optional: volume, number, address, editor, edition, month, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey,
        reftitle, refauthor, refyear, refpublisher, refaddress
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refmonth, refvolume, refnumber, refeditor,
        refedition, refnote
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal,
        refpages, refbooktitle, refinstitution,
        refschool, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "booklet") {
    #mandatory: title
    #optional: author, address, year, month, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey,
        reftitle, refauthor, refyear
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refaddress, refmonth, refnote 
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refvolume, refnumber,
        refpages, refpublisher, refbooktitle, refeditor, refinstitution,
        refschool, refedition, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "inbook") {
    #mandatory: author, title, pages, publisher, year
    #optional: volume, number, address, editor, edition
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey,
        reftitle, refauthor, refyear, refpages, refpublisher
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refvolume, refnumber, refaddress, refeditor, refedition
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refmonth, 
        refbooktitle, refinstitution,
        refschool, refnote, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "incollection") {
    #mandatory: author, title, booktitle, publisher, year
    #optional: editor, volume, number, pages, address, edition, month, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear, refbooktitle, refeditor, refpublisher, refaddress
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refvolume, refnumber, refpages, refedition, refmonth, refnote 
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refinstitution,
        refschool,  refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "inproceedings") {
    #mandatory: author, title, booktitle, year
    #optional: editor, volume, number, pages, address, month, publisher, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear, refbooktitle
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refeditor, refvolume, refnumber, refpages, refpublisher,
        refaddress, refmonth, refnote
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refinstitution,
        refschool, refedition, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "manual") {
    #mandatory: title
    #optional: author, address, edition, month, year, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refaddress, refedition, refnote, refmonth
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refvolume, refnumber,
        refpages, refpublisher, refbooktitle, refeditor, refinstitution,
        refschool, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "masterthesis") {
    #mandatory: a
    #uthor, title, school, year
    #optional: address, month, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear, refschool,
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refaddress, refmonth, refnote
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refvolume, refnumber,
        refpages, refpublisher, refbooktitle, refeditor, refinstitution,
        refedition, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "misc") {
    #mandatory: 
    #optional: author, title, month, year, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refmonth, refnote
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refvolume, refnumber,
        refpages, refpublisher, refbooktitle, refeditor, refinstitution,
        refschool, refaddress, refedition,  refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "phdthesis") {
    #mandatory: author, title, school, year
    #optional: address, month, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear, refschool
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refaddress, refmonth, refnote
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refvolume, refnumber,
        refpages, refpublisher, refbooktitle, refeditor, refinstitution,
        refedition, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "proceedings") {
    #mandatory: title, year
    #optional: editor, volume, number, address, publisher, note, month
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refeditor, refvolume, refnumber, refpublisher, refaddress,
        refmonth, refnote, 
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, 
        refpages, refbooktitle, refinstitution,
        refschool, refedition, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "techreport") {
    #mandatory: author, title, institution, year
    #optional: number, address, month, note
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear, refinstitution
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refnumber, refaddress, refmonth, refnote
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refvolume, 
        refpages, refpublisher, refbooktitle, refeditor,
        refschool, refedition, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  } else if (bibtype == "unpublished") {
    #mandatory: author, title, note
    #optional: month, year
    ui <- base::list(shiny::fluidRow(
      shiny::column(
        4,
        shiny::tags$h3("Mandatory fields"),
        refbibtype, refkey, 
        reftitle, refauthor, refyear, refnote
      ),
      shiny::column(
        4,
        shiny::tags$h3("Optional fields"),
        refmonth
      ),
      shiny::column(
        4,
        shiny::tags$h3("Other fields"),
        refjournal, refvolume, refnumber,
        refpages, refpublisher, refbooktitle, refeditor, refinstitution,
        refschool, refaddress, refedition, refdoi, refurl,
        refabstract, refkeywords, refisbn, refissn
      )
    ))
  }
}
