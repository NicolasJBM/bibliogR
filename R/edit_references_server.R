#' @name edit_references_server
#' @title Search and edit references
#' @author Nicolas Mangin
#' @description Module facilitating their edition on a case by case basis.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param refdir Character. Path to the folder where the references are.
#' @return Save edited list of references
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny selectizeInput
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export



edit_references_server <- function(id, course_paths, refdir){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    key <- NULL
    bibtype <- NULL
    author <- NULL
    title <- NULL
    journal <- NULL
    year <- NULL
    month <- NULL
    volume <- NULL
    number <- NULL
    pages <- NULL
    publisher <- NULL
    booktitle <- NULL
    editor <- NULL
    institution <- NULL
    school <- NULL
    address <- NULL
    edition <- NULL
    note <- NULL
    doi <- NULL
    url <- NULL
    abstract <- NULL
    keywords <- NULL
    isbn <- NULL
    issn <- NULL
    jnl <- NULL
    field <- NULL
    
    refpath <- shiny::reactive({ base::paste0(refdir, "/references.RData") })
    
    references <- shiny::reactive({
      input$loadref
      input$saved
      input$deleted
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Loading references..."
      )
      shiny::req(base::file.exists(refpath()))
      base::load(refpath())
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "References loaded!",
        text = "All references are now loaded.",
        type = "success"
      )
      references
    })
    
    
    
    shiny::observeEvent(input$editref, {
      shiny::showModal(shiny::modalDialog(
        title = "Edit old or new entry?",
        "Enter an existing key to edit an old entry OR select a type for a new one.",
        shiny::textInput(ns("defbibkey"), "Key:", value = NA, width = "100%"),
        shiny::selectInput(
          ns("slctbibtype"), "bibtype:",
          choices = c(
            "article","book","booklet","inbook","incollection","inproceedings",
            "manual","masterthesis","misc","phdthesis","proceedings","techreport",
            "unpublished"
          ),
          selected = "article", width = "100%"
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("fillref"), "OK")
        )
      ))
    })
    
    shiny::observeEvent(input$fillref, {
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        title = "Reference",
        "Fill in at least the mandatory fields.",
        bibliogR::make_reference_form(
          id, input$defbibkey, input$slctbibtype, references()
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            ns("saveref"), "Save", icon = shiny::icon("floppy-disk"),
            style = "background-color:#006600;color:#FFF;"
          )
        )
      ))
    })
    
    shiny::observeEvent(input$saveref, {
      # Retrieve input
      add <- tibble::tibble(
        author = input$refauthor,
        title = input$reftitle,
        journal = input$refjournal,
        year = input$refyear,
        month = input$refmonth,
        volume = input$refvolume,
        number = input$refnumber,
        pages = input$refpages,
        publisher = input$refpublisher,
        booktitle = input$refbooktitle,
        editor = input$refeditor,
        institution = input$refinstitution,
        school = input$refschool,
        address = input$refaddress,
        edition = input$refedition,
        note = input$refnote,
        doi = input$refdoi,
        url = input$refurl,
        abstract = input$refabstract,
        keywords = input$refkeywords,
        isbn = input$refisbn,
        issn = input$refissn
      ) |>
        dplyr::mutate_all(
          function(x) if (x == "" | x == "NA") x <- NA else x <- x
        )
      # Add it to the database
      if (input$defbibkey %in% references()$key){
        add$key <- input$defbibkey
        add$bibtype <- references() |>
          dplyr::filter(key == input$defbibkey) |>
          dplyr::select(bibtype) |>
          base::unlist() |> base::as.character()
        references <- references() |>
          dplyr::filter(key != input$defbibkey)
      } else {
        add$key <- bibliogR::make_new_key(
          name = base::unlist(stringr::str_split(add$author[[1]], ","))[1],
          year = add$year[[1]],
          keys = references()$key
        )
        add$bibtype <- input$slctbibtype
        references <- references()
      }
      # Append journal information
      journal_info <- references() |>
        dplyr::select(journal, issn, jnl, field) |>
        base::unique() |> stats::na.omit()
      if (add$journal %in% journal_info$journal) {
        add <- add |>
          dplyr::select(-issn) |>
          dplyr::left_join(journal_info, by = "journal")
      }
      # Save
      references <- references |>
        dplyr::bind_rows(add) |>
        dplyr::select(
        bibtype, key,
        author, title, journal, year, month, volume, number, pages, publisher,
        booktitle, editor, institution, school, address, edition, note,
        doi, url, abstract, keywords, isbn, issn,
        jnl, field
      )
      shiny::removeModal()
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Saving references..."
      )
      base::save(references, file = refpath())
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "References saved!",
        text = "Now reload the references to see the changes you made.",
        type = "success", closeOnEsc = FALSE, inputId = "saved"
      )
    })
    
    
    
    shiny::observeEvent(input$delref, {
      shiny::showModal(shiny::modalDialog(
        title = "References removal",
        "Write down the citation keys of the references you wish to remove.",
        shiny::selectizeInput(
          ns("defrmkeys"), "Keys:", choices = NA, selected = NA,
          multiple = TRUE, options = base::list(create = TRUE), width = "100%"
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            ns("delkeys"), "OK", icon = shiny::icon("trash-can"),
            style = "background-color:#660000;color:#FFF;"
          )
        )
      ))
    })
    
    shiny::observeEvent(input$delkeys, {
      shiny::removeModal()
      shinyalert::shinyalert(
        "Remove these references?",
        base::paste0(
          "Are you sure you want to remove these references: ",
          base::paste(input$defrmkeys, collapse = ", "), "?"
        ),
        showCancelButton = TRUE, cancelButtonText = "Cancel",
        confirmButtonText = "Delete", inputId = "confirmdelkey",
        type = "warning"
      )
    })
    
    shiny::observeEvent(input$confirmdelkey, {
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Deleting references..."
      )
      references <- references() |>
        dplyr::filter(!(key %in% input$defrmkeys))
      base::save(references, file = refpath())
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "References deleted!",
        text = "Now reload the references to see the changes you made.",
        type = "success", closeOnEsc = FALSE, inputId = "deleted"
      )
    })
    
    
    
    shiny::observeEvent(input$updateref, {
      if (base::length(course_paths()) == 1){
        shinyalert::shinyalert(
          title = "Missing selections!",
          text = "You need to select a course folder to update course references.",
          type = "error"
        )
      } else {
        if ("bibliogR" %in% base::as.data.frame(utils::installed.packages())$Package){
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = "Updating references..."
          )
          bibliogR::make_bib_file(
            source_folder = c(
              course_paths()$subfolders$original,
              course_paths()$subfolders$translated
            ),
            references = references(),
            destination_folder = base::paste0(
              course_paths()$subfolders$edit, "/data"
            ),
            file_name = "references.bib"
          )
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(
            title = "References updated!",
            text = "Your references are now up-to-date.",
            type = "success"
          )
        } else {
          shinyalert::shinyalert(
            title = "Missing package bibliogR!",
            text = "To work with references, you need to install the package bibliogR first.",
            type = "error"
          )
        }
      }
    })
    
    return(references)
  })
}

