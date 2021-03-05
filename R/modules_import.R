#' @name modules_import
#' @title Module to import files
#' @param id    Character string. Id of the module
#' @param label Character string. Label on the interface
#' @description Module to import files
#' @importFrom shiny NS
#' @importFrom shiny fileInput
#' @importFrom shiny moduleServer
#' @importFrom shiny validate
#' @importFrom shiny need
#' @importFrom stringr str_extract
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @importFrom RefManageR ReadBib
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate_all
#' @importFrom shiny observe
#' @importFrom shiny reactive


import_ui <- function(id, label = "Select the file") {
  ns <- shiny::NS(id)
  shiny::fileInput(
    ns("file"),
    label,
    accept = c(".xlsx", ".bib", ".csv"),
    multiple = FALSE,
    width = "100%"
  )
}

import_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      file <- shiny::reactive({
        shiny::validate(shiny::need(
          input$file,
          message = "You need to select a file."
        ))
        input$file
      })

      filepath <- file()$datapath[[1]]

      filetype <- stringr::str_extract(file()$name[[1]], "\\.[a-z]{3,4}$")

      if (filetype == ".csv") {
        filecontent <- utils::read.csv(filepath, stringsAsFactors = FALSE)
      } else if (filetype == ".bib") {
        filecontent <- tibble::as_tibble(RefManageR::ReadBib(filepath))
      } else {
        filecontent <- readxl::read_excel(filepath, col_types = "text")
      }

      dplyr::mutate_all(filecontent, as.character)

      shiny::observe({
        msg <- sprintf("File %s was uploaded", file()$name)
        cat(msg, "\n")
      })

      return(filecontent)
    }
  )
}
