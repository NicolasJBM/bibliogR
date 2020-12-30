#' @name import_references
#' @title Import References in bibliogR
#' @author Nicolas Mangin
#' @description Gadget to import a list of references from Excel to R
#' @return An updated database of references in the bibliogR package folder.
#' @importFrom miniUI miniPage
#' @importFrom miniUI gadgetTitleBar
#' @importFrom miniUI miniTabstripPanel
#' @importFrom miniUI miniTabPanel
#' @importFrom miniUI miniContentPanel
#' @importFrom shinythemes shinytheme
#' @importFrom shiny fillCol
#' @importFrom shiny fillRow
#' @importFrom shiny icon
#' @importFrom shiny fileInput
#' @importFrom shiny textInput
#' @importFrom shiny dateInput
#' @importFrom shiny numericInput
#' @importFrom shiny textAreaInput
#' @importFrom shiny selectInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tags
#' @importFrom shiny htmlOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny textOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderUI
#' @importFrom shiny renderPlot
#' @importFrom shiny renderText
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny h3
#' @importFrom shiny isolate
#' @importFrom shiny reactiveValuesToList
#' @importFrom shiny tableOutput
#' @importFrom shiny renderTable
#' @importFrom shiny HTML
#' @importFrom shiny validate
#' @importFrom shiny need
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny eventReactive
#' @importFrom shiny dialogViewer
#' @importFrom shiny paneViewer
#' @importFrom readxl read_excel
#' @export


import_references <- function() {
  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- miniPage(
    theme = shinythemes::shinytheme("flatly"),
    tags$head(tags$style(
      HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
    )),

    gadgetTitleBar("Import references"),
    miniTabstripPanel(

      # Panel where the author selects references in the filtered list
      miniTabPanel(
        "Selection",
        icon = icon("list"),
        miniContentPanel(
          shiny::fileInput(
            "references", "Select on your drive the file references.xlsx:",
            accept = c(".xlsx"),
            multiple = FALSE
          )
        )
      )
    )
  )



  server <- function(input, output, session) {
    observeEvent(input$done, {
      withProgress(message = "References", value = 0, {
        incProgress(0 / 3, detail = "Import database...")
        if (!is.null(input$references)) {
          references <- readxl::read_excel(
            input$references$datapath[[1]],
            col_types = "text"
          )
        } else {
          load(paste0(find.package("bibliogR"), "/references.RData"))
        }

        incProgress(1 / 3, detail = "Write database...")
        save(references, file = paste0(
          find.package("bibliogR"), "/references.RData"
        ))

        incProgress(1 / 3, detail = "Compress database...")
        tools::resaveRdaFiles(paste0(
          find.package("bibliogR"), "/references.RData"
        ))
      })

      stopApp()
    })
  }

  runGadget(ui, server, viewer = paneViewer())
}
