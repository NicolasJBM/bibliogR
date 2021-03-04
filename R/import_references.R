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
#' @importFrom bslib bs_theme
#' @importFrom bslib font_google
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
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom readxl read_excel
#' @export


import_references <- function() {
  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- miniPage(
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      base_font = bslib::font_google("Open Sans"),
      "enable-gradients" = FALSE,
      "enable-shadows" = TRUE,
      spacer = "0.5rem"
    ),

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
          fillRow(
            flex = c(4, 2),
            shiny::fileInput(
              "references", "Select on your drive the file references.xlsx:",
              accept = c(".xlsx"),
              multiple = FALSE,
              width = "100%"
            ),
            shiny::actionButton(
              "import",
              "Import",
              width = "100%",
              icon("upload"),
              style =
                "background-color: #009933; color: #FFF; margin-top: 30px"
            )
          )
        )
      )
    )
  )



  server <- function(input, output, session) {
    observeEvent(input$import, {
      shiny::req(input$references)

      withProgress(message = "References", value = 0, {
        incProgress(1 / 4, detail = "Import database...")
        references <- readxl::read_excel(
          input$references$datapath[[1]],
          col_types = "text"
        )

        incProgress(1 / 4, detail = "Write database...")
        save(references, file = paste0(
          find.package("bibliogR"), "/references.RData"
        ))

        incProgress(1 / 4, detail = "Compress database...")
        tools::resaveRdaFiles(paste0(
          find.package("bibliogR"), "/references.RData"
        ))

        incProgress(1 / 4, detail = "Done!")
        showModal(modalDialog(
          title = "All your references are now imported!",
          "You can leave the application and start citing.",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    })

    observeEvent(input$done, {
      stopApp()
    })
  }

  runGadget(ui, server, viewer = paneViewer())
}
