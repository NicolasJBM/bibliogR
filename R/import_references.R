#' @name import_references
#' @title Import References in bibliogR
#' @author Nicolas Mangin
#' @description Gadget to import a list of references from Excel to R
#' @return An updated database of references in the bibliogR package folder.
#' @importFrom miniUI miniPage
#' @importFrom bslib bs_theme
#' @importFrom bslib font_google
#' @importFrom shiny tags
#' @importFrom shiny HTML
#' @importFrom miniUI gadgetTitleBar
#' @importFrom miniUI miniTabstripPanel
#' @importFrom miniUI miniTabPanel
#' @importFrom shiny icon
#' @importFrom miniUI miniContentPanel
#' @importFrom shiny fillRow
#' @importFrom shiny fileInput
#' @importFrom shiny actionButton
#' @importFrom shiny observeEvent
#' @importFrom shiny req
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom readxl read_excel
#' @importFrom tools resaveRdaFiles
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny paneViewer
#' @export


import_references <- function() {
  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- miniUI::miniPage(
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      base_font = bslib::font_google("Open Sans"),
      "enable-gradients" = FALSE,
      "enable-shadows" = TRUE,
      spacer = "0.5rem"
    ),

    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          ".shiny-notification {
                position:fixed;top: 30%;left: 0%;right: 0%;
             }"
        )
      )
    ),

    miniUI::gadgetTitleBar("Import references"),
    miniUI::miniTabstripPanel(

      # Panel where the author selects references in the filtered list
      miniUI::miniTabPanel(
        "Selection",
        icon = shiny::icon("list"),
        miniUI::miniContentPanel(
          shiny::fillRow(
            flex = c(4, 2),
            shiny::fileInput(
              "references", "Select on your drive the file references.xlsx:",
              accept = ".xlsx",
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
    shiny::observeEvent(input$import, {
      shiny::req(input$references)

      shiny::withProgress(message = "References", value = 0, {
        shiny::incProgress(1 / 4, detail = "Import database...")

        references <- readxl::read_excel(
          input$references$datapath[[1]],
          col_types = "text"
        )

        shiny::incProgress(1 / 4, detail = "Write database...")

        save(references, file = paste0(
          find.package("bibliogR"), "/references.RData"
        ))

        shiny::incProgress(1 / 4, detail = "Compress database...")

        tools::resaveRdaFiles(paste0(
          find.package("bibliogR"), "/references.RData"
        ))

        shiny::incProgress(1 / 4, detail = "Done!")

        shiny::showModal(shiny::modalDialog(
          title = "All your references are now imported!",
          "You can leave the application and start citing.",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}
