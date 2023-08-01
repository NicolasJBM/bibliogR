#' @name manage_references_ui
#' @title Batch import new references or delete duplicates.
#' @author Nicolas Mangin
#' @description Module allowing the user to import multiple references from a file or to identify and remove duplicated references.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save edited list of references
#' @importFrom lubridate year
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fileInput
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny selectInput
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinyWidgets dropdownButton
#' @importFrom shinyWidgets tooltipOptions
#' @export


manage_references_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        1,
        shinyWidgets::dropdownButton(
          shiny::tags$h3("Importation of new references"),
          shiny::fileInput(
            ns("filestoimport"), "Browse for the files to import.",
            multiple = TRUE, accept = c(".csv",".xlsx"), width = "100%"
          ),
          shiny::tags$hr(),
          shiny::actionButton(
            ns("importfiles"), "Import", icon = shiny::icon("upload"),
            style = "background-color:#003366;color:#FFF;width:100%;margin-bottom:10px;",
            title = "Download files containing references to add to the reference list."
          ),
          circle = TRUE, status = "success",
          icon = shiny::icon("upload"), width = "300px",
          tooltip = shinyWidgets::tooltipOptions(title = "Click to import new references.")
        ),
        shiny::tags$hr(),
        shinyWidgets::dropdownButton(
          shiny::tags$h3("Removal of duplicated references"),
          shiny::numericInput(ns("minyear"), "Minimum year:", value = lubridate::year(base::Sys.Date())-1),
          shiny::numericInput(ns("maxyear"), "Maximum year:", value = lubridate::year(base::Sys.Date())),
          shiny::selectInput(
            ns("distmethod"), "Kind of distance:",
            choices = c("osa","lv","dl","hamming","lcs","qgram","cosine","jaccard","jw","soundex"),
            selected = "qgram"
          ),
          shiny::numericInput(ns("maxdist"), "Threshold:", value = 10),
          shiny::tags$hr(),
          shiny::actionButton(
            ns("searchduplicates"), "Search", icon = shiny::icon("magnifying-glass"),
            style = "background-color:#660033;color:#FFF;width:100%;margin-bottom:10px;",
            title = "Search for potential duplicates within the specified parameters."
          ),
          circle = TRUE, status = "danger",
          icon = shiny::icon("list-check"), width = "300px",
          tooltip = shinyWidgets::tooltipOptions(title = "Click to remove duplicates")
        ),
        shiny::tags$hr(),
        shinyWidgets::actionBttn(
          ns("import_remove"),
          style = "material-circle", 
          color = "primary",
          icon = shiny::icon("cogs"),
          title = "Import or remove references."
        )
      ),
      shiny::column(
        11,
        rhandsontable::rHandsontableOutput(ns("importations_duplications"))
      ),
      
    )
  )
}

