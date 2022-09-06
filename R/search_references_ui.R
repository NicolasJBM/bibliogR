#' @name search_references_ui
#' @title Filter a list of references
#' @author Nicolas Mangin
#' @description Mudule allowing the user to find references using various filters.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Filter list of references
#' @importFrom DT dataTableOutput
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shinyWidgets searchInput
#' @export


search_references_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        shinyWidgets::searchInput(
          inputId = ns("inrefkey"),
          label = "Key:",
          width = "100%",
          placeholder = "Pattern to search",
          btnSearch = shiny::icon("magnifying-glass"),
          btnReset = shiny::icon("eraser")
        ),
        shinyWidgets::searchInput(
          inputId = ns("inrefauthors"),
          label = "Authors:",
          width = "100%",
          placeholder = "Pattern to search",
          btnSearch = shiny::icon("magnifying-glass"),
          btnReset = shiny::icon("eraser")
        ),
        shiny::sliderInput(
          ns("slctrefperiod"),
          "Period:",
          min = 0,
          max = 2021,
          value = c(0,2021),
          sep = "",
          width = "100%"
        ),
        shinyWidgets::searchInput(
          inputId = ns("inreftitle"),
          label = "Title:",
          width = "100%",
          placeholder = "Pattern to search",
          btnSearch = shiny::icon("magnifying-glass"),
          btnReset = shiny::icon("eraser")
        ),
        shinyWidgets::searchInput(
          inputId = ns("inrefabstract"),
          label = "Abstract:",
          width = "100%",
          placeholder = "Pattern to search",
          btnSearch = shiny::icon("magnifying-glass"),
          btnReset = shiny::icon("eraser")
        ),
        shiny::selectInput(
          ns("slctreffield"),
          "Field:",
          choices = "",
          selected = "",
          width = "100%"
        ),
        shiny::selectInput(
          ns("slctrefjournal"),
          "Journal:",
          choices = "",
          selected = "",
          width = "100%"
        )
      ),
      shiny::column(
        8,
        DT::dataTableOutput(ns("reflist"))#,
        #shiny::tags$head(
        #  shiny::tags$style(
        #    "#filtref-reflist table .odd {
        #    background-color: #345;
        #    color: #FFF;
        #  }
          #filtref-reflist table .even {
        #    background-color: #222d32;
        #    color: #FFF;
        #  }
        #  ",
        #  media="screen",
        #  type="text/css"
        #  )
        #)
      )
    )
  )
}

