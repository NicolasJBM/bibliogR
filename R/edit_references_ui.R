#' @name edit_references_ui
#' @title Search and edit references
#' @author Nicolas Mangin
#' @description Module facilitating their edition on a case by case basis.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save edited list of references
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @export


edit_references_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::actionButton(
          ns("editref"), "Edit", icon = shiny::icon("pen-to-square"),
          style = "background-color:#000066;color:#FFF;width:100%;margin-bottom:10px;border:0px;",
          title = "Open a form to add a new reference or edit an existing one. Reload references after."
        )
      ),
      shiny::column(
        3,
        shiny::actionButton(
          ns("delref"), "Delete", icon = shiny::icon("trash-can"),
          style = "background-color:#660000;color:#FFF;width:100%;margin-bottom:10px;border:0px;",
          title = "Open a form to enter the keys of references which should be deleted."
        )
      ),
      shiny::column(
        3,
        shiny::actionButton(
          ns("loadref"), "Load", icon = shiny::icon("upload"),
          style = "background-color:#336666;color:#FFF;width:100%;border:0px;",
          title = "Load or reload references."
        )
      ),
      shiny::column(
        3,
        shiny::actionButton(
          ns("updateref"),
          "Update",
          icon = shiny::icon("rotate"),
          style = "background-color:#006699;color:#FFF;width:100%;border:0px;",
          title = "Update the .bib file to include all references cited in all the documents."
        )
      )
    )
  )
}

