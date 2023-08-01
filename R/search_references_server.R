#' @name search_references_server
#' @title Filter a list of references
#' @author Nicolas Mangin
#' @description Module allowing the user to find references using various filters.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param references Reactive. Function containing a list of references.
#' @return Filtered list of references
#' @importFrom DT renderDataTable
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom shiny NS
#' @importFrom shiny moduleServer
#' @importFrom shiny reactive
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateSliderInput
#' @export



search_references_server <- function(id, references){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    year <- NULL
    
    # Key
    after_key_filter <- shiny::reactive({
      shiny::req(base::length(references()) > 1)
      bibliogR::filter_references(
        dataset = references(),
        variable = "key",
        filter_value = input$inrefkey,
        filter_type = "pattern"
      )
    })
    
    
    # Authors
    after_authors_filter <- shiny::reactive({
      shiny::req(base::length(after_key_filter()) > 1)
      selection <- bibliogR::filter_references(
        dataset = after_key_filter(),
        variable = "author",
        filter_value = input$inrefauthors,
        filter_type = "pattern"
      )
      if (base::nrow(selection) > 0){
        minimum <- base::min(selection$year)
        maximum <- base::max(selection$year)
      } else {
        minimum <- 0
        maximum <- 2021
      }
      shiny::updateSliderInput(
        session,
        "slctrefperiod",
        min = minimum,
        max = maximum,
        value = c(minimum, maximum)
      )
      selection
    })
    
    # Period
    after_period_filter <- shiny::reactive({
      shiny::req(base::length(after_authors_filter()) > 1)
      selection <- bibliogR::filter_references(
        dataset = after_authors_filter(),
        variable = "year",
        filter_value = input$slctrefperiod,
        filter_type = "range"
      )
      if (base::nrow(selection) <= 200){
        shiny::updateSelectInput(
          session,
          "slctrefkey",
          choices = c("", base::unique(selection$key))
        )
      }
      selection
    })
    
    
    # Title
    after_title_filter <- shiny::reactive({
      shiny::req(base::length(after_period_filter()) > 1)
      bibliogR::filter_references(
        dataset = after_period_filter(),
        variable = "title",
        filter_value = input$inreftitle,
        filter_type = "pattern"
      )
    })
    
    # Abstract
    after_abstract_filter <- shiny::reactive({
      shiny::req(base::length(after_title_filter()) > 1)
      selection <- bibliogR::filter_references(
        dataset = after_title_filter(),
        variable = "abstract",
        filter_value = input$inrefabstract,
        filter_type = "pattern"
      )
      shiny::updateSelectInput(
        session,
        "slctreffield",
        choices = c("", base::unique(selection$field))
      )
      selection
    })
    
    # Field
    after_field_filter <- shiny::reactive({
      shiny::req(base::length(after_abstract_filter()) > 1)
      selection <- bibliogR::filter_references(
        dataset = after_abstract_filter(),
        variable = "field",
        filter_value = input$slctreffield,
        filter_type = "selection"
      )
      shiny::updateSelectInput(
        session,
        "slctrefjournal",
        choices = c("", base::unique(selection$journal))
      )
      selection
    })
    
    # Journal
    after_journal_filter <- shiny::reactive({
      shiny::req(base::length(after_field_filter()) > 1)
      bibliogR::filter_references(
        dataset = after_field_filter(),
        variable = "journal",
        filter_value = input$slctrefjournal,
        filter_type = "selection"
      )
    })
    
    # Selection
    output$reflist <- DT::renderDataTable({
      shiny::req(base::length(after_journal_filter()) > 1)
      selected_columns <- c(
        "key", "title", "author", "year", "journal", "abstract"
      )
      if (base::nrow(after_journal_filter()) <= 250) {
        reflist <- after_journal_filter() |>
          dplyr::arrange(dplyr::desc(year)) |>
          dplyr::select(dplyr::all_of(selected_columns))
      } else {
        reflist <- base::data.frame(
          key = "Too many references:", title = "Please refine your search.",
          author = NA, year = NA, journal = NA, abstract = NA
        )
      }
      selected_references <- bibliogR::with_childrow(
        x = dplyr::mutate_all(reflist, base::as.character),
        vars = c(
          "author", "year", "journal", "abstract"
        ),
        opts = base::list(pageLength = 10)
      )
      selected_references
    })
    
    return(after_journal_filter)
  })
}

