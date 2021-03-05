#' @name cite_references
#' @title Insert Citations in Text
#' @author Nicolas Mangin
#' @description
#' Gadget for the selection and insertion of citations in Rmarkdown documents.
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
#' @importFrom shiny selectizeInput
#' @importFrom shiny updateSelectizeInput
#' @importFrom shiny sliderInput
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
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_all
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr rename
#' @importFrom dplyr count
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_lower
#' @importFrom furrr future_map_chr
#' @importFrom stats na.omit
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @importFrom DT JS
#' @importFrom glue glue
#' @importFrom utils head
#' @importFrom rstudioapi insertText
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 aes
#' @export


cite_references <- function() {

  ############################################################################
  # Interface
  ui <- miniUI::miniPage(

    # CSS formating
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      base_font = bslib::font_google("Open Sans"),
      "enable-gradients" = FALSE,
      "enable-shadows" = TRUE,
      spacer = "0.25rem"
    ),
    shiny::tags$head(
      shiny::tags$style(
        type = "text/css",
        "body {font-size: 0.75em;} "
      ),
      shiny::tags$style(
        shiny::HTML(
          ".shiny-notification {
            position:fixed;top: 30%;left: 0%;right: 0%;
          }"
        )
      )
    ),


    miniUI::gadgetTitleBar("Insert citations"),
    miniUI::miniTabstripPanel(

      # Search tab
      miniUI::miniTabPanel(
        "Search",
        icon = shiny::icon("search"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            flex = c(7, 1, 1, 2),
            shiny::fillRow(
              flex = c(3, 2),
              shiny::fillCol(
                flex = c(1, 1, 1, 1, 1),
                shiny::textInput(
                  "slcttitle",
                  "Has in title:",
                  value = "",
                  width = "100%"
                ),
                shiny::textInput(
                  "slctabstract",
                  "Has in abstract:",
                  value = "",
                  width = "100%"
                ),
                shiny::selectInput(
                  "slctfield",
                  "Select the field:",
                  choices = "",
                  selected = "",
                  multiple = FALSE,
                  width = "100%"
                ),
                shiny::selectInput(
                  "slctjournal",
                  "Select the journal:",
                  choices = "",
                  selected = "",
                  multiple = FALSE,
                  width = "100%"
                ),
                shiny::textInput(
                  "slctauthor",
                  "Authors:",
                  value = "",
                  width = "100%"
                )
              ),
              shiny::fillCol(
                flex = c(1, 8),
                shiny::htmlOutput("citecount"),
                shiny::plotOutput(
                  "fieldcount",
                  height = "100%",
                  width = "100%"
                )
              )
            ),
            shiny::sliderInput(
              "slctperiod",
              "Period:",
              min = 0,
              max = 3000,
              value = c(0, 3000),
              step = 1,
              width = "100%",
              sep = ""
            ),
            shiny::tags$br(),
            shiny::plotOutput(
              "yearcount",
              height = "100px"
            )
          )
        )
      ),

      # Selection tab
      miniUI::miniTabPanel(
        "Select",
        icon = shiny::icon("list"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            flex = c(2, 8),
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::uiOutput("selection", width = "100%")
              ),
              shiny::column(
                2,
                shiny::selectInput(
                  "format",
                  "Format:",
                  choices = c("create", "add", "subject"),
                  selected = "create",
                  multiple = FALSE
                )
              ),
              shiny::column(
                2,
                shiny::textInput("pages", "Pages:", value = "")
              ),
              shiny::column(
                2,
                shiny::actionButton(
                  "insert",
                  "Insert",
                  width = "100%",
                  shiny::icon("quote-right"),
                  style =
                    "background-color: #009933; color: #FFF; margin-top: 25px"
                )
              )
            ),
            DT::dataTableOutput(
              "reflist",
              width = "100%",
              height = "100%"
            )
          )
        )
      )
    )
  )


  ##############################################################################
  # Server
  server <- function(input, output, session) {

    # Bind variables for dplyr
    abstract <- NULL
    author <- NULL
    year <- NULL
    key <- NULL
    title <- NULL
    journal <- NULL
    keywords <- NULL
    issn <- NULL
    volume <- NULL
    number <- NULL
    references <- NULL
    n <- NULL
    field <- NULL

    # Load the local database of references
    shiny::withProgress(
      value = 1 / 4,
      message = "Loading references", {
        load(paste0(find.package("bibliogR"), "/references.RData"))

        shiny::incProgress(1 / 4, message = "Selecting information")

        references <- references %>%
          dplyr::select(
            key, title, author, year,
            field, journal, issn,
            volume, number,
            abstract, keywords
          )

        shiny::incProgress(1 / 4, message = "Initialize user input")

        fields <- unique(c("", stats::na.omit(references$field)))
        shiny::updateSelectInput(
          session, "slctfield",
          choices = fields,
          selected = ""
        )

        journals <- unique(c("", stats::na.omit(references$journal)))
        shiny::updateSelectInput(
          session, "slctjournal",
          choices = journals,
          selected = ""
        )

        minyear <- min(stats::na.omit(as.numeric(references$year)))
        maxyear <- max(stats::na.omit(as.numeric(references$year)))
        shiny::updateSliderInput(
          session, "slctperiod",
          min = minyear,
          max = maxyear,
          value = c(minyear, maxyear)
        )

        shiny::incProgress(1 / 4, message = "Done!")
      }
    )

    # Apply filters
    after_title_selection <- reactive({
      references %>%
        filter_references(
          variable = "title",
          filter_value = input$slcttitle,
          filter_type = "pattern"
        )
    })

    after_abstract_selection <- reactive({
      after_title_selection() %>%
        filter_references(
          variable = "abstract",
          filter_value = input$slctabstract,
          filter_type = "pattern"
        )
    })

    after_field_selection <- reactive({
      after_abstract_selection() %>%
        filter_references(
          variable = "field",
          filter_value = input$slctfield,
          filter_type = "selection"
        )
    })

    after_journal_selection <- reactive({
      after_field_selection() %>%
        filter_references(
          variable = "journal",
          filter_value = input$slctjournal,
          filter_type = "selection"
        )
    })

    after_author_selection <- reactive({
      after_journal_selection() %>%
        filter_references(
          variable = "author",
          filter_value = input$slctauthor,
          filter_type = "pattern"
        )
    })

    after_period_selection <- reactive({
      after_author_selection() %>%
        filter_references(
          variable = "year",
          filter_value = input$slctperiod,
          filter_type = "range"
        )
    })

    # Update filters
    shiny::observe({

      fields <- unique(c("", stats::na.omit(after_author_selection()$field)))
      if (input$slctfield %in% fields) {
        tmpfield <- input$slctfield
      } else {
        tmpfield <- ""
      }
      shiny::updateSelectInput(
        session, "slctfield",
        choices = fields,
        selected = tmpfield
      )

      journals <- unique(c("", stats::na.omit(
        after_author_selection()$journal)))
      if (input$slctjournal %in% journals) {
        tmpjournal <- input$slctjournal
      } else {
        tmpjournal <- ""
      }
      shiny::updateSelectInput(
        session, "slctjournal",
        choices = journals,
        selected = tmpjournal
      )

      minyear <- min(stats::na.omit(
        as.numeric(after_author_selection()$year)))
      if (input$slctperiod[1] >= minyear) {
        tmpminyear <- input$slctperiod[1]
      } else {
        tmpminyear <- minyear
      }
      maxyear <- max(stats::na.omit(
        as.numeric(after_author_selection()$year)))
      if (input$slctperiod[2] <= maxyear) {
        tmpmaxyear <- input$slctperiod[2]
      } else {
        tmpmaxyear <- maxyear
      }
      shiny::updateSliderInput(
        session, "slctperiod",
        min = minyear,
        max = maxyear,
        value = c(tmpminyear, tmpmaxyear)
      )
    })

    # Count the number of references filtered
    output$citecount <- shiny::renderUI({
      shiny::HTML(paste0(
        "<center>Number of references: ",
        nrow(after_period_selection()),
        "</center>"
      ))
    })

    # Display the distribution of papers across fields or journals
    output$fieldcount <- shiny::renderPlot({
      if (is.null(input$slctfield)) {
        fccond <- TRUE
      } else {
        fccond <- (is.na(input$slctfield) | input$slctfield == "")
      }
      if (fccond) {
        baseplot <- after_period_selection() %>%
          dplyr::count(field) %>%
          stats::na.omit()
        if (nrow(baseplot) > 0) {
          set_levels <- baseplot$field[order(baseplot$n, decreasing = FALSE)]
          baseplot %>%
            dplyr::mutate(field = factor(field, levels = set_levels)) %>%
            ggplot2::ggplot(ggplot2::aes(x = field, y = n)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::theme_minimal()
        }
      } else {
        baseplot <- after_period_selection() %>%
          dplyr::filter(nchar(journal) > 3) %>%
          dplyr::count(journal) %>%
          stats::na.omit()
        if (nrow(baseplot) > 0) {
          set_levels <- baseplot$journal[order(baseplot$n, decreasing = FALSE)]
          baseplot %>%
            dplyr::mutate(journal = factor(journal, levels = set_levels)) %>%
            ggplot2::ggplot(ggplot2::aes(x = journal, y = n)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::theme_minimal()
        }
      }
    })

    # Display the distribution of papers across years
    output$yearcount <- renderPlot({
      baseplot <- after_period_selection() %>%
        dplyr::count(year) %>%
        stats::na.omit() %>%
        dplyr::mutate(year = as.numeric(year))
      if (nrow(baseplot) > 0) {
        baseplot %>%
          ggplot2::ggplot(ggplot2::aes(x = year, y = n)) +
          ggplot2::geom_col() +
          ggplot2::theme_minimal()
      }
    })

    output$reflist <- DT::renderDataTable({
      if (nrow(after_period_selection()) <= 250) {
        reflist <- after_period_selection() %>%
          dplyr::select(-field)
      } else {
        reflist <- data.frame(
          key = "Please", title = "refine your search",
          author = NA, year = NA, journal = NA, issn = NA,
          volume = NA, number = NA, abstract = NA, keywords = NA
        )
      }

      withchildrow(
        x = reflist,
        vars = c(
          "author", "year", "journal", "issn",
          "volume", "number", "abstract", "keywords"
        ),
        opts = list(pageLength = 50)
      )
    })

    # Selection of references
    output$selection <- shiny::renderUI({
      if (nrow(after_period_selection()) <= 250) {
        reflist <- after_period_selection() %>%
          dplyr::select(-field)
      } else {
        reflist <- data.frame(
          key = "Please,", title = "refine your search",
          author = NA, year = NA, journal = NA, issn = NA,
          volume = NA, number = NA, abstract = NA, keywords = NA
        )
      }

      shiny::selectInput(
        "selection",
        "Selection",
        choices = reflist$key,
        multiple = T,
        width = "100%"
      )
    })

    # Format and insert the citations
    shiny::observeEvent(input$insert, {
      citations <- format_citations(
        citations = input$selection,
        pages = input$pages
      )
      rstudioapi::insertText(citations)
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }
  shiny::runGadget(
    ui,
    server,
    viewer = shiny::paneViewer(minHeight = "maximize")
  )
}
