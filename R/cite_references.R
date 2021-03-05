#' @name cite_references
#' @title Insert Citations in Text
#' @author Nicolas Mangin
#' @description
#' Gadget for the selection and insertion of citations in Rmarkdown documents.
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
#' @importFrom shiny fillCol
#' @importFrom shiny fillRow
#' @importFrom shiny textInput
#' @importFrom shiny selectInput
#' @importFrom shiny htmlOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny sliderInput
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny uiOutput
#' @importFrom shiny actionButton
#' @importFrom DT dataTableOutput
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom dplyr select
#' @importFrom stats na.omit
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateSliderInput
#' @importFrom shiny observe
#' @importFrom shiny renderUI
#' @importFrom shiny renderPlot
#' @importFrom dplyr %>%
#' @importFrom dplyr all_of
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 theme_minimal
#' @importFrom DT renderDataTable
#' @importFrom shiny observeEvent
#' @importFrom rstudioapi insertText
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny paneViewer
#' @importFrom shiny reactive
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
    after_title_selection <- shiny::reactive({
      references %>%
        filter_references(
          variable = "title",
          filter_value = input$slcttitle,
          filter_type = "pattern"
        )
    })

    after_abstract_selection <- shiny::reactive({
      after_title_selection() %>%
        filter_references(
          variable = "abstract",
          filter_value = input$slctabstract,
          filter_type = "pattern"
        )
    })

    after_field_selection <- shiny::reactive({
      after_abstract_selection() %>%
        filter_references(
          variable = "field",
          filter_value = input$slctfield,
          filter_type = "selection"
        )
    })

    after_journal_selection <- shiny::reactive({
      after_field_selection() %>%
        filter_references(
          variable = "journal",
          filter_value = input$slctjournal,
          filter_type = "selection"
        )
    })

    after_author_selection <- shiny::reactive({
      after_journal_selection() %>%
        filter_references(
          variable = "author",
          filter_value = input$slctauthor,
          filter_type = "pattern"
        )
    })

    after_period_selection <- shiny::reactive({
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
        after_author_selection()$journal
      )))
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
        as.numeric(after_author_selection()$year)
      ))
      maxyear <- max(stats::na.omit(
        as.numeric(after_author_selection()$year)
      ))
      shiny::updateSliderInput(
        session, "slctperiod",
        min = minyear,
        max = maxyear,
        value = c(minyear, maxyear)
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
      if (length(unique(after_period_selection())) > 1) {
        level <- "field"
      } else {
        level <- "journal"
      }

      baseplot <- after_period_selection() %>%
        dplyr::select(level = dplyr::all_of(level)) %>%
        dplyr::count(level) %>%
        stats::na.omit()

      if (nrow(baseplot) > 0) {
        set_levels <- baseplot$level[order(baseplot$n, decreasing = FALSE)]
        baseplot %>%
          dplyr::mutate(level = factor(level, levels = set_levels)) %>%
          ggplot2::ggplot(ggplot2::aes(x = level, y = n)) +
          ggplot2::geom_col() +
          ggplot2::xlab(level) +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal()
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
