#' @name cite_references
#' @title Insert Citations in Text
#' @author Nicolas Mangin
#' @description Gadget for the selection and insertion of citations in Rmarkdown documents.
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
  ui <- miniPage(
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      base_font = bslib::font_google("Open Sans"),
      "enable-gradients" = FALSE,
      "enable-shadows" = TRUE,
      spacer = "0.25rem"
    ),

    tags$head(
      tags$style(
        type = "text/css",
        "body {font-size: 0.75em;} "
      ),
      tags$style(
        HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
      )
    ),

    gadgetTitleBar("Insert citations"),
    miniTabstripPanel(

      # Panel where the author selects references in the filtered list
      miniTabPanel(
        "Search",
        icon = icon("search"),
        miniContentPanel(
          fillCol(
            flex = c(7, 1, 1, 2),
            fillRow(
              flex = c(3, 2),
              fillCol(
                flex = c(1, 1, 1, 1, 1),
                uiOutput("filttitle"),
                uiOutput("filtabstract"),
                uiOutput("filtfield"),
                uiOutput("filtjournal"),
                selectizeInput(
                  "slctauthor",
                  "Authors:",
                  choices = NULL,
                  multiple = TRUE,
                  width = "100%"
                )
              ),
              fillCol(
                flex = c(1, 8),
                htmlOutput("citecount"),
                plotOutput("fieldcount", height = "100%", width = "100%")
              )
            ),
            uiOutput("filtperiod"),
            tags$br(),
            plotOutput("yearcount", height = "100px")
          )
        )
      ),

      # Panel where the author checks references in the filtered list
      miniTabPanel(
        "Select",
        icon = icon("list"),
        miniContentPanel(
          fillCol(
            flex = c(2, 8),
            fluidRow(
              column(6, uiOutput("selection", width = "100%")),
              column(2, selectInput(
                "format",
                "Format:",
                choices = c("create", "add", "subject"),
                selected = "create",
                multiple = FALSE
              )),
              column(2, textInput("pages", "Pages:", value = "")),
              column(
                2,
                actionButton(
                  "insert", "Insert",
                  width = "100%",
                  icon("quote-right"),
                  style =
                    "background-color: #009933; color: #FFF; margin-top: 25px"
                )
              )
            ),
            DT::dataTableOutput("reflist", width = "100%", height = "100%")
          )
        )
      )
    )
  )



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
    desc <- NULL
    references <- NULL
    n <- NULL
    field <- NULL

    # Load the local database of references
    # (imported with function import_references)
    load(paste0(find.package("bibliogR"), "/references.RData"))
    observe({
      authors <- references$author %>%
        str_split(" ") %>%
        unlist() %>%
        setdiff("and") %>%
        str_remove_all(",") %>%
        str_remove_all("\\.") %>%
        unique() %>%
        sort() %>%
        stringr::str_remove_all("[-0-9]")

      authors <- authors[nchar(authors) > 1]

      updateSelectizeInput(
        session,
        "slctauthor",
        choices = authors,
        server = TRUE
      )
    })

    # Prepare reactive values
    values <- reactiveValues()

    withProgress(
      message = "Retrieve the database of references",
      detail = "This may take a while...", {
        values$references <- references
        incProgress(1 / 3)
        values$years <- c(
          min(na.omit(as.numeric(references$year))),
          max(na.omit(as.numeric(references$year)))
        )
        incProgress(1 / 3)
      }
    )

    # Prepare filters

    ############################################################################
    # The first three filters follow distinct patterns
    # Key
    afterfiltkey <- reactive({
      filter <- input$slctkey
      if (is.null(filter)) {
        values$references
      } else if (filter == "") {
        values$references
      } else {
        dplyr::filter(values$references, key == filter)
      }
    })

    # Authors
    afterfiltauthors <- reactive({
      if (is.null(input$slctauthor)) {
        filter <- NULL
      } else {
        filter <- paste0("(^|\\s)", input$slctauthor, "(,|\\s)")
      }
      if (is.null(filter)) {
        afterfiltkey()
      } else if (filter[[1]] == "") {
        afterfiltkey()
      } else {
        authors <- stringr::str_to_lower(filter)
        base <- afterfiltkey()
        for (i in seq_len(length(authors))) {
          base <- dplyr::filter(
            base,
            str_detect(stringr::str_to_lower(base$author), authors[i])
          )
        }
        base
      }
    })

    # Period
    output$filtperiod <- renderUI({
      sliderInput(
        "slctperiod",
        "Period:",
        min = values$years[1],
        max = values$years[2],
        value = c(values$years[1], values$years[2]),
        step = 1,
        width = "100%",
        sep = ""
      )
    })
    afterfiltperiod <- reactive({
      if (!is.null(input$slctperiod)) {
        afterfiltauthors() %>%
          mutate(year = as.numeric(year)) %>%
          dplyr::filter(
            year >= input$slctperiod[1],
            year <= input$slctperiod[2]
          ) %>%
          mutate(year = as.character(year))
      } else {
        afterfiltauthors()
      }
    })
    ############################################################################


    # Field
    output$filtfield <- renderUI({
      make_filter(
        dataset = afterfiltperiod(), variable = "field",
        id = "slctfield", label = "Field:"
      )
    })
    afterfiltfield <- reactive({
      filter_data(
        dataset = afterfiltperiod(), variable = "field",
        filt = input$slctfield, type = "selection"
      )
    })

    # Journal
    output$filtjournal <- renderUI({
      make_filter(
        dataset = afterfiltfield(), variable = "journal",
        id = "slctjournal", label = "Journal:"
      )
    })
    afterfiltjournal <- reactive({
      filter_data(
        dataset = afterfiltfield(), variable = "journal",
        filt = input$slctjournal, type = "selection"
      )
    })

    # Title
    output$filttitle <- renderUI({
      textInput("slcttitle", "In title:", value = "", width = "100%")
    })

    afterfilttitle <- reactive({
      filter_data(
        dataset = afterfiltjournal(), variable = "title",
        filt = input$slcttitle, type = "text"
      )
    })

    # Abstract
    output$filtabstract <- renderUI({
      textInput("slctabstract", "In abstract:", value = "", width = "100%")
    })

    afterfiltabstract <- reactive({
      filter_data(
        dataset = afterfilttitle(), variable = "abstract",
        filt = input$slctabstract, type = "text"
      )
    })

    # keywords
    output$slctkeyword <- renderUI({
      textInput("slctkeyword", "In keywords:", value = "", width = "100%")
    })

    afterfiltkeyword <- reactive({
      filter_data(
        dataset = afterfiltabstract(), variable = "keywords",
        filt = input$slctkeyword, type = "text"
      )
    })


    ############################################################################
    # Apply filters
    filtered <- reactive({
      afterfiltkeyword() %>%
        dplyr::select(
          key, title, author, year, field, journal, issn,
          volume, number, abstract, keywords
        ) %>%
        dplyr::arrange(desc(year), author)
    })


    # Count the number of references filtered
    output$citecount <- renderUI({
      HTML(paste0(
        "<center>Number of references: ",
        nrow(filtered()),
        "</center>"
      ))
    })

    output$fieldcount <- renderPlot({
      if (is.null(input$slctfield)) {
        fccond <- TRUE
      } else {
        fccond <- (is.na(input$slctfield) | input$slctfield == "")
      }
      if (fccond) {
        baseplot <- filtered() %>%
          dplyr::filter(!(field %in% c("Field", "Other"))) %>%
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
        baseplot <- filtered() %>%
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

    output$yearcount <- renderPlot({
      baseplot <- filtered() %>%
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
      if (nrow(filtered()) <= 250) {
        reflist <- filtered() %>%
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
    output$selection <- renderUI({
      if (nrow(filtered()) <= 250) {
        reflist <- filtered() %>%
          dplyr::select(-field)
      } else {
        reflist <- data.frame(
          key = "Please", title = "refine your search",
          author = NA, year = NA, journal = NA, issn = NA,
          volume = NA, number = NA, abstract = NA, keywords = NA
        )
      }

      selectInput(
        "selection",
        "Selection",
        choices = reflist$key,
        multiple = T,
        width = "100%"
      )
    })

    # Cite
    observeEvent(input$insert, {
      pg <- case_when(
        str_detect(input$pages, "-") ~ "pp.",
        TRUE ~ "p."
      )
      citations <- case_when(
        length(input$selection) == 1 &
          input$format == "create" &
          input$pages == "" ~
        paste0("[@", input$selection[[1]], "]"),
        length(input$selection) == 1 &
          input$format == "create" &
          input$pages != "" ~
        paste0("[@", input$selection[[1]], ", ", pg, input$pages, "]"),
        length(input$selection) >= 2 &
          input$format == "create" ~
        paste0("[@", paste(input$selection, collapse = "; @"), "]"),
        length(input$selection) == 1 &
          input$format == "add" &
          input$pages == "" ~
        paste0("; @", input$selection[[1]]),
        length(input$selection) == 1 &
          input$format == "add" &
          input$pages != "" ~
        paste0("; @", input$selection[[1]], ", ", pg, input$pages),
        length(input$selection) >= 1 &
          input$format == "add" ~
        paste0("; @", paste(input$selection, collapse = "; @")),
        TRUE ~ paste0("@", input$selection[[1]])
      )
      rstudioapi::insertText(citations)
    })


    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}





# Function to generate dynamic filters in user interface
make_filter <- function(dataset, variable, id, label) {
  choices <- sort(
    as.character(unique(c(unlist(dataset[, variable]), ""))),
    decreasing = FALSE
  )
  selectInput(
    id,
    label,
    choices = choices,
    selected = NULL,
    multiple = FALSE,
    width = "100%"
  )
}


# Functions to apply dynamically filters
filter_data <- function(dataset, variable, filt, type) {
  if (is.null(filt)) {
    dataset
  } else if (filt == "") {
    dataset
  } else {
    if (type == "selection") {
      dplyr::filter(dataset, str_detect(unlist(dataset[, variable]), filt))
    } else {
      terms <- stringr::str_to_lower(unlist(str_split(filt, " ")))
      terms <- stringr::str_replace_all(terms, "_", " ")
      base <- dataset
      for (term in terms) {
        base <- dplyr::filter(
          base,
          str_detect(stringr::str_to_lower(unlist(base[, variable])), term)
        )
      }
      base
    }
  }
}


# Collapsible entries in reference list
# source: http://www.reigo.eu/2018/04/extending-dt-child-row-example/
withchildrow <- function(x, vars = NULL, opts = NULL, ...) {
  names_x <- names(x)
  if (is.null(vars)) stop("'vars' must be specified!")
  pos <- match(vars, names_x)
  if (any(furrr::future_map_chr(x[, pos], typeof) == "list")) {
    stop("list columns are not supported in datatable2()")
  }

  pos <- pos[pos <= ncol(x)] + 1
  rownames(x) <- NULL
  if (nrow(x) > 0) x <- cbind(" " = "&oplus;", x)

  # options
  opts <- c(
    opts,
    list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0, pos)),
        list(orderable = FALSE, className = "details-control", targets = 1),
        list(className = "dt-left", targets = 1:3),
        list(className = "dt-right", targets = 4:ncol(x))
      )
    )
  )

  DT::datatable(
    x,
    ...,
    escape = -2,
    options = opts,
    callback = JS(.callback2(x = x, pos = c(0, pos)))
  )
}

.callback2 <- function(x, pos = NULL) {
  part1 <- "table.column(1).nodes().to$().css({cursor: 'pointer'});"

  part2 <- .child_row_table2(x, pos = pos)

  part3 <-
    "
   table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&ominus;');
    }
  });"

  paste(part1, part2, part3)
}

.child_row_table2 <- function(x, pos = NULL) {
  names_x <- paste0(names(x), ":")
  text <- "
  var format = function(d) {
    text = '<div><table >' +
  "

  for (i in seq_along(pos)) {
    text <- paste(text, glue::glue(
      "'<tr>' +
          '<td>' + '{names_x[pos[i]]}' + '</td>' +
          '<td>' + d[{pos[i]}] + '</td>' +
        '</tr>' + "
    ))
  }

  paste0(
    text,
    "'</table></div>'
      return text;};"
  )
}
