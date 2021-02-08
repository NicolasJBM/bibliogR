#' @name combine_references
#' @title Import References and Combine Them
#' @author Nicolas Mangin
#' @description Application gathering references from different sources, identifying potential duplicates, and combining them in a signle reference file.
#' @return An Excel file with the new complete reference list.
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
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr everything
#' @importFrom furrr future_map
#' @importFrom furrr future_map_chr
#' @importFrom furrr future_pmap
#' @importFrom stringr str_extract
#' @importFrom readxl read_excel
#' @importFrom WriteXLS WriteXLS
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_context_menu
#' @importFrom tibble rownames_to_column
#' @export

combine_references <- function() {
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

    gadgetTitleBar("Combine References"),
    miniTabstripPanel(

      # Panel where the author selects references in the filtered list
      miniTabPanel(
        "Import",
        icon = icon("file-upload"),
        miniContentPanel(
          shiny::fileInput(
            "references",
            "Select the initial list of references:",
            accept = c(".xlsx"),
            multiple = FALSE
          ),

          shiny::fileInput(
            "files",
            "Select the files containing the new references:",
            accept = c(".bib"),
            multiple = TRUE
          )
        )
      ),
      miniTabPanel(
        "Select",
        icon = icon("check"),
        miniContentPanel(
          rhandsontable::rHandsontableOutput("display_matches")
        )
      )
    )
  )


  server <- function(input, output, session) {

    # Bind variables for dplyr
    datapath <- NULL
    journal <- NULL
    tmpkey <- NULL
    year <- NULL
    author <- NULL
    title <- NULL
    import <- NULL
    key <- NULL
    disttitle <- NULL
    distauth <- NULL

    # Import the files to be combined
    references <- reactive({
      if (!is.null(input$references)) {
        readxl::read_excel(input$references$datapath[[1]], col_types = "text")
      }
    })

    new_references <- reactive({
      if (!is.null(input$files)) {
        input$files %>%
          dplyr::select(file = datapath) %>%
          dplyr::mutate(import = furrr::future_map(
            file, bibliogR::get_new_references
          )) %>%
          tidyr::unnest(import) %>%
          dplyr::select(-file) %>%
          tibble::rownames_to_column("tmpkey")
      }
    })

    # Flag potential duplicates
    matches <- reactive({
      withProgress(
        message = "Find potential matches",
        detail = "This may take a while...", {
          incProgress(amount = 0, message = "Import and simplify;")

          future::plan("multisession")

          new <- new_references() %>%
            dplyr::select(tmpkey, journal, year, title, author) %>%
            dplyr::mutate(
              year = as.numeric(year),
              title = furrr::future_map_chr(
                title, bibliogR::clean_string,
                simplify = TRUE
              ),
              author = furrr::future_map_chr(
                author, bibliogR::clean_string,
                simplify = TRUE
              )
            ) %>%
            as.data.frame()

          old <- references() %>%
            dplyr::select(key, journal, year, title, author) %>%
            dplyr::mutate(year = as.numeric(year)) %>%
            dplyr::filter(
              journal %in% unique(new_references()$journal),
              year %in% unique(new$year)
            ) %>%
            dplyr::mutate(
              title = furrr::future_map_chr(
                title, bibliogR::clean_string,
                simplify = TRUE
              ),
              author = furrr::future_map_chr(
                author, bibliogR::clean_string,
                simplify = TRUE
              )
            ) %>%
            as.data.frame()

          nbrref <- nrow(new)
          match <- vector("list", length = nbrref)

          for (i in seq_len(nbrref)) {
            message <- paste0(
              "Identifying potential matches for reference ",
              i,
              " out of ",
              nbrref,
              ";"
            )
            incProgress(amount = (1 / nbrref), message = message)
            match[[i]] <- bibliogR::match_new_reference(
              tmpkey = new[i, "tmpkey"],
              journal = new[i, "journal"],
              year = new[i, "year"],
              title = new[i, "title"],
              author = new[i, "author"],
              method = "osa",
              references = old
            )
          }
        }
      )
      match <- match %>%
        dplyr::bind_rows() %>%
        dplyr::filter(tmpkey != "") %>%
        dplyr::mutate(keep = TRUE) %>%
        dplyr::select(keep, dplyr::everything())
      match
    })


    output$display_matches <- rhandsontable::renderRHandsontable({
      dplyr::arrange(
        matches(),
        disttitle,
        distauth
      ) %>%
        rhandsontable::rhandsontable(
          height = 600,
          width = "100%",
          stretchH = "all"
        ) %>%
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE,
          allowColEdit = FALSE
        )
    })


    observeEvent(input$done, {
      keep <- rhandsontable::hot_to_r(input$display_matches) %>%
        dplyr::filter(keep == TRUE) %>%
        dplyr::select(tmpkey) %>%
        unlist()

      complement <- new_references() %>%
        dplyr::filter(tmpkey %in% keep) %>%
        dplyr::select(-tmpkey)

      references_new <- bibliogR::add_new_references(
        complement = complement,
        references = references()
      )

      print("Saving references: the gadget will stop when this is done.")
      WriteXLS::WriteXLS(references_new, "references_new.xlsx")

      stopApp()
    })
  }

  runGadget(ui, server, viewer = paneViewer())
}
