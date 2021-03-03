#' @name combine_references
#' @title Import References and Combine Them
#' @author Nicolas Mangin
#' @description Application gathering references from different sources, identifying potential duplicates, and combining them in a single reference file.
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
#' @importFrom shiny req
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
          fillCol(
            flex = c(1, 1, 1),
            fillRow(
              flex = c(1, 1),
              shiny::fileInput(
                "references",
                "Initial references (xlsx):",
                accept = c(".xlsx"),
                multiple = FALSE,
                width = "100%"
              ),

              shiny::actionButton(
                "importxlsx",
                "Import",
                icon = icon("upload"),
                width = "100%",
                style =
                  "margin-top:30px; background-color: #009933; color: #FFF;"
              )
            ),

            tags$hr(),

            fillRow(
              flex = c(1, 1),
              shiny::fileInput(
                "files",
                "Additional references (bib):",
                accept = c(".bib"),
                multiple = TRUE,
                width = "100%"
              ),

              shiny::actionButton(
                "importbib",
                "Import",
                icon = icon("upload"),
                width = "100%",
                style =
                  "margin-top:30px; background-color: #009933; color: #FFF;"
              )
            )
          )
        )
      ),

      miniTabPanel(
        "Edit",
        icon = icon("edit"),
        miniContentPanel(
          fillCol(
            flex = c(10, 2),
            rhandsontable::rHandsontableOutput("display_additional"),
            shiny::actionButton(
              "update",
              "Update",
              icon = icon("pencil-alt"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #009933; color: #FFF;"
            )
          )
        )
      ),

      miniTabPanel(
        "Duplicates",
        icon = icon("clone"),
        miniContentPanel(
          fillCol(
            flex = c(1, 9, 2),
            shiny::actionButton(
              "searchdupl",
              "Scan",
              icon = icon("search"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #330099; color: #FFF;"
            ),
            rhandsontable::rHandsontableOutput("display_matches"),
            shiny::actionButton(
              "filterselection",
              "Filter",
              icon = icon("filter"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #990033; color: #FFF;"
            )
          )
        )
      ),

      miniTabPanel(
        "Export",
        icon = icon("file-download"),
        miniContentPanel(
          fillRow(
            shiny::actionButton(
              "combine",
              "Combine and export",
              icon = icon("download"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #993300; color: #FFF;"
            )
          )
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

    # Create reactive values
    tables <- reactiveValues()
    tables$references <- NA
    tables$additional <- NA
    tables$matches <- NA

    # Import the files to be combined
    observeEvent(input$importxlsx, {
      req(input$references)
      withProgress(message = "Importing...", value = 0.5, {
        tables$references <- readxl::read_excel(
          input$references$datapath[[1]],
          col_types = "text"
        )
      })
    })

    observeEvent(input$importbib, {
      req(input$files)
      withProgress(message = "Importing...", value = 0.5, {
        tables$additional <- input$files %>%
          dplyr::select(file = datapath) %>%
          dplyr::mutate(import = furrr::future_map(
            file, bibliogR::get_new_references
          )) %>%
          tidyr::unnest(import) %>%
          dplyr::select(-file) %>%
          tibble::rownames_to_column("tmpkey")
      })
    })

    # Display and update additional references
    output$display_additional <- rhandsontable::renderRHandsontable({
      if (!is.na(tables$additional)) {
        rhandsontable::rhandsontable(
          tables$additional,
          width = "100%",
          height = 450
        ) %>%
          rhandsontable::hot_context_menu(
            allowRowEdit = FALSE,
            allowColEdit = FALSE
          )
      }
    })

    observeEvent(input$update, {
      tables$additional <- suppressWarnings(
        rhandsontable::hot_to_r(input$display_additional)
      )
    })

    # Flag potential duplicates
    observeEvent(input$searchdupl, {
      withProgress(
        message = "Find potential matches",
        detail = "This may take a while...", {
          incProgress(amount = 0, message = "Search for potential duplicates;")

          future::plan("multisession")

          new <- tables$additional %>%
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

          old <- tables$references %>%
            dplyr::select(key, journal, year, title, author) %>%
            dplyr::mutate(year = as.numeric(year)) %>%
            dplyr::filter(
              journal %in% unique(tables$references$journal),
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
              "Processing reference ",
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
        dplyr::select(keep, dplyr::everything()) %>%
        dplyr::arrange(
          disttitle,
          distauth
        )

      if (nrow(match) == 0) {
        match <- tables$additional %>%
          dplyr::select(tmpkey, title, author, year) %>%
          dplyr::mutate(keep = TRUE) %>%
          dplyr::select(keep, dplyr::everything())
      }

      tables$matches <- match
    })

    output$display_matches <- rhandsontable::renderRHandsontable({
      if (length(tables$matches) > 1) {
        tables$matches %>%
          rhandsontable::rhandsontable(
            height = 400,
            width = "100%",
            stretchH = "right"
          ) %>%
          rhandsontable::hot_context_menu(
            allowRowEdit = FALSE,
            allowColEdit = FALSE
          )
      }
    })

    observeEvent(input$filterselection, {
      keep <- rhandsontable::hot_to_r(input$display_matches) %>%
        dplyr::filter(keep == TRUE)

      tables$matches <- keep

      complement <- tables$additional %>%
        dplyr::filter(tmpkey %in% keep$tmpkey)

      tables$additional <- complement
    })

    observeEvent(input$combine, {
      withProgress(message = "Combine and export...", value = 0.33, {
        complement <- tables$additional %>%
          dplyr::select(-tmpkey)

        references_new <- bibliogR::add_new_references(
          complement = complement,
          references = tables$references
        )

        incProgress(0.33)

        print("Now saving the file...")

        WriteXLS::WriteXLS(
          references_new,
          paste0("references_", Sys.Date(), ".xlsx")
        )
      })
    })


    observeEvent(input$done, {
      stopApp()
    })
  }

  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
