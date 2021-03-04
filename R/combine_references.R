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
#' @importFrom shiny paneViewer
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
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
#' @importFrom tibble column_to_rownames
#' @importFrom RefManageR as.BibEntry
#' @importFrom RefManageR WriteBib
#' @export

combine_references <- function() {

  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- miniUI::miniPage(
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

    gadgetTitleBar("Combine Reference Files"),
    miniTabstripPanel(

      # Panel where the author selects references in the filtered list
      miniTabPanel(
        "Import",
        icon = icon("file-upload"),
        miniContentPanel(
          fillCol(
            flex = c(1, 1, 1),
            import_ui("initial", "Select the initial set of references"),
            import_ui("additional", "Select the additional set of references"),
            actionButton(
              "import",
              "Import",
              width = "100%",
              icon("upload"),
              style =
                "background-color: #009933; color: #FFF;"
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
          fillCol(
            flex = c(1, 1, 1, 6),
            shiny::actionButton(
              "combinexlsx",
              "Combine and export as .xlsx",
              icon = icon("download"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #009933; color: #FFF;"
            ),
            tags$hr(),
            shiny::actionButton(
              "combinebib",
              "Combine and export as .bib",
              icon = icon("download"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #003399; color: #FFF;"
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    # Bind variables
    author <- NULL
    distauth <- NULL
    disttitle <- NULL
    journal <- NULL
    key <- NULL
    title <- NULL
    tmpkey <- NULL
    year <- NULL

    # Create reactive values
    tables <- reactiveValues()
    tables$references <- NA
    tables$additional <- NA

    # Import tables upon request
    shiny::observeEvent(input$import, {
      shiny::withProgress(message = "Importing...", value = 0.3, {
        tables$references <- import_server("initial")
        shiny::incProgress(0.3)
        tables$additional <- import_server("additional") %>%
          prepare_new_references() %>%
          tibble::rownames_to_column("tmpkey")
      })
    })

    # Display and update additional references
    output$display_additional <- rhandsontable::renderRHandsontable({
      if (length(tables$additional) > 1) {
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
                title, clean_string,
                simplify = TRUE
              ),
              author = furrr::future_map_chr(
                author, clean_string,
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
                title, clean_string,
                simplify = TRUE
              ),
              author = furrr::future_map_chr(
                author, clean_string,
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
            match[[i]] <- match_new_reference(
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

    observeEvent(input$combinexlsx, {
      withProgress(message = "Combine and export...", value = 0.33, {
        complement <- tables$additional %>%
          dplyr::select(-tmpkey)

        references_new <- add_new_references(
          complement = complement,
          references = tables$references
        )

        incProgress(0.33)

        print("Now saving the file...")

        WriteXLS::WriteXLS(
          references_new,
          paste0("references_", Sys.Date(), ".xlsx")
        )

        showModal(modalDialog(
          title = "Combination and exportation complete",
          "You can now leave the application.",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    })

    observeEvent(input$combinebib, {
      withProgress(message = "Combine and export...", value = 0.33, {
        complement <- tables$additional %>%
          dplyr::select(-tmpkey)

        references_new <- add_new_references(
          complement = complement,
          references = tables$references
        )

        incProgress(0.33)

        if (length(unique(references_new)) == nrow(references_new)) {
          print("Now saving the file...")

          references_new %>%
            tibble::column_to_rownames("key") %>%
            RefManageR::as.BibEntry() %>%
            RefManageR::WriteBib(
              file = paste0("references_", Sys.Date(), ".bib")
            )

          showModal(modalDialog(
            title = "Your references have been combined and exported!",
            "You can now leave the application and import them.",
            easyClose = TRUE,
            footer = NULL
          ))
        } else {
          showModal(modalDialog(
            title = "Sorry, the file cannot be exported",
            "Keys are not unique.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      })
    })

    observeEvent(input$done, {
      stopApp()
    })
  }

  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
