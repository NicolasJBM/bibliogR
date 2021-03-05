#' @name combine_references
#' @title Import References and Combine Them
#' @author Nicolas Mangin
#' @description
#' Application gathering references from different sources,
#' identifying potential duplicates, and combining them in
#' a single reference file.
#' @return An Excel file with the new complete reference list.
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
#' @importFrom shiny actionButton
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny reactiveValues
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom tibble rownames_to_column
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom future plan
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom furrr future_map_chr
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr arrange
#' @importFrom WriteXLS WriteXLS
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom tibble column_to_rownames
#' @importFrom RefManageR as.BibEntry
#' @importFrom RefManageR WriteBib
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny paneViewer
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

    shiny::tags$head(shiny::tags$style(
      shiny::HTML(".shiny-notification {
              position:fixed;top: 30%;left: 0%;right: 0%;
           }")
    )),

    miniUI::gadgetTitleBar("Combine Reference Files"),
    miniUI::miniTabstripPanel(

      # Panel where the author selects references in the filtered list
      miniUI::miniTabPanel(
        "Import",
        icon = shiny::icon("file-upload"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            flex = c(1, 1, 1),
            import_ui("initial", "Select the initial set of references"),
            import_ui("additional", "Select the additional set of references"),
            shiny::actionButton(
              "import",
              "Import",
              width = "100%",
              shiny::icon("upload"),
              style =
                "background-color: #009933; color: #FFF;"
            )
          )
        )
      ),

      miniUI::miniTabPanel(
        "Edit",
        icon = shiny::icon("edit"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            flex = c(10, 2),
            rhandsontable::rHandsontableOutput("display_additional"),
            shiny::actionButton(
              "update",
              "Update",
              icon = shiny::icon("pencil-alt"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #009933; color: #FFF;"
            )
          )
        )
      ),

      miniUI::miniTabPanel(
        "Duplicates",
        icon = shiny::icon("clone"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            flex = c(1, 9, 2),
            shiny::actionButton(
              "searchdupl",
              "Scan",
              icon = shiny::icon("search"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #330099; color: #FFF;"
            ),
            rhandsontable::rHandsontableOutput("display_matches"),
            shiny::actionButton(
              "filterselection",
              "Filter",
              icon = shiny::icon("filter"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #990033; color: #FFF;"
            )
          )
        )
      ),

      miniUI::miniTabPanel(
        "Export",
        icon = shiny::icon("file-download"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            flex = c(1, 1, 1, 6),
            shiny::actionButton(
              "combinexlsx",
              "Combine and export as .xlsx",
              icon = shiny::icon("download"),
              width = "100%",
              style =
                "margin-top:30px; background-color: #009933; color: #FFF;"
            ),
            shiny::tags$hr(),
            shiny::actionButton(
              "combinebib",
              "Combine and export as .bib",
              icon = shiny::icon("download"),
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
    tables <- shiny::reactiveValues()
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

    shiny::observeEvent(input$update, {
      tables$additional <- suppressWarnings(
        rhandsontable::hot_to_r(input$display_additional)
      )
    })

    # Flag potential duplicates
    shiny::observeEvent(input$searchdupl, {
      shiny::withProgress(
        message = "Find potential matches",
        detail = "This may take a while...", {
          shiny::incProgress(
            amount = 0,
            message = "Search for potential duplicates;"
          )

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
            shiny::incProgress(amount = (1 / nbrref), message = message)

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

    shiny::observeEvent(input$filterselection, {
      keep <- rhandsontable::hot_to_r(input$display_matches) %>%
        dplyr::filter(keep == TRUE)

      tables$matches <- keep

      complement <- tables$additional %>%
        dplyr::filter(tmpkey %in% keep$tmpkey)

      tables$additional <- complement
    })

    shiny::observeEvent(input$combinexlsx, {
      shiny::withProgress(message = "Combine and export...", value = 0.33, {
        complement <- tables$additional %>%
          dplyr::select(-tmpkey)

        references_new <- add_new_references(
          complement = complement,
          references = tables$references
        )

        shiny::incProgress(0.33)

        print("Now saving the file...")

        WriteXLS::WriteXLS(
          references_new,
          paste0("references_", Sys.Date(), ".xlsx")
        )

        shiny::showModal(shiny::modalDialog(
          title = "Combination and exportation complete",
          "You can now leave the application.",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    })

    shiny::observeEvent(input$combinebib, {
      shiny::withProgress(message = "Combine and export...", value = 1 / 3, {
        complement <- tables$additional %>%
          dplyr::select(-tmpkey)

        references_new <- add_new_references(
          complement = complement,
          references = tables$references
        )

        shiny::incProgress(1 / 3)

        if (length(unique(references_new)) == nrow(references_new)) {
          print("Now saving the file...")

          references_new %>%
            tibble::column_to_rownames("key") %>%
            RefManageR::as.BibEntry() %>%
            RefManageR::WriteBib(
              file = paste0("references_", Sys.Date(), ".bib")
            )

          shiny::showModal(shiny::modalDialog(
            title = "Your references have been combined and exported!",
            "You can now leave the application and import the file.",
            easyClose = TRUE,
            footer = NULL
          ))
        } else {
          shiny::showModal(shiny::modalDialog(
            title = "Sorry, the file cannot be exported",
            "Keys are not unique.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      })
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  shiny::runGadget(
    ui,
    server,
    viewer = shiny::paneViewer(minHeight = "maximize"))
}
