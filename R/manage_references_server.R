#' @name manage_references_server
#' @title Batch import new references or delete duplicates.
#' @author Nicolas Mangin
#' @description Module allowing the user to import multiple references from a file or to identify and remove duplicated references.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param references Reactive. Function containing a list of references.
#' @param refdir Character. Path to the folder where the references are.
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_chr
#' @importFrom purrr map_int
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny NS
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny req
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stringr str_split
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @export



manage_references_server <- function(id, references, refdir){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    abstract <- NULL
    at_risk <- NULL
    author <- NULL
    count <- NULL
    data <- NULL
    distance <- NULL
    journal <- NULL
    key <- NULL
    simplified_key <- NULL
    title <- NULL
    year <- NULL
    distmethod <- NULL
    maxdist <- NULL
    name <- NULL
    datapath <- NULL
    check <- NULL
    field <- NULL
    issn <- NULL
    jnl <- NULL
    
    
    refpath <- shiny::reactive({ base::paste0(refdir, "/references.RData") })
    
    modrval <- shiny::reactiveValues()
    
    importations <- shiny::reactive({
      shiny::req(!base::is.null(input$filestoimport))
      shiny::req(base::nrow(input$filestoimport)>0)
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Importing references..."
      )
      importations <- input$filestoimport |>
        dplyr::mutate(data = purrr::map2(name, datapath, bibliogR::get_references)) |>
        dplyr::select(data) |>
        tidyr::unnest(data) |>
        base::unique()
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "References imported!",
        text = "You can now select and edit them.",
        type = "success"
      )
      importations
    })
    
    duplications <- shiny::reactive({
      shiny::req(!base::is.null(input$minyear))
      shiny::req(!base::is.null(input$maxyear))
      shiny::req(!base::is.null(input$distmethod))
      shiny::req(!base::is.null(input$maxdist))
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Searching for potential duplications..."
      )
      duplications <- references() |>
        dplyr::filter(year >= input$minyear, year <= input$maxyear) |>
        dplyr::select(key, title) |>
        dplyr::mutate(simplified_key = purrr::map_chr(key, stringr::str_extract, pattern = "^[A-Z].+[0-9]{4}")) |>
        dplyr::mutate(simplified_key = base::tolower(simplified_key)) |>
        dplyr::group_by(simplified_key) |>
        tidyr::nest() |>
        dplyr::mutate(count = purrr::map_int(data, base::nrow)) |>
        dplyr::filter(simplified_key != "", count > 1) |>
        dplyr::mutate(at_risk = purrr::map(
          data, find_potential_duplicates,
          distmethod = input$distmethod,
          maxdist = input$maxdist
        )) |>
        dplyr::mutate(check = purrr::map2(data, at_risk, function(x,y){
          x[y,]
        })) |>
        dplyr::ungroup() |>
        dplyr::select(check) |>
        tidyr::unnest(check) |>
        dplyr::arrange(key) |>
        dplyr::select(key) |>
        dplyr::left_join(references(), by = "key") |>
        dplyr::select(key, title, author, journal, abstract)
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "Search successful!",
        text = "You can now select which references you wish to delete.",
        type = "success"
      )
      duplications
    })
    
    shiny::observeEvent(input$importfiles, {
      modrval$check <- importations()
      modrval$type <- "importations"
    })
    
    shiny::observeEvent(input$searchduplicates, {
      modrval$check <- duplications()
      modrval$type <- "duplications"
    })
    
    output$importations_duplications <- rhandsontable::renderRHandsontable({
      shiny::req(base::length(modrval$check) >= 5)
      modrval$check |>
        dplyr::mutate(remove = FALSE) |>
        dplyr::select(remove, dplyr::everything()) |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_cols(manualColumnResize = TRUE) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$import_remove, {
      shiny::req(base::length(modrval$check) >= 5)
      shiny::req(modrval$type %in% c("importations","duplications"))
      selection <- rhandsontable::hot_to_r(input$importations_duplications)
      shiny::req(base::length(selection) >= 5)
      if (modrval$type == "importations"){
        shinyalert::shinyalert(
          "Import these references?",
          base::paste0(
            "Are you sure you want to import ",
            base::nrow(dplyr::filter(selection, remove == FALSE)),
            " new references?"
          ),
          showCancelButton = TRUE, cancelButtonText = "Cancel",
          confirmButtonText = "Import", inputId = "confirmimport",
          type = "warning"
        )
      } else {
        shinyalert::shinyalert(
          "Remove these references?",
          base::paste0(
            "Are you sure you want to remove ",
            base::nrow(dplyr::filter(selection, remove == TRUE)),
            " references?"
          ),
          showCancelButton = TRUE, cancelButtonText = "Cancel",
          confirmButtonText = "Delete", inputId = "confirmdelete",
          type = "warning"
        )
      }
    })
    
    shiny::observeEvent(input$confirmimport, {
      shiny::req(modrval$type == "importations")
      import <- rhandsontable::hot_to_r(input$importations_duplications) |>
        dplyr::filter(remove == FALSE) |>
        dplyr::mutate(key = NA) |>
        dplyr::select(-remove)
      shiny::req(base::length(import) >= 5)
      shiny::req(base::nrow(import) >= 1)
      assigned_keys <- references()$key
      for (i in base::seq_len(base::nrow(import))){
        tmpkey <- bibliogR::make_new_key(
          name = base::unlist(stringr::str_split(import$author[[i]], ","))[1],
          year = import$year[[i]],
          keys = assigned_keys
        )
        import$key[[i]] <- tmpkey
        assigned_keys <- c(assigned_keys, tmpkey)
      }
      # Append journal information
      journal_info <- references() |>
        dplyr::select(journal, issn, jnl, field) |>
        base::unique() |> stats::na.omit()
      import <- import |>
        dplyr::left_join(journal_info, by = "journal")
      references <- dplyr::bind_rows(references(), import)
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Adding new references..."
      )
      base::save(references, file = refpath())
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "New references added!",
        text = "Now reload the references to see the changes you made.",
        type = "success"
      )
      modrval$check <- NA
    })
    
    shiny::observeEvent(input$confirmdelete, {
      shiny::req(modrval$type == "duplications")
      remove <- rhandsontable::hot_to_r(input$importations_duplications) |>
        dplyr::filter(remove == TRUE)
      shiny::req(base::length(remove) >= 5)
      references <- references() |>
        dplyr::filter(!(key %in% remove$key))
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Removing duplicates..."
      )
      base::save(references, file = refpath())
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "Duplicates removed!",
        text = "Now reload the references to see the changes you made.",
        type = "success"
      )
      modrval$check <- NA
    })
    
  })
}

