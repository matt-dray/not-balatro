files <- list.files("www/img", pattern = "^\\w{2}.png$", full.names = TRUE)

get_card_score <- function(hand) {

  if (length(hand) == 0) return(0)

  values <- strsplit(hand, "") |> lapply("[", 2)

  lapply(
    values,
    \(value) if (value %in% c("A", "K", "Q", "J")) 11 else as.numeric(value)
  ) |>
    unlist() |>
    sum()

}

get_card_name <- function(file) {
  file |>
    tools::file_path_sans_ext() |>
    basename() |>
    gsub("_", " ", x = _)
}

cards <- shiny::tagList()

for (file in files) {
  source <- base64enc::dataURI(file = file, mime = "image/png")
  img <- shiny::tags$image(src = source)
  card_name <- get_card_name(file)
  cards[[card_name]] <- img
}

opts_item_limit <- sortable::sortable_options(
  # Via Barret Schloerke:
  #   https://forum.posit.co/t/shiny-sortable-how-to-limit-number-of-items-that-can-be-dropped/69233/2
  # In turn, inspiration from:
  #   https://jsbin.com/nacoyah/edit?js,output
  group = list(
    name = "shared_group",
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 1 child already
        return to.el.children.length < 5;
      }
    ")
  )
)

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style.css"
    )
  ),
  shiny::fluidRow(
    shiny::column(
      shiny::tags$h1("Not Balatro"),
      shiny::tags$p(
        "An experiment with the",
        shiny::tags$a(href = "https://rstudio.github.io/sortable", "{sortable}"),
        "package for R."
      ),
      shiny::tags$h2("Cards"),
      shiny::tags$p("Click and drag cards from the pool into your hand."),
      width = 12,
      shiny::tags$h3(shiny::textOutput("pool_count")),
      sortable::rank_list(
        input_id = "pool_list",
        labels = cards,
        orientation = "horizontal",
        options = sortable::sortable_options(group = "shared_group")
      ),
      shiny::tags$h3(shiny::textOutput("hand_count")),
      sortable::rank_list(
        input_id = "hand_list",
        labels = NULL,
        orientation = "horizontal",
        options = opts_item_limit,
      )
    )
  )
)

server <- function(input, output) {

  output$pool_card_names <- shiny::renderPrint(input$pool_list)
  output$hand_card_names <- shiny::renderPrint(input$hand_list)

  hand_card_score <- shiny::reactive(get_card_score(input$hand_list))

  output$pool_count <- shiny::renderText({
    num <- if (is.null(input$pool_list)) length(cards) else length(input$pool_list)
    paste0("Pool (", num, "/", length(cards), ")")
  })

  output$hand_count <- shiny::renderText({
    num <- if (is.null(input$hand_list)) 0 else length(input$hand_list)
    paste0("Hand (", num, "/", 5, ", ", hand_card_score(), " points)")
  })

}

shiny::shinyApp(ui, server)
