files <- list.files("www/img", pattern = ".webp$", full.names = TRUE)

get_joker_name <- function(file) {
  file |>
    tools::file_path_sans_ext() |>
    basename() |>
    gsub("_", " ", x = _)
}

jokers <- shiny::tagList()

for (file in files) {
  source <- base64enc::dataURI(file = file, mime = "image/webp")
  img <- shiny::tags$image(src = source)
  joker_name <- get_joker_name(file)
  jokers[[joker_name]] <- img
}

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
      shiny::tags$p("An experiment with the {sortable} package for R."),
      shiny::tags$h2("Jokers"),
      shiny::tags$p("Click and drag jokers from the pool into your hand."),
      width = 12,
      sortable::bucket_list(
        header = NULL,
        group_name = "bucket_jokers",
        orientation = "vertical",
        sortable::add_rank_list(
          text = "Pool",
          labels = jokers,
          input_id = "rank_list_pool",
          orientation = "horizontal"
        ),
        sortable::add_rank_list(
          text = "Hand",
          labels = NULL,
          input_id = "rank_list_hand",
          orientation = "horizontal"
        )
      )
    )
  ),
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::tags$b("Result"),
      shiny::column(
        width = 12,
        shiny::tags$p("input$rank_list_pool"),
        shiny::verbatimTextOutput("pool_set"),
        shiny::tags$p("input$rank_list_hand"),
        shiny::verbatimTextOutput("hand_set"),
        shiny::tags$p("input$bucket_jokers"),
        shiny::verbatimTextOutput("bucket_set")
      )
    )
  )
)

server <- function(input, output, session) {
  output$pool_set <- shiny::renderPrint(input$rank_list_pool)
  output$hand_set <- shiny::renderPrint(input$rank_list_hand)
  output$bucket_set <- shiny::renderPrint(input$bucket_jokers)
}

shiny::shinyApp(ui, server)
