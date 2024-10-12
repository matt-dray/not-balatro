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
      shiny::tags$h2("Jokers"),
      shiny::tags$p("Click and drag jokers from the pool into your hand."),
      width = 12,
      shiny::tags$h3(shiny::textOutput("pool_count")),
      sortable::rank_list(
        input_id = "pool_list",
        labels = jokers,
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
  ),
  tags$h2("Selections"),
  "Pool: ", verbatimTextOutput("pool_card_names"),
  "Hand: ", verbatimTextOutput("hand_card_names"),
)

server <- function(input, output) {

  output$joker_count <- shiny::renderText(length(jokers))
  output$pool_count <- shiny::renderText({
    num <- if (is.null(input$pool_list)) {
      length(jokers)
    } else {
      length(input$pool_list)
    }
    paste0("Pool (", num, "/", length(jokers), ")")
  })
  output$hand_count <- shiny::renderText({
    num <- if (is.null(input$hand_list)) {
      0
    } else {
      length(input$hand_list)
    }
    paste0("Hand (", num, "/", 5, ")")
  })

  output$pool_card_names <- shiny::renderPrint(input$pool_list)
  output$hand_card_names <- shiny::renderPrint(input$hand_list)

}

shiny::shinyApp(ui, server)
