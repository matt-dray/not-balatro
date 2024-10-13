source(list.files("R", full.names = TRUE))

all_cards <- get_card_taglist()

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
      shiny::uiOutput("card_pool_ui"),
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

  output$card_pool_ui <- shiny::renderUI({

    card_sample <- sample(all_cards, 8) |> names()
    cards <- all_cards[names(all_cards) %in% card_sample]

    sortable::rank_list(
      input_id = "pool_list",
      labels = cards,
      orientation = "horizontal",
      options = sortable::sortable_options(group = "shared_group")
    )

  })

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
