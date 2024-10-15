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
      shiny::tags$h1("🃏 Not Balatro"),
      shiny::tags$p(
        "A work-in-progress experiment by",
        shiny::tags$a(href = "https://www.matt-dray.com/", "Matt"),
        "with the",
        shiny::tags$a(href = "https://rstudio.github.io/sortable", "{sortable}"),
        "package for R. Find the source",
        shiny::tags$a(href = "https://github.com/matt-dray/not-balatro", "on GitHub."),
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

  rv <- shiny::reactiveValues(
    deck_remaining = permute_suits_and_values()
  )

  output$card_pool_ui <- shiny::renderUI({

    card_sample <- sample(all_cards, 8) |> names()
    deck_remaining <- shiny::isolate(rv[["deck_remaining"]])
    rv[["deck_remaining"]] <- deck_remaining[!deck_remaining %in% card_sample]
    cards <- all_cards[names(all_cards) %in% card_sample]

    sortable::rank_list(
      input_id = "pool_list",
      labels = cards,
      orientation = "horizontal",
      options = sortable::sortable_options(group = "shared_group")
    )

  })

  output$pool_count <- shiny::renderText({
    pool_size <- length(input$pool_list)
    deck_count <- shiny::isolate(rv[["deck_remaining"]])
    paste0("Pool (", pool_size, "/8, ", length(deck_count), "/52 in deck)")
  })

  output$hand_count <- shiny::renderText({
    card_count <- if (is.null(input$hand_list)) 0 else length(input$hand_list)
    poker_hand <- evaluate_poker_hand(input$hand_list)
    score <- score_hand(input$hand_list)
    paste0(
      "Hand (",
      card_count, "/5, ",
      poker_hand, ", ",
      score, " points)"
    )
  })

}

shiny::shinyApp(ui, server)
