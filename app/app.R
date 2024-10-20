ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
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
      shiny::actionButton("button_rank", "Rank", shiny::icon("hashtag")),
      shiny::actionButton("button_suit", "Suit", shiny::icon("heart")),
      shiny::actionButton("button_draw", "Draw", shiny::icon("square-plus")),
      shiny::tags$h3(shiny::textOutput("hand_count")),
      sortable::rank_list(
        input_id = "hand_list",
        labels = NULL,
        orientation = "horizontal",
        options = set_opts_hand_limit("shared_group", pull_limit = 8, put_limit = 5)
      )
    )
  )
)

server <- function(input, output) {

  # Data ----

  all_card_images <- get_card_taglist()

  # Reactive values ----

  # Set up card sets

  rv <- shiny::reactiveValues(
    hand = NULL,
    pool = sample(permute_suits_and_ranks(), 8) |> order_cards("rank")
  )

  deck <- permute_suits_and_ranks()
  rv[["deck"]] <- deck[!deck %in% shiny::isolate(rv[["pool"]])]

  pool <- shiny::isolate(rv[["pool"]])
  pool_images <- all_card_images[names(all_card_images) %in% pool]
  rv[["pool_images"]] <- pool_images[shiny::isolate(rv[["pool"]])]  # reorder

  # Observers ----

  # Remove hand cards from pool
  shiny::observe({
    rv[["hand"]] <- input$hand_list
    pool <- rv[["pool"]]
    rv[["pool"]] <- pool[!pool %in% rv[["hand"]]]
  })

  # Prevent card draw if the pool is full
  shiny::observe({
    if (length(input$pool_list) == 8) {
      shinyjs::disable("button_draw")
    } else {
      shinyjs::enable("button_draw")
    }
  })

  # On button click, draw new pool
  shiny::observeEvent(input$button_draw, {

    deck <- rv[["deck"]]
    pool <- rv[["pool"]]
    n_cards_needed <- 8 - length(pool)

    new_cards <- sample(deck, n_cards_needed)
    new_pool <- c(pool, new_cards)

    rv[["deck"]] <- deck[!deck %in% new_cards]  # remove sampled cards from deck
    rv[["pool"]] <- new_pool  # set sample as pool
    rv[["pool_images"]] <- all_card_images[names(all_card_images) %in% rv[["pool"]]]

    sortable:::update_rank_list(
      "pool_list",
      text = rv[["pool_images"]]
    )

  })

  # On button click, order by rank
  shiny::observeEvent(input$button_rank, {

    cards_ordered <- order_cards(rv[["pool"]], "rank")
    rv[["pool"]] <- cards_ordered
    rv[["pool_images"]] <- rv[["pool_images"]][cards_ordered]

    sortable:::update_rank_list(
      "pool_list",
      text = rv[["pool_images"]]
    )

  })

  # On button click, order by rank
  shiny::observeEvent(input$button_suit, {

    cards_ordered <- order_cards(rv[["pool"]], "suit")
    rv[["pool"]] <- cards_ordered
    rv[["pool_images"]] <- rv[["pool_images"]][cards_ordered]

    sortable:::update_rank_list(
      "pool_list",
      text = rv[["pool_images"]]
    )

  })

  # Outputs ----

  # Create pool list UI
  output$card_pool_ui <- shiny::renderUI({
    sortable::rank_list(
      input_id = "pool_list",
      labels = rv[["pool_images"]],
      orientation = "horizontal",
      options = sortable::sortable_options(group = "shared_group")
    )
  })

  output$pool_count <- shiny::renderText({
    pool_size <- length(input$pool_list)
    deck_count <- shiny::isolate(rv[["deck"]])
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
