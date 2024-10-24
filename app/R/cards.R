set_opts_hand_limit <- function(name, pull_limit, put_limit) {
  sortable::sortable_options(
    # Via Barret Schloerke:
    #   https://forum.posit.co/t/shiny-sortable-how-to-limit-number-of-items-that-can-be-dropped/69233/2
    # In turn, inspiration from:
    #   https://jsbin.com/nacoyah/edit?js,output
    group = list(
      name = "shared_group",
      pull = htmlwidgets::JS(paste0("
        function(to) {
          // only allow a 'put' if there is less than x children already
          return to.el.children.length < ", pull_limit, ";
        }
      ")),
      put = htmlwidgets::JS(paste0("
        function(to) {
          // only allow a 'put' if there is less than x children already
          return to.el.children.length < ", put_limit, ";
        }
      "))
    )
  )
}

get_card_taglist <- function(files = get_card_images()) {

  cards <- shiny::tagList()

  for (file in files) {
    source <- base64enc::dataURI(file = file, mime = "image/png")
    img <- shiny::tags$image(src = source)
    card_name <- get_card_name(file)
    cards[[card_name]] <- img
  }

  cards

}

get_card_images <- function(path = "www/img") {
  list.files(path, pattern = "^\\w{2}.png$", full.names = TRUE)
}

get_card_name <- function(file) {
  file |>
    tools::file_path_sans_ext() |>
    basename() |>
    gsub("_", " ", x = _)
}

score_hand <- function(hand) {

  if (length(hand) == 0) return(0)

  ranks <- substr(hand, 1, 1)

  lapply(
    ranks,
    \(rank) {
      if (rank %in% c("A", "K", "Q", "J")) {
        11
      } else if (rank == "T") {
        10
      } else {
        as.numeric(rank)
      }
    }
  ) |>
    unlist() |>
    sum()

}

evaluate_poker_hand <- function(hand) {

  if (length(hand) == 0) return("no hand")

  rank_table <- substr(hand, 1, 1) |> table() |> sort()
  suit_table  <- substr(hand, 2, 2) |> table() |> sort()

  ranks_ordered <- enumerate_cards(hand) |> sort()
  is_straight <- all(unique(diff(ranks_ordered)) == 1)

  if (
    length(hand) == 5 &
    all(names(rank_table) %in% c("A", "K", "Q", "J", "T")) &
    is_straight &
    length(suit_table) == 1
  ) {
    return("royal flush")
  }
  if (length(hand) == 5 & is_straight & length(suit_table) == 1) return("straight flush")
  if (max(rank_table == 4)) return("four of a kind")
  if (
    length(hand) == 5 &
    length(which(rank_table == 2)) == 1 &
    length(which(rank_table == 3)) == 1
  ) {
    return("full house")
  }
  if (length(hand) == 5 & length(suit_table) == 1) return("flush")
  if (length(hand) == 5 & is_straight) return("straight")
  if (!is.na(table(rank_table)["3"]) && table(rank_table)["3"] == 1) return("three of a kind")
  if (length(hand) > 3 & length(which(rank_table == 2)) == 2) return("two pair")
  if (!is.na(table(rank_table)["2"]) && table(rank_table)["2"] == 1) return("a pair")
  if (length(hand) == length(rank_table)) return("high card")

}

enumerate_cards <- function(cards) {

  ranks <- substr(cards, 1, 1)

  lapply(
    ranks,
    \(rank) switch(
      rank,
      "A" = "14",
      "K" = "13",
      "Q" = "12",
      "J" = "11",
      "T" = "10",
      rank
    )
  ) |>
    unlist() |>
    as.numeric()

}

permute_suits_and_ranks <- function(
    ranks = c(2:9, "T", "J", "Q", "K", "A"),
    suits = c("C", "D", "H", "S"),
    as_vector = TRUE
) {

  permutations <- expand.grid(rank = ranks, suit = suits)

  if (as_vector) {
    permutations <- paste0(permutations[["rank"]], permutations[["suit"]])
  }

  rev(permutations)

}

order_cards <- function(cards, order_by = c("rank", "suit")) {

  order_by <- match.arg(order_by)

  cards_df <- data.frame(
    card = cards,
    rank = substr(cards, 1, 1) |> enumerate_cards(),
    suit = substr(cards, 2, 2)
  )

  if (order_by == "rank") cards_sorted <- sort_by(cards_df, ~ rank + suit)
  if (order_by == "suit") cards_sorted <- sort_by(cards_df, ~ suit + rank)

  cards_sorted[["card"]]

}
