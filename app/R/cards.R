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

  values <- substr(hand, 1, 1)

  lapply(
    values,
    \(value) {
      if (value %in% c("A", "K", "Q", "J")) {
        11
      } else if (value == "T") {
        10
      } else {
        as.numeric(value)
      }
    }
  ) |>
    unlist() |>
    sum()

}

evaluate_poker_hand <- function(hand) {

  if (length(hand) == 0) return("no hand")

  value_table <- substr(hand, 1, 1) |> table() |> sort()
  suit_table  <- substr(hand, 2, 2) |> table() |> sort()

  values_ordered <- enumerate_cards(hand) |> sort()
  is_straight <- all(unique(diff(values_ordered)) == 1)

  if (
    length(hand) == 5 &
    all(names(value_table) %in% c("A", "K", "Q", "J", "T")) &
    is_straight &
    length(suit_table) == 1
  ) {
    return("royal flush")
  }
  if (length(hand) == 5 & is_straight & length(suit_table) == 1) return("straight flush")
  if (max(value_table == 4)) return("four of a kind")
  if (
    length(hand) == 5 &
    length(which(value_table == 2)) == 1 &
    length(which(value_table == 3)) == 1
  ) {
    return("full house")
  }
  if (length(hand) == 5 & length(suit_table) == 1) return("flush")
  if (length(hand) == 5 & is_straight) return("straight")
  if (!is.na(table(value_table)["3"]) && table(value_table)["3"] == 1) return("three of a kind")
  if (length(hand) > 3 & length(which(value_table == 2)) == 2) return("two pair")
  if (!is.na(table(value_table)["2"]) && table(value_table)["2"] == 1) return("a pair")
  if (length(hand) == length(value_table)) return("high card")

}

enumerate_cards <- function(hand) {

  values <- substr(hand, 1, 1)

  lapply(
    values,
    \(value) switch(
      value,
      "A" = "14",
      "K" = "13",
      "Q" = "12",
      "J" = "11",
      "T" = "10",
      value
    )
  ) |>
    unlist() |>
    as.numeric()

}

permute_suits_and_values <- function(
    values = c("A", "K", "Q", "J", 10:2),
    suits = c("C", "D", "H", "S"),
    as_vector = TRUE
) {

  permutations <- expand.grid(value = values, suit = suits)

  if (as_vector) {
    permutations <- paste0(permutations[["value"]], permutations[["suit"]])
  }

  permutations

}
