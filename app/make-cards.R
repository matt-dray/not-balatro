make_card <- function(
    rank,
    suit,
    blank_path = "app/www/img/blank.png",  # 80x100px
    write_path = "app/www/img"
) {

  if (rank == "T") rank <- "10"

  suit_symbol <- switch(
    as.character(suit),
    "C" = "♣",
    "D" = "♦",
    "H" = "♥",
    "S" = "♠"
  )

  suit_colour <- switch(
    as.character(suit),
    "C" = "black",
    "D" = "red",
    "H" = "red",
    "S" = "black"
  )

  card <- magick::image_read(blank_path) |>
    magick::image_crop("78x98") |>
    magick::image_border("grey40", "1x1") |>
    magick::image_annotate(
      text = suit_symbol,
      gravity = "center",
      size = 50,
      color = suit_colour
    ) |>
    magick::image_annotate(
      text = rank,
      location = "+5",
      size = 25,
      color = suit_colour
    ) |>
    magick::image_annotate(
      text = suit_symbol,
      location = "+5+20",
      size = 25,
      color = suit_colour
    )

  rank <- if (rank == "10") "T" else rank
  card_file <- paste0(rank, suit, ".png")
  card |> magick::image_write(file.path(write_path, card_file))

}

source("app/R/cards.R")
card_perms <- permute_suits_and_ranks(as_vector = FALSE)
purrr::pmap(card_perms, make_card)
