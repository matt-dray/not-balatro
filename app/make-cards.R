make_card <- function(
    value = c("A", "K", "Q", "J", 10:2),
    suit = c("C", "D", "H", "S"),
    blank_path = "www/img/blank.png",  # 80x100px
    write_path = "www/img"
) {

  suit_symbol <- switch(
    as.character(suit),
    "C" = "♣",
    "D" = "♦",
    "H" = "♥",
    "S" = "♠"
  )

  suit_colour <- if (suit %in% c("D", "H")) "red" else "black"

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
      text = value,
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

  value <- if (value == "10") "T" else value
  card_file <- paste0(value, suit, ".png")
  card |> magick::image_write(file.path(write_path, card_file))

}

source("R/cards.R")
card_perms <- permute_suits_and_values(as_vector = FALSE)
purrr::pmap(card_perms, make_card)
