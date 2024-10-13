make_card <- function(
    suit = c("C", "D", "H", "S"),
    value = c("A", "K", "Q", "J", 10:2),
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

  card_file <- paste0(suit, value, ".png")
  card |> magick::image_write(file.path(write_path, card_file))

}

suits <- c("C", "D", "H", "S")
values <- c("A", "K", "Q", "J", 10:1)
card_perms <- expand.grid(value = values, suit = suits)
purrr::pmap(card_perms, make_card)
