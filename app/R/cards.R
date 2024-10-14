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
  list.files(path, pattern = "^\\w{2,3}.png$", full.names = TRUE)
}

get_card_name <- function(file) {
  file |>
    tools::file_path_sans_ext() |>
    basename() |>
    gsub("_", " ", x = _)
}

get_card_score <- function(hand) {

  if (length(hand) == 0) return(0)

  values <- substr(hand, 2, nchar(hand))

  lapply(
    values,
    \(value) if (value %in% c("A", "K", "Q", "J")) 11 else as.numeric(value)
  ) |>
    unlist() |>
    sum()

}
