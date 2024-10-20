
# not-balatro

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![](https://img.shields.io/badge/Shiny-shinylive-447099?style=flat&labelColor=white&logo=Posit&logoColor=447099)](https://matt-dray.github.io/not-balatro/)
<!-- badges: end -->

A test of [the {sortable} package](https://rstudio.github.io/sortable/) to see how pliable it is to play card games with R using [{shiny} apps](https://shiny.posit.co/). Specifically, clicking and dragging 'cards' from a pool into your hand and then calculating a score given your selections.

[Available on the web](https://matt-dray.github.io/not-balatro/) and prepared for serverless deployment with [{shinylive}](https://posit-dev.github.io/r-shinylive/). Very much a work in progress/proof of concept.

<img src='img/cards.gif' alt="Two rows of playing cards labelled 'pool' and 'hand'. Buttons are pressed to order the cards by rank and then by suit. Cards are dragged from the pool to the hand. A 4 is dragged and the text updates from 'no hand' to 'high card'. Another 4 and it changes to 'a pair'. A 'draw' button is pressed and two new cards are added to the pool. Then two kings are added to the hand and the text changes to 'two pair'.">
