library(shiny)
library(bslib)
library(tidyverse)

# Data ----
#' Sumber: J. Schrader, S. Moeljono, J. Tambing, C. Sattler, & H. Kreft
#' https://doi.org/10.3897/BDJ.8.e55275
#' Baca juga: J. Schrader, S. Moeljono, G. Keppel, & H. Kreft
#' https://doi.org/10.1111/ecog.04512
load(url("https://raw.githubusercontent.com/ydkristanto/matematika-tl-11/main/data/data_raja_ampat.RData"))

# Antarmuka ----
ui <- page_navbar(
  title = "Jelajah Kepulauan Raja Ampat",
  sidebar = sidebar(
    selectInput(
      "analisis", "Tingkat Analisis:",
      choices = c("Pulau", "Transek", "Plot"),
      selected = "Pulau"
    ),
    selectInput(
      "model", "Model:",
      choices = c(
        "Linear",
        "Pangkat",
        "Logaritma",
        "Eksponensial Negatif",
        "Fungsi Rasional"
      )
    )
  ),
  nav_panel(
    title = "Eksplorasi"
  ),
  nav_panel(
    title = "Informasi"
  )
)

# Peladen ----
server <- function(input, output, session) {
  
}

# Jalankan aplikasi ----
shinyApp(ui, server)

