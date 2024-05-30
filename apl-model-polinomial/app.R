library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Data ----
wilayah_id <- c(
  
)

# Tautan ----
tautan_apl_lain <- tags$a(
  shiny::icon("shapes"),
  "Lainnya",
  href = "https://kristantomath.com/",
  target = "_blank"
)
tautan_github <- tags$a(
  shiny::icon("github"),
  "Github",
  href = "https://github.com/ydkristanto/matematika-tl-11",
  target = "_blank"
)

# Antarmuka ----
ui <- page_navbar(
  title = "Pemodelan Fungsi Polinomial",
  id = "model_polinomial",
  ## Sidebar ----
  sidebar = sidebar(
    ### Gambar fitur ----
    htmlOutput("gambar_fitur"),
    ### Data ----
    selectInput(
      "data",
      div("Data:", style = "font-weight: bold;"),
      choices = c(
        "Ketakcukupan Pangan" = "pangan",
        "Produksi Gabah" = "gabah",
        "Wisatawan Borobudur" = "borobudur",
        "Pembangunan Rumah" = "rumah",
        "Preferensi Musik" = "musik"
      ),
      selected = "pangan"
    ),
    conditionalPanel(
      "input.data == 'pangan'",
      selectizeInput(
        "provinsi",
        div("Wilayah:", style = "font-weight: bold;"),
        choices = c(
          "Indonesia"
        ),
        selected = "Indonesia"
      )
    ),
    ### Derajat model ----
    numericInput(
      "derajat",
      div("Derajat Polinomial:", style = "font-weight: bold;"),
      value = 1,
      min = 0,
      max = 10,
      step = 1
    ),
    ### Galat ----
    checkboxInput(
      "galat",
      "Tampilkan galat",
      value = FALSE
    )
  ),
  ## Panel utama ----
  nav_panel(
    ### Eksplorasi ----
    title = "Eksplorasi",
    icon = shiny::icon("chart-simple")
  ),
  nav_panel(
    ### Informasi ----
    title = "Informasi",
    icon = shiny::icon("circle-info")
  ),
  nav_spacer(),
  nav_menu(
    ### Menu ----
    title = "Tautan",
    nav_item(tautan_apl_lain),
    nav_item(tautan_github),
    icon = shiny::icon("link"),
    align = "right"
  ),
  footer = div(
    "© 2024 Yosep Dwi Kristanto",
    style = "font-size: 0.8em; text-align: right;"
  )
)


# Peladen ----
server <- function(input, output, session) {
  
}

# Aplikasi Shiny ----
shinyApp(ui, server)
