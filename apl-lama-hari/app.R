library(shiny)
library(bslib)
library(tidyverse)
library(plotly)

# Data ----
load(url("https://raw.githubusercontent.com/ydkristanto/matematika-tl-11/main/data/sun_data.RData"))
daftar_kota <- sun_data %>% 
  distinct(city) %>% 
  arrange(city) %>% 
  mutate(
    kota = str_to_title(str_replace_all(city, "-", " "))
  )
nama_kota <- daftar_kota$city
names(nama_kota) <- daftar_kota$kota
daftar_statistik <- c(
  "Lama hari" = "day_length",
  "Lama malam" = "night_length",
  "Perbedaan siang malam" = "diff",
  "Matahari terbit" = "sunrise_time",
  "Matahari terbenam" = "sunset_time",
  "Fajar" = "CT_start",
  "Senja" = "CT_end",
  "Tengah hari" = "SN_time",
  "Jarak matahari" = "SN_mil_km"
)

# Tautan ----
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
  href = "https://github.com/ydkristanto/apl-derajat-rad",
  target = "_blank"
)

# Antarmuka ----
ui <- page_navbar(
  title = "Lama Hari",
  id = "lama_hari",
  ## Sidebar ----
  sidebar = sidebar(
    accordion(
      ### Statistik ----
      accordion_panel(
        title = "Statistik",
        selectInput(
          "statistik",
          div("Statistik:", style = "font-weight: bold;"),
          daftar_statistik,
          selected = "day_length"
        ),
        selectInput(
          "periode",
          div("Periode:", style = "font-weight: bold;"),
          c(
            "Harian" = "hari",
            "Bulanan" = "bulan"
          ),
          selected = "hari"
        ),
        icon = shiny::icon("chart-line")
      ),
      ### Filter ----
      accordion_panel(
        title = "Filter",
        selectInput(
          "kota",
          div("Kota:", style = "font-weight: bold;"),
          nama_kota,
          selected = c("denpasar", "linz"),
          multiple = TRUE
        ),
        dateRangeInput(
          "interval",
          div("Interval:", style = "font-weight: bold;"),
          start = "2023-01-01",
          end = "2023-12-31",
          min = "2021-01-01",
          max = "2030-12-31",
          format = "dd-mm-yyyy",
          weekstart = 1,
          language = "id",
          separator = " - "
        ),
        icon = shiny::icon("filter")
      )
    )
  ),
  nav_panel(
    title = "Eksplorasi",
    navset_card_underline(
      title = "Penyajian Data",
      nav_panel(
        title = "Diagram",
        plotlyOutput("diagram")
      ),
      nav_panel(
        title = "Tabel",
        dataTableOutput("tabel")
      ),
      full_screen = TRUE
    ),
    icon = shiny::icon("chart-simple")
  ),
  nav_panel(
    title = "Informasi",
    
    icon = shiny::icon("circle-info")
  ),
  nav_spacer(),
  nav_menu(
    title = "Tautan",
    nav_item(tautan_apl_lain),
    nav_item(tautan_github),
    icon = shiny::icon("link"),
    align = "right"
  )
)

# Server ----
server <- function(input, output, session) {
  ## Filter data ----
  dat <- reactive({
    periode <- input$periode
    kota <- input$kota
    min_tanggal <- input$interval[1]
    maks_tanggal <- input$interval[2]
    
    data <- sun_data %>% 
      filter(
        city %in% kota,
        date >= min_tanggal,
        date <= maks_tanggal
      )
    
    data
  })
  
  ## Diagram ----
  output$diagram <- renderPlotly({
    periode <- input$periode
    statistik <- input$statistik
    teks <- names(daftar_statistik)[which(daftar_statistik %in% statistik)]
    judul_x <- ifelse(
      periode == "hari",
      "Tanggal",
      "Bulan"
    )
    palet_warna <- c("#E32D91", "#4EA6DC", "#4775E7", "#8971E1", "#C830CC")
    
    # Menyiapkan data
    if(periode == "hari") {
      plot <- dat() %>% 
        select(date, statistik, city) %>% 
        mutate(city = str_to_title(city)) %>% 
        ggplot(aes(x = date, y = .data[[statistik]])) +
        geom_point(
          aes(color = city),
          size = 2,
          alpha = .2
        ) +
        scale_color_manual(
          values = palet_warna,
          name = "Kota"
        ) +
        theme_minimal() +
        labs(
          y = teks,
          x = judul_x
        )
    } else if (periode == "bulan") {
      nama_var <- paste0(statistik, "_mean")
      
      data <- dat() %>% 
        select(date, all_of(statistik), city) %>% 
        drop_na() %>% 
        mutate(bulan = format(date, "%Y-%m")) %>%
        group_by(city, bulan) %>%
        summarise_at(statistik, funs(mean)) %>% 
        mutate(city = str_to_title(city))
      plot <- data %>% 
        ggplot(aes(x = bulan, y = .data[[statistik]])) +
        geom_point(
          aes(color = city),
          size = 3,
          alpha = .8
        ) +
        scale_color_manual(
          values = palet_warna,
          name = "Kota"
        ) +
        theme_minimal() +
        labs(
          y = teks,
          x = judul_x
        )
      
    }
    
    ggplotly(plot) %>% 
      layout(
        legend = list(
          orientation = 'h', y = 100, xanchor = "center", x = .5
        )
      )
  })
  
  ## Tabel ----
  output$tabel <- renderDataTable({
    periode <- input$periode
    
  })
  
}

# Aplikasi ----
shinyApp(ui, server)
