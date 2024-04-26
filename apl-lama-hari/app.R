library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)

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
  "Lama hari (jam)" = "day_length",
  "Lama malam (jam)" = "night_length",
  "Perbedaan siang malam (jam)" = "diff",
  "Matahari terbit" = "sunrise_time",
  "Matahari terbenam" = "sunset_time",
  "Fajar" = "CT_start",
  "Senja" = "CT_end",
  "Tengah hari" = "SN_time",
  "Jarak matahari (ribu km)" = "SN_mil_km"
)
daftar_statistik_simpel <- c(
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
  title = "Kalkulator Matahari",
  id = "matahari",
  ## Sidebar ----
  sidebar = sidebar(
    accordion(
      ### Statistik ----
      accordion_panel(
        title = "Statistik",
        selectInput(
          "statistik",
          div("Statistik:", style = "font-weight: bold;"),
          daftar_statistik_simpel,
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
          selected = c("denpasar", "perth"),
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
  ## Eksplorasi ----
  nav_panel(
    title = "Eksplorasi",
    ### Penyajian data ----
    navset_card_underline(
      title = "Penyajian Data",
      nav_panel(
        title = "Diagram",
        plotlyOutput("diagram")
      ),
      ### Tabel ----
      nav_panel(
        title = "Tabel",
        DTOutput("tabel")
      ),
      full_screen = TRUE
    ),
    icon = shiny::icon("chart-simple")
  ),
  ## Informasi ----
  nav_panel(
    title = "Informasi",
    layout_column_wrap(
      width = 1 / 2,
      navset_card_underline(
        ### Tentang ----
        nav_panel(
          title = "Tentang",
          p("Dasbor ini bertujuan untuk memvisualisasikan data tentang matahari. Misalnya, Anda dapat melihat dan membandingkan lamanya hari antara dua kota setiap waktunya. Anda juga dapat melihat kapan matahari terbit dan terbenam setiap waktunya."),
          p("Bagi pendidik, aplikasi ini dapat digunakan untuk memfasilitasi peserta didiknya bermatematika. Aplikasi ini cocok untuk pembelajaran klasikal, penyelidikan dalam kelompok kecil maupun individu. Sebagai saran, pendidik dapat menyiapkan beberapa masalah tentang pemodelan matematika.")
        ),
        nav_panel(
          ### Alat ----
          title = "Alat",
          p("Aplikasi ini dikembangkan dengan menggunakan bahasa pemrograman", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah. Tata letak dasbor ini diatur dengan menggunakan ", a("bslib.", href = "https://CRAN.R-project.org/package=bslib", target = "_blank"), " Visualisasi datanya menggunakan ", a("ggplot2", href = "https://ggplot2.tidyverse.org", target = "_blank"), " dan ", a("plotly.", href = "https://plotly-r.com", target = "_blank"))
        ),
        nav_panel(
          ### Pengembang ----
          title = "Pengembang",
          p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta.")
        ),
        nav_panel(
          ### Kode Sumber ----
          title = "Kode Sumber",
          p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/matematika-tl-11", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/matematika-tl-11/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/matematika-tl-11/pulls", target = "_blank"), "di repositori tersebut.")
        )
      ),
      ### Data ----
      card(
        card_header(
          "Data"
        ),
        card_body(
          p(
            "Aplikasi ini menggunakan data yang diunduh dari ", a("www.timeanddate.com", href = "https://www.timeanddate.com/", target = "_blank"), " pada 22 April 2024."
          )
        )
      )
    ),
    icon = shiny::icon("circle-info")
  ),
  nav_spacer(),
  nav_menu(
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
      ) %>% 
      mutate(
        night_length = 24 - day_length,
        sunrise_time = as.POSIXct(sunrise_time, format = "%H:%M"),
        sunset_time = as.POSIXct(sunset_time, format = "%H:%M"),
        CT_start = as.POSIXct(CT_start, format = "%H:%M"),
        CT_end = as.POSIXct(CT_end, format = "%H:%M"),
        SN_time = as.POSIXct(SN_time, format = "%H:%M")
      ) %>% 
      mutate(
        diff = day_length - night_length
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
        select(date, tidyselect::all_of(statistik), city) %>% 
        mutate(city = str_to_title(city)) %>% 
        ggplot(aes(x = date, y = .data[[statistik]])) +
        geom_point(
          aes(
            color = city,
            text = sprintf(
              "<b>Kota: %s</b><br>Tanggal: %s<br>%s: %s",
              city, date, teks, .data[[statistik]]
            )
          ),
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
        select(date, tidyselect::all_of(statistik), city) %>% 
        drop_na() %>% 
        mutate(bulan = floor_date(date, "month")) %>%
        group_by(city, bulan) %>%
        summarise_at(statistik, .funs = list(mean)) %>% 
        mutate(city = str_to_title(city))
      plot <- data %>% 
        ggplot(aes(x = bulan, y = .data[[statistik]])) +
        geom_point(
          aes(
            color = city,
            text = sprintf(
              "<b>Kota: %s</b><br>Bulan: %s<br>%s: %s",
              city, bulan, teks, .data[[statistik]]
            )
          ),
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
    
    ggplotly(plot, tooltip = c("text")) %>% 
      layout(
        legend = list(
          orientation = 'h', y = 100, xanchor = "center", x = .5
        )
      )
  })
  
  ## Data table ----
  dat_table <- reactive({
    periode <- input$periode
    kota <- input$kota
    min_tanggal <- input$interval[1]
    maks_tanggal <- input$interval[2]
    statistik <- input$statistik
    nama_kolom <- names(daftar_statistik)[which(daftar_statistik %in% statistik)] %>% 
      str_to_title()
    
    # Menyiapkan data_awal
    data_awal <- sun_data %>% 
      mutate(
        day_length = round(day_length, 2),
        night_length = round(24 - day_length, 2)
      ) %>% 
      mutate(
        diff = round(day_length - night_length, 2)
      ) %>% 
      filter(
        city %in% kota,
        date >= min_tanggal,
        date <= maks_tanggal
      )
    
    if(periode == "hari") {
      data <- data_awal %>% 
        select(
          country,
          city,
          date,
          all_of(statistik)
        ) %>% 
        rename(
          Negara = country,
          Kota = city,
          Tanggal = date,
          !!nama_kolom := .data[[statistik]]
        ) %>% 
        mutate(
          Kota = str_to_title(str_replace_all(Kota, "-", " "),),
          Negara = str_to_title(str_replace_all(Negara, "-", " "),)
        )
    } else if(periode == "bulan") {
      data <- data_awal %>% 
        mutate(
          Bulan = format(date, "%Y-%m"),
          sunrise_time = as.POSIXct(sunrise_time, format = "%H:%M"),
          sunset_time = as.POSIXct(sunset_time, format = "%H:%M"),
          CT_start = as.POSIXct(CT_start, format = "%H:%M"),
          CT_end = as.POSIXct(CT_end, format = "%H:%M"),
          SN_time = as.POSIXct(SN_time, format = "%H:%M")
        ) %>% 
        group_by(country, city, Bulan) %>% 
        summarise_at(statistik, .funs = list(mean)) %>% 
        rename(
          Negara = country,
          Kota = city
        ) %>% 
        mutate(
          !!statistik := ifelse(
            statistik %in% c(
              "sunrise_time", "sunset_time", 
              "CT_start", "CT_end", "SN_time"
            ),
            format(.data[[statistik]], format = "%H:%M"),
            round(.data[[statistik]], 2)
          ),
          Kota = str_to_title(str_replace_all(Kota, "-", " "),),
          Negara = str_to_title(str_replace_all(Negara, "-", " "),)
        ) %>% 
        rename(!!nama_kolom := .data[[statistik]])
    }
    
    data
    
  })
  
  ## Tabel ----
  output$tabel <- renderDT(
    dat_table(),
    options = list(
      pageLength = 5
    )
  )
  
}

# Aplikasi ----
shinyApp(ui, server)
