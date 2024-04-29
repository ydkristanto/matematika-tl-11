library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(scales)
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
  "Ringkasan" = "ringkasan",
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
          selected = "ringkasan"
        ),
        conditionalPanel(
          "input.statistik != 'ringkasan'",
          selectInput(
            "periode",
            div("Periode:", style = "font-weight: bold;"),
            c(
              "Harian" = "hari",
              "Bulanan" = "bulan"
            ),
            selected = "hari"
          )
        ),
        icon = shiny::icon("chart-line")
      ),
      ### Filter ----
      accordion_panel(
        title = "Filter",
        selectizeInput(
          "kota",
          div("Kota:", style = "font-weight: bold;"),
          nama_kota,
          selected = c("denpasar", "perth"),
          multiple = TRUE,
          options = list(maxItems = 3)
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
            "Aplikasi ini menggunakan data yang diunduh dari ", a("www.timeanddate.com", href = "https://www.timeanddate.com/", target = "_blank"), " pada 22 April 2024. Data tersebut terdiri dari beberapa variabel untuk 13 kota, merentang dari 1 Januari 2021 sampai 31 Desember 2030. Variabel-variabel tersebut adalah sebagai berikut."
          ),
          tags$ul(
            tags$li(tags$b("Lama hari:"), " Lamanya hari (dalam jam) dari matahari terbit sampai terbenam."),
            tags$li(tags$b("Lama malam:"), " Lamanya malam (dalam jam) dari matahari terbenam sampai terbit."),
            tags$li(tags$b("Perbedaan siang malam:"), " Selisih antara lama siang dan malam."),
            tags$li(tags$b("Matahari terbit:"), " Waktu terbitnya matahari."),
            tags$li(tags$b("Matahari terbenam:"), " Waktu terbenamnya matahari."),
            tags$li(tags$b("Fajar:"), " Waktu sebelum matahari terbit, ketika pusat geometris matahari 6° di bawah cakrawala. (Pengertian ini mengacu pada ", tags$i("civil twilight,"), " salah satu ", a("jenis ", tags$i("twilight"), href = "https://www.weather.gov/lmk/twilight-types", target = "_blank"), " dari weather.gov.)"),
            tags$li(tags$b("Senja:"), " Waktu setelah matahari terbenam, ketika pusat geometris matahari 6° di bawah cakrawala. (Pengertian ini mengacu pada ", tags$i("civil twilight,"), " salah satu ", a("jenis ", tags$i("twilight"), href = "https://www.weather.gov/lmk/twilight-types", target = "_blank"), " dari weather.gov.)"),
            tags$li(tags$b("Tengah hari:"), " Waktu ketika matahari melalui posisi garis bujur (meridian) dan mencapai posisi tertingginya di langit. Untuk banyak kasus, tengah hari ini tidak terjadi pukul 12:00 PM."),
            tags$li(tags$b("Jarak matahari:"), " Jarak suatu tempat terhadap matahari (dalam ribuan km).")
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
  
  dat_ringkasan <- reactive({
    kota <- input$kota
    min_tanggal <- input$interval[1]
    maks_tanggal <- input$interval[2]
    
    # Merapikan data
    data <- sun_data %>% 
      select(
        country, city, date, sunrise_time, sunset_time,
        AST_start, AST_end, NT_start, NT_end, CT_start, CT_end, SN_time
      ) %>% 
      filter(
        city %in% kota,
        date >= min_tanggal,
        date <= maks_tanggal
      ) %>% 
      rename(
        day_start = sunrise_time,
        day_end = sunset_time
      ) %>% 
      mutate(
        day_start = hms::parse_hm(day_start),
        day_end = hms::parse_hm(day_end),
        CT_start = hms::parse_hm(CT_start),
        CT_end = hms::parse_hm(CT_end),
        NT_start = hms::parse_hm(NT_start),
        NT_end = hms::parse_hm(NT_end),
        SN_time = hms::parse_hm(SN_time)
      )
    
    # Menambahkan event ----
    malam1_dat <- data %>% 
      select(country, city, date, NT_start, SN_time) %>% 
      mutate(
        start = hms::parse_hm("00:00"),
        event = rep("Malam", nrow(data))
      ) %>% 
      rename(end = NT_start)
    fajar_bhr_dat <- data %>% 
      select(country, city, date, NT_start, CT_start, SN_time) %>% 
      mutate(
        event = rep("Fajar Bahari", nrow(data))
      ) %>% 
      rename(start = NT_start, end = CT_start)
    fajar_dat <- data %>% 
      select(country, city, date, CT_start, day_start, SN_time) %>% 
      mutate(
        event = rep("Fajar", nrow(data))
      ) %>% 
      rename(start = CT_start, end = day_start)
    siang_dat <- data %>% 
      select(country, city, date, day_start, day_end, SN_time) %>% 
      mutate(event = rep("Siang", nrow(data))) %>% 
      rename(start = day_start, end = day_end)
    senja_dat <- data %>% 
      select(country, city, date, day_end, CT_end, SN_time) %>% 
      mutate(
        event = rep("Senja", nrow(data))
      ) %>% 
      rename(start = day_end, end = CT_end)
    senja_bhr_dat <- data %>% 
      select(country, city, date, CT_end, NT_end, SN_time) %>% 
      mutate(
        event = rep("Senja Bahari", nrow(data))
      ) %>% 
      rename(start = CT_end, end = NT_end)
    malam2_dat <- data %>% 
      select(country, city, date, NT_end, SN_time) %>% 
      mutate(
        end = hms::parse_hm("24:00"),
        event = rep("Malam 2", nrow(data))
      ) %>% 
      rename(start = NT_end)
    
    data_rapi <- bind_rows(
      malam1_dat,
      fajar_bhr_dat,
      fajar_dat,
      siang_dat,
      senja_dat,
      senja_bhr_dat,
      malam2_dat
    ) %>% 
      mutate(
        city = str_to_title(str_replace(city, "-", " ")),
        event = factor(
          event,
          c(
            "Malam", "Fajar Bahari", "Fajar", "Siang",
            "Senja", "Senja Bahari", "Malam 2"
          )
        )
      ) %>% 
      select(country, city, date, event, start, end, SN_time) %>% 
      arrange(country, city, date)
    
    data_rapi
    
  })
  
  ## Diagram ----
  output$diagram <- renderPlotly({
    periode <- input$periode
    statistik <- input$statistik
    kota <- input$kota
    teks <- names(daftar_statistik)[which(daftar_statistik %in% statistik)]
    judul_x <- ifelse(
      periode == "hari",
      "Tanggal",
      "Bulan"
    )
    palet_warna <- c("#E32D91", "#4EA6DC", "#4775E7", "#8971E1", "#C830CC")
    
    # Menyiapkan data
    ### Diagram matahari ----
    if(statistik == "ringkasan") {
      if (length(kota) >= 2) {
        plot <- dat_ringkasan() %>% 
          ggplot(
            aes(x = date, ymin = start, ymax = end, fill = event)
          ) + 
          geom_ribbon() + 
          geom_point(
            aes(
              x = date, y = start, color = city,
              text = sprintf(
                "<b>Kota</b>: %s<br>Tanggal: %s<br>Waktu: %s",
                city, date, hms::as_hms(start)
              )
            ),
            alpha = 0
          ) + 
          geom_line(
            aes(
              x = date, y = SN_time, group = city,
              text = sprintf(
                "<b>Kota</b>: %s<br>Tanggal: %s<br>Waktu: %s",
                city, date, hms::as_hms(SN_time)
              )
            ),
            color = "yellow"
          ) +
          scale_fill_manual(
            name = NULL,
            values = c(
              "Malam" = "#2f464e",
              "Fajar Bahari" = "#5a7781",
              "Fajar" = "#86a9b5",
              "Siang" = "#b1dae8",
              "Senja" = "#86a9b5",
              "Senja Bahari" = "#5a7781",
              "Malam 2" = "#2f464e"
            )
          ) + 
          scale_y_time(
            limits = c(hms::parse_hm("00:00"), hms::parse_hm("24:00")),
            labels = scales::label_time(format = "%H:%M"),
            breaks = hms::hms(hours = seq(0, 24, 2))
          ) + 
          facet_wrap(vars(city), nrow = 1) + 
          theme_minimal() + 
          theme(
            legend.position = "bottom",
            strip.text.x = element_text(
              size = 12, face = "bold"
            )
          ) + 
          labs(
            x = "Tanggal",
            y = "Waktu"
          )
      } else if((length(kota) == 1)) {
        plot <- dat_ringkasan() %>% 
          ggplot(
            aes(x = date, ymin = start, ymax = end, fill = event)
          ) + 
          geom_ribbon() + 
          geom_point(
            aes(
              x = date, y = start, color = city,
              text = sprintf(
                "<b>Kota</b>: %s<br>Tanggal: %s<br>Waktu: %s",
                city, date, hms::as_hms(start)
              )
            ),
            alpha = 0
          ) + 
          geom_line(
            aes(
              x = date, y = SN_time, group = city,
              text = sprintf(
                "<b>Kota</b>: %s<br>Tanggal: %s<br>Waktu: %s",
                city, date, hms::as_hms(SN_time)
              )
            ),
            color = "yellow"
          ) +
          scale_fill_manual(
            name = NULL,
            values = c(
              "Malam" = "#2f464e",
              "Fajar Bahari" = "#5a7781",
              "Fajar" = "#86a9b5",
              "Siang" = "#b1dae8",
              "Senja" = "#86a9b5",
              "Senja Bahari" = "#5a7781",
              "Malam 2" = "#2f464e"
            )
          ) + 
          scale_y_time(
            limits = c(hms::parse_hm("00:00"), hms::parse_hm("24:00")),
            labels = scales::label_time(format = "%H:%M"),
            breaks = hms::hms(hours = seq(0, 24, 2))
          ) + 
          theme_minimal() + 
          theme(
            legend.position = "bottom"
          ) + 
          labs(
            x = "Tanggal",
            y = "Waktu"
          )
      } else if((length(kota) == 0)) {
        plot <- dat_ringkasan() %>% 
          ggplot() + 
          geom_blank() +
          scale_y_time(
            limits = c(hms::parse_hm("00:00"), hms::parse_hm("24:00")),
            labels = scales::label_time(format = "%H:%M"),
            breaks = hms::hms(hours = seq(0, 24, 2))
          ) + 
          theme_minimal() + 
          theme(
            legend.position = "bottom"
          ) + 
          labs(
            x = "Tanggal",
            y = "Waktu"
          )
      }
    } else if(periode == "hari" && statistik != "ringkasan") {
      ## Diagram pencar ----
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
    if (statistik != "ringkasan") {
      ggplotly(plot, tooltip = c("text")) %>% 
        layout(
          legend = list(
            orientation = 'h', y = 100, xanchor = "center", x = .5
          )
        )
    } else {
      ggplotly(plot, tooltip = c("text")) %>% 
        style(showlegend = FALSE)
    }
    
  })
  
  ## Data table ----
  tabel_dat <- reactive({
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
  
  tabel_dat_ringkasan <- reactive({
    dat_ringkasan() %>% 
      mutate(
        country = ifelse(
          country %in% c("usa", "uk"),
          toupper(country),
          str_to_title(country)
        ),
        event = ifelse(
          event == "Malam 2",
          "Malam",
          as.character(event)
        )
      ) %>% 
    rename(
      Negara = country,
      Kota = city,
      Tanggal = date,
      Kejadian = event,
      Mulai = start,
      Berakhir = end,
      `Tengah Hari` = SN_time
    )
  })
  
  ## Tabel ----
  output$tabel <- renderDT(
    if (input$statistik == "ringkasan") {
      tabel_dat_ringkasan()
    } else {
      tabel_dat()
    },
    options = list(
      pageLength = 5
    )
  )
  
}

# Aplikasi ----
shinyApp(ui, server)
