# Paket ----
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
options(shiny.mathjax.url = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js")

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
  title = "Derajat dan Radian",
  id = "derajat_rad",
  ## Sidebar ----
  sidebar = sidebar(
    ### Sidebar simulasi ----
    conditionalPanel(
      "input.derajat_rad === 'Simulasi'",
      sliderInput(
        "putaran",
        div("Putaran:", style = "font-weight: bold;"),
        min = 0, max = 1, value = .125, step = .005,
        ticks = FALSE
      ),
      sliderInput(
        "radius",
        div("Jari-jari:", style = "font-weight: bold;"),
        min = .5, max = 10, value = 1, step = .5,
        ticks = FALSE
      ),
      br(),
      p("Pengaturan grafik:", style = "font-weight: bold;"),
      checkboxInput(
        "balik",
        "Balik koordinat"
      ),
      checkboxInput(
        "model",
        "Tampilkan model"
      ),
      br(),
      p(
        actionButton(
          "tombol_simpan",
          "Simpan data",
          width = "100%"
        )
      ),
      p(
        actionButton(
          "tombol_hapus",
          "Hapus data",
          width = "100%"
        )
      )
    ),
    ### Sidebar informasi ----
    conditionalPanel(
      "input.derajat_rad === 'Informasi'",
      h4(
        "Deskripsi",
        style = "font-weight: bold; font-size: inherit;"
      ),
      p(
        "Aplikasi Shiny ini mensimulasikan hubungan antara ukuran sudut derajat dan radian."
      ),
      hr(),
      h4(
        "Lisensi MIT",
        style = "font-weight: bold; font-size: inherit;"
      ),
      p(
        "Copyright © 2024 Yosep Dwi Kristanto"
      )
    )
  ),
  ## Simulasi ----
  nav_panel(
    title = "Simulasi",
    icon = icon("circle-play"),
    layout_columns(
      value_box(
        title = "Panjang Busur",
        value = textOutput("busur"),
        showcase = icon("ruler")
      ),
      value_box(
        title = "Derajat",
        value = textOutput("derajat"),
        showcase = icon("compass-drafting")
      ),
      value_box(
        title = "Radian",
        value = textOutput("radian"),
        showcase = icon("circle-notch")
      ),
      card(
        plotOutput("lingkaran"),
        full_screen = TRUE
      ),
      navset_card_underline(
        nav_panel(
          title = "Grafik",
          plotOutput("grafik_data")
        ),
        nav_panel(
          title = "Tabel",
          tableOutput("tabel_data")
        ),
        full_screen = TRUE
      ),
      col_widths = c(4, 4, 4, 5, 7),
      row_heights = c(1, 3)
    )
  ),
  ## Informasi ----
  nav_panel(
    title = "Informasi",
    icon = icon("circle-info"),
    layout_column_wrap(
      width = 1 / 2,
      navset_card_underline(
        #### Tentang ----
        nav_panel(
          title = "Tentang",
          p("Aplikasi berbasis web ini bertujuan untuk menunjukkan hubungan antara derajat dan radian. Melalui aplikasi ini, peserta didik dapat mencatat ukuran sudut dalam derajat dan radian untuk kemudian divisualisasikan ke dalam grafik. Melalui pengamatan terhadap grafik tersebut, peserta didik diharapkan akan dapat mengetahui bahwa kedua kuantitas tersebut proporsional atau sebanding. Selain itu, mereka juga diharapkan dapat menemukan radian sebagai fungsi terhadap derajat, dan demikian juga sebaliknya."),
          p("Bagi pendidik, aplikasi ini dapat digunakan untuk memfasilitasi peserta didiknya bermatematika. Aplikasi ini cocok untuk pembelajaran klasikal, penyelidikan dalam kelompok kecil maupun individu. Sebagai saran, pendidik dapat menyiapkan beberapa masalah tentang hubungan antara derajat dan radian sebagai panduan peserta didik untuk melakukan penyelidikan matematis.")
        ),
        nav_panel(
          #### Alat ----
          title = "Alat",
          p("Aplikasi ini dikembangkan dengan menggunakan bahasa pemrograman", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah. Tata letak dasbor ini diatur dengan menggunakan ", a("bslib.", href = "https://CRAN.R-project.org/package=bslib", target = "_blank"))
        ),
        nav_panel(
          #### Pengembang ----
          title = "Pengembang",
          p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta.")
        ),
        nav_panel(
          #### Kode Sumber ----
          title = "Kode Sumber",
          p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/apl-derajat-rad", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-derajat-rad/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-derajat-rad/pulls", target = "_blank"), "di repositori tersebut.")
        )
      ),
      #### Matematika ----
      card(
        card_header(
          "Hubungan Antara Derajat dan Radian"
        ),
        card_body(
          withMathJax(),
          p("Hubungan antara ukuran sudut derajat dan radian adalah sebagai berikut."),
          p("$$180^{\\circ}=\\pi \\text{ rad}$$"),
          tags$ul(
            tags$li("Untuk mengkonversi derajat ke radian, kalikan dengan \\(\\frac{\\pi}{180}\\)."),
            tags$li("Untuk mengkonversi radian ke derajat, kalikan dengan \\( \\frac{180}{\\pi}\\).")
          )
        )
      )
    )
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

# Peladen ----
server <- function(input, output, session) {
  ## busur ----
  output$busur <- renderText({
    putaran <- as.numeric(input$putaran)
    radius <- as.numeric(input$radius)
    round(putaran * 2 * pi * radius, 2)
  })
  
  ## derajat ----
  output$derajat <- renderText({
    putaran <- as.numeric(input$putaran)
    paste0(round(360 * putaran, 2), "°")
  })
  
  ## radian ----
  output$radian <- renderText({
    putaran <- as.numeric(input$putaran)
    paste0(round(2 * pi * putaran, 2), " rad")
  })
  
  ## tabel_data awal ----
  tabel_data <- reactiveVal(
    data.frame(
      putaran = numeric(),
      panjang_busur = numeric(),
      derajat = numeric(),
      radian = numeric()
    )
  )
  
  ## Tambahkan baris tabel_data ----
  observeEvent(input$tombol_simpan, {
    putaran <- as.numeric(input$putaran)
    radius <- as.numeric(input$radius)
    panjang_busur <- putaran * 2 * pi * radius
    derajat <- 360 * putaran
    radian <- 2 * pi * putaran
    
    tabel_data() %>% 
      add_row(
        putaran = putaran,
        panjang_busur = round(panjang_busur, 2),
        derajat = round(derajat, 2),
        radian = round(radian, 2)
      ) %>% 
      tabel_data()
  })
  
  ## Hapus data ----
  observeEvent(input$tombol_hapus, {
    tabel_data(
      data.frame(
        putaran = numeric(),
        panjang_busur = numeric(),
        derajat = numeric(),
        radian = numeric()
      )
    )
  })
  
  ## lingkaran ----
  output$lingkaran <- renderPlot({
    putaran <- as.numeric(input$putaran)
    radius <- as.numeric(input$radius)
    sudut <- putaran * 2 * pi
    
    # Lingkaran
    k_lingkaran <- function(pusat = c(0,0), r = 1, n = 360){
      tt <- seq(0, 2 * pi, length.out = n)
      xx <- pusat[1] + r * cos(tt)
      yy <- pusat[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
    data_lingkaran <- k_lingkaran(r = radius)
    
    # Busur lingkaran
    b_lingkaran <- function(awal = 0, akhir = 2 * pi, pusat = c(0,0), r = 1) {
      n <- round(akhir / (2 * pi) * 360)
      tt <- seq(awal, akhir, length.out = n)
      xx <- pusat[1] + r * cos(tt)
      yy <- pusat[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
    data_busur <- b_lingkaran(akhir = sudut, r = radius)
    
    # Plot
    ggplot() +
      geom_hline(
        yintercept = 0
      ) +
      geom_vline(
        xintercept = 0
      ) +
      geom_segment(
        aes(
          x = 0, y = 0,
          xend = 6.5 / 5 * radius * cos(sudut),
          yend = 6.5 / 5 * radius * sin(sudut)
        ),
        linewidth = 2, color = "#4775E7",
        arrow = arrow(
          angle = 20,
          type = "closed"
        )
      ) +
      geom_point(
        aes(x = 0, y = 0),
        size = 4
      ) +
      geom_path(
        aes(x = x, y = y),
        data = data_lingkaran,
        linewidth = 1.5
      ) +
      geom_path(
        aes(x = x, y = y),
        data = data_busur,
        linewidth = 2,
        color = "#E32D91",
        arrow = arrow(
          angle = 20,
          type = "closed"
        )
      ) +
      geom_point(
        aes(
          x = radius * cos(sudut),
          y = radius * sin(sudut)
        ),
        size = 4, color = "#4775E7"
      ) +
      xlim(-6.5 / 5 * radius, 6.5 / 5 * radius) +
      ylim(-6.5 / 5 * radius, 6.5 / 5 * radius) +
      coord_fixed() +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_blank()
      )
  })
  
  ## grafik_data ----
  output$grafik_data <- renderPlot({
    data = tabel_data()
    
    plot <- data %>% 
      ggplot(aes(x = derajat, y = radian)) +
      geom_point(
        size = 4
      ) +
      scale_x_continuous(
        breaks = seq(from = 0, to = 360, by = 30),
        labels = scales::label_number(suffix = "\u00b0"),
        limits = c(0, 360)
      ) +
      ylim(0, 2 * pi) +
      theme_bw(base_size = 14) +
      theme(
        plot.title = element_text(
          face = "bold"
        )
      ) +
      labs(
        title = "Hubungan Antara Derajat dan Radian",
        x = "Ukuran Sudut (Derajat)",
        y = "Ukuran Sudut (Radian)"
      )
    if (input$balik == TRUE) {
      plot <- plot +
        coord_flip()
    }
    if (input$model == TRUE) {
      plot <- plot +
        geom_smooth(
          method = "lm",
          formula = y ~ x,
          se = FALSE
        )
    }
    plot
  })
  
  ## tabel_data ----
  output$tabel_data <- renderTable(
    tabel_data() %>% 
      rename(
        Putaran = putaran,
        `Panjang Busur` = panjang_busur,
        Derajat = derajat,
        Radian = radian
      ),
    striped = TRUE,
    hover = TRUE,
    rownames = TRUE,
    width = "100%"
  )
  
}

# Aplikasi Shiny ----
shinyApp(ui = ui, server = server)
