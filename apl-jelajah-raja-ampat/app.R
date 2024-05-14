library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
options(shiny.mathjax.url = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js")

# Data ----
#' Sumber: J. Schrader, S. Moeljono, J. Tambing, C. Sattler, & H. Kreft
#' https://doi.org/10.3897/BDJ.8.e55275
#' Baca juga: J. Schrader, S. Moeljono, G. Keppel, & H. Kreft
#' https://doi.org/10.1111/ecog.04512
load(url("https://raw.githubusercontent.com/ydkristanto/matematika-tl-11/main/data/data_raja_ampat.RData"))

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
  title = "Jelajah Kepulauan Raja Ampat",
  ## Sidebar ----
  sidebar = sidebar(
    htmlOutput("gambar_fitur"),
    ### Analisis ----
    selectInput(
      "analisis",
      div("Tingkat Analisis:", style = "font-weight:bold;"),
      choices = c("Pulau", "Transek", "Subtransek" = "Plot"),
      selected = "Pulau"
    ),
    ### Model ----
    selectInput(
      "model",
      div("Model:", style = "font-weight:bold;"),
      choices = c(
        "Linear",
        "Pangkat",
        "Logaritma",
        "Eksponensial Negatif",
        "Fungsi Rasional",
        "Logistik",
        "Weibull"
      )
    ),
    ### Transformasi ----
    checkboxInput(
      "transform", "Transformasi luas",
      value = FALSE
    ),
    ### Plot model ----
    checkboxInput(
      "plot_model", "Plot model",
      value = TRUE
    ),
    ### Galat
    conditionalPanel(
      "input.plot_model",
      checkboxInput(
        "galat", "Tampilkan galat",
        value = FALSE
      )
    )
  ),
  ## Panel utama ----
  ### Eksplorasi ----
  nav_panel(
    title = "Eksplorasi",
    navset_card_underline(
      title = "Data dan Model",
      nav_panel(
        title = "Plot",
        layout_columns(
          plotlyOutput("plot"),
          div(
            withMathJax(),
            tags$h5("Model", style = "font-weight: bold;"),
            p("Model banyaknya spesies (\\( y \\)) tumbuhan berkayu terhadap luas wilayahnya (\\( x \\)) direpresentasikan ke dalam persamaan berikut."),
            uiOutput("pers_model")
          ),
          col_widths = c(8, 4)
        )
      ),
      nav_panel(
        title = "Data",
        tableOutput("tabel")
      )
    ),
    icon = shiny::icon("chart-simple")
  ),
  ### Informasi ----
  nav_panel(
    title = "Informasi",
    icon = shiny::icon("circle-info"),
    layout_column_wrap(
      width = 1 / 2,
      navset_card_underline(
        #### Tentang ----
        nav_panel(
          title = "Tentang",
          markdown(
            "Aplikasi berbasis web ini bertujuan untuk memodelkan hubungan antara banyaknya spesies tumbuhan berkayu dan luas wilayah (*species–area relationship*, SAR) di Kepulauan Raja Ampat. Melalui aplikasi ini, Anda dapat menggunakan model-model matematis yang beragam, seperti model linear, eksponensial, dan logistik. Selain itu, Anda juga dapat membandingkan model-model tersebut untuk mendapatkan model yang paling baik.

Bagi pendidik, aplikasi ini dapat digunakan untuk memfasilitasi peserta didik bermatematika. Misalnya, permasalahan berikut dapat diajukan kepada peserta didik.

> Model manakah yang menurutmu paling baik? Bagaimana strategimu dalam membandingkan model-model tersebut?

Dalam menjawab permasalahan tersebut, peserta didik dapat berdiskusi dengan teman-temannya dalam kelompok. Untuk melakukannya, mereka dapat mengamati diagram pencar (dan modelnya) atau tabel yang disediakan. Diagram pencar tersebut dapat dilihat pada tab Plot sedangkan tabelnya dapat dilihat pada tab Data dalam laman Eksplorasi.

Diagram pencar tersebut memperlihatkan hubungan antara luas wilayah dan banyaknya spesies tumbuhan berkayu. Tampilannya tergantung dari tingkat analisis yang dipilih, yaitu pulau, transek, dan subtransek. Ketika pulau yang dipilih, variabel `Banyak Spesies` menyatakan total banyaknya spesies tumbuhan berkayu dalam pulau tersebut. Ketika transek yang dipilih, variabel tersebut menyatakan rata-rata banyaknya spesies di setiap transek dalam sebuah pulau. Hal ini juga sama ketika subtransek yang terpilih.

Tabel dalam tab Data memperlihatkan detail data yang ditampilkan pada diagram pencar. Data dalam tabel tersebut terdiri dari lima variabel, yaitu `ID Pulau`, `Luas` (atau transformasinya, yaitu `log(Luas)`), `Banyak Spesies`, `Banyak Spesies (Pred. Model)`, dan `Galat`. Tiga variabel pertama cukup jelas. Variabel `Banyak Spesies (Pred. Model)` menyatakan (rata-rata) banyaknya spesies yang diperoleh dengan menginputkan `Luas` ke dalam model yang dihasilkan. Variabel `Galat` merupakan selisih antara `Banyak Spesies` dan `Banyak Spesies (Pred. Model)`."
          )
        ),
        nav_panel(
          #### Alat ----
          title = "Alat",
          p("Aplikasi ini dikembangkan dengan menggunakan bahasa pemrograman", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah. Tata letak dasbor ini diatur dengan menggunakan ", a("bslib.", href = "https://CRAN.R-project.org/package=bslib", target = "_blank"), " Visualisasi datanya menggunakan ", a("ggplot2", href = "https://ggplot2.tidyverse.org", target = "_blank"), " dan ", a("plotly.", href = "https://plotly-r.com", target = "_blank"))
        ),
        nav_panel(
          #### Pengembang ----
          title = "Pengembang",
          p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta.")
        ),
        nav_panel(
          #### Kode Sumber ----
          title = "Kode Sumber",
          p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/matematika-tl-11", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/matematika-tl-11/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/matematika-tl-11/pulls", target = "_blank"), "di repositori tersebut.")
        )
      ),
      #### Data ----
      card(
        card_header(
          "Data"
        ),
        card_body(
          p("Aplikasi ini menggunakan data yang disediakan oleh Schrader dkk. Data tersebut dipublikasikan di dalam ", a(tags$i("Biodiversity Data Journal."), href = "https://doi.org/10.3897/BDJ.8.e55275", target = "_blank")),
          p("Data tersebut memuat banyak variabel. Variabel-variabel yang penting dalam aplikasi ini adalah tingkat analisis, luas pulau, dan banyak spesies tumbuhan berkayu. Tingkat analisisnya terbagi menjadi tiga, yaitu pulau, transek, dan subtransek. Transek merupakan wilayah persegi panjang di dalam pulau sedangkan subtransek merupakan wilayah persegi sebagai bagian dari transek. Untuk lebih jelasnya, perhatikan gambar berikut."),
          div(
            htmlOutput("keterangan_transek"),
            style = "text-align:center; text-size:0.8em;"
          )
        )
      )
    )
  ),
  nav_spacer(),
  ### Tautan ----
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

# Peladen ----
server <- function(input, output, session) {
  ## gambar_fitur ----
  output$gambar_fitur <- renderText({
    '<img src = "https://people.usd.ac.id/~ydkristanto/wp-content/uploads/2024/05/raja-ampat-feature-01.png" width = "100%">'
  })
  
  ## keterangan_transek ----
  output$keterangan_transek <- renderText({
    '
    <figure>
      <img src = "https://people.usd.ac.id/~ydkristanto/wp-content/uploads/2024/05/pulau-transek-subtransek-01.png" width = "80%" alt = "Transek dan substransek dalam pulau.">
      <figcaption style = "font-size:0.75em;">Ilustrasi pulau, transek (10 m × 2 m), dan subtransek (2 m × 2 m). Banyaknya transek dalam sebuah pulau tergantung dari luas pulau tersebut. Pulau yang lebih besar memuat lebih banyak transek. [Gambar diadaptasi dari Schrader, Moeljono, Tambing, Sattler, & Kreft (<a href = "https://doi.org/10.3897/BDJ.8.e55275.figure1" target = "_blank">2020</a>) / <a href = "https://creativecommons.org/licenses/by/4.0/deed.id" target = "_blank">CC BY 4.0 DEED</a>] </figcaption>
    </figure>
    '
  })
  
  ## Sidebar galat ----
  observeEvent(input$plot_model, {
    if (!input$plot_model) {
      updateCheckboxInput(getDefaultReactiveDomain(), "galat", value = FALSE)
    }
  })
  
  ## Data pulau ----
  dat_pulau <- reactive({
    data <- data_pulau %>% 
      select(island_ID, island_area, species_number) %>% 
      rename(
        ID_pulau = island_ID,
        luas_riil = island_area,
        banyak_spesies = species_number
      ) %>% 
      mutate(
        luas_trans = log10(luas_riil)
      )
    
    data
    
  })
  
  ## Data transek ----
  dat_transek <- reactive({
    n_spesies <- data_komunitas %>% 
      arrange(transect_ID, plot_ID) %>% 
      group_by(island_ID, transect_ID) %>% 
      distinct(species_ID) %>% 
      summarise(banyak_spesies = n(), .groups = "drop") %>% 
      rename(
        ID_pulau = island_ID,
        ID_transek = transect_ID
      )
    
    luas_pulau <- data_pulau %>% 
      select(island_ID, island_area) %>% 
      rename(
        ID_pulau = island_ID,
        luas_riil = island_area
      ) %>% 
      mutate(luas_trans = log10(luas_riil))
    
    spesies_nol <- data_pulau %>% 
      filter(species_number == 0) %>% 
      select(island_ID, species_number, island_area) %>% 
      mutate(
        ID_transek = NA,
        luas_trans = log10(island_area)
      ) %>% 
      rename(
        ID_pulau = island_ID,
        banyak_spesies = species_number,
        luas_riil = island_area
      ) %>% 
      select(
        ID_pulau, ID_transek, banyak_spesies, luas_riil, luas_trans
      )
    
    data <- left_join(
      n_spesies,
      luas_pulau,
      by = join_by(ID_pulau)
    ) %>% 
      bind_rows(spesies_nol) %>% 
      group_by(ID_pulau) %>% 
      summarise(
        banyak_spesies = mean(banyak_spesies, na.rm = TRUE),
        luas_riil = mean(luas_riil, na.rm = TRUE),
        luas_trans = mean(luas_trans, na.rm = TRUE),
        .groups = "drop"
      )
    
    data
    
  })
  
  ## Data transek ----
  dat_plot <- reactive({
    n_spesies <- data_komunitas %>% 
      arrange(transect_ID, plot_ID) %>% 
      group_by(island_ID, transect_ID, plot_ID) %>% 
      distinct(species_ID) %>% 
      summarise(banyak_spesies = n(), .groups = "drop") %>% 
      rename(
        ID_pulau = island_ID,
        ID_transek = transect_ID,
        ID_plot = plot_ID
      )
    
    luas_pulau <- data_pulau %>% 
      select(island_ID, island_area) %>% 
      rename(
        ID_pulau = island_ID,
        luas_riil = island_area
      ) %>% 
      mutate(luas_trans = log10(luas_riil))
    
    spesies_nol <- data_pulau %>% 
      filter(species_number == 0) %>% 
      select(island_ID, species_number, island_area) %>% 
      mutate(
        ID_transek = NA,
        ID_plot = NA,
        luas_trans = log10(island_area)
      ) %>% 
      rename(
        ID_pulau = island_ID,
        banyak_spesies = species_number,
        luas_riil = island_area
      ) %>% 
      select(
        ID_pulau, ID_transek, ID_plot, banyak_spesies, luas_riil, luas_trans
      )
    
    data <- left_join(
      n_spesies,
      luas_pulau,
      by = join_by(ID_pulau)
    ) %>% 
      bind_rows(spesies_nol) %>% 
      group_by(ID_pulau) %>% 
      summarise(
        banyak_spesies = mean(banyak_spesies, na.rm = TRUE),
        luas_riil = mean(luas_riil, na.rm = TRUE),
        luas_trans = mean(luas_trans, na.rm = TRUE),
        .groups = "drop"
      )
    
    data
    
  })
  
  ## dat ----
  dat <- reactive({
    # Memilih data
    if (input$analisis == "Pulau") {
      dat <- dat_pulau()
    } else if (input$analisis == "Transek") {
      dat <- dat_transek()
    } else if (input$analisis == "Plot") {
      dat <- dat_plot()
    }
    
    # Memilih variabel luas
    if (input$transform) {
      data <- dat %>% 
        mutate(luas = luas_trans) %>% 
        select(ID_pulau, luas, banyak_spesies)
    } else {
      data <- dat %>% 
        mutate(luas = luas_riil) %>% 
        select(ID_pulau, luas, banyak_spesies)
    }
    
    data
    
  })
  
  ## Model ----
  model <- reactive({
    # Nilai awal untuk nls()
    if(input$transform) {
      if(input$analisis == "Pulau") {
        c_pangkat <- 1.1
        z_pangkat <- 2.1
        c_log <- 2
        z_log <- 23
        c_eks <- -2.01532
        z_eks <- -.63726
        c_rasio <- -2.56523
        z_rasio <- 3.36139
        d_rasio <- -.130095
        d_logis <- 26.4334
        z_logis <- 1.56928
        f_logis <- 4.79603
        d_w <- 33.1737
        z_w <- 0.0226077
        f_w <- 2.78488
      } else if(input$analisis == "Transek") {
        c_pangkat <- 1.4
        z_pangkat <- 1.4
        c_log <- 1.5
        z_log <- 11.3
        c_eks <- -5.83774
        z_eks <- -.248316
        c_rasio <- -2.99824
        z_rasio <- 4.28796
        d_rasio <- .137511
        d_logis <- 7.60477
        z_logis <- 3.77735
        f_logis <- 6.26236
        d_w <- 7.56392
        z_w <- 0.0783054
        f_w <- 4.27308
      } else if(input$analisis == "Plot") {
        c_pangkat <- .8
        z_pangkat <- 1
        c_log <- .8
        z_log <- 3.6
        c_eks <- 16.3731
        z_eks <- 0.0544549
        c_rasio <- -1.64504
        z_rasio <- 2.883
        d_rasio <- .570726
        d_logis <- 2.69196
        z_logis <- 3.5155
        f_logis <- 4.84676
        d_w <- 2.67584
        z_w <- 0.239406
        f_w <- 3.3757
      }
    } else {
      if(input$analisis == "Pulau") {
        c_pangkat <- .9
        z_pangkat <- .3
        c_log <- -5.3
        z_log <- 6.2
        c_eks <- 19.4805
        z_eks <- 0.00105999
        c_rasio <- .664516
        z_rasio <- .0294826
        d_rasio <- .00133312
        d_logis <- 18.3346
        z_logis <- 0.00280074
        f_logis <- 2.11491
        d_w <- 21.9586
        z_w <- 0.0125515
        f_w <- 0.608648
      } else if(input$analisis == "Transek") {
        c_pangkat <- .9
        z_pangkat <- .2
        c_log <- -1.7
        z_log <- 2.8
        c_eks <- 7.58628
        z_eks <- 0.0124218
        c_rasio <- -.74314
        z_rasio <- .165243
        d_rasio <- .0207741
        d_logis <- 7.36549
        z_logis <- 0.0791336
        f_logis <- 3.18376
        d_w <- 7.5056
        z_w <- 0.00474649
        f_w <- 1.28947
      } else if(input$analisis == "Plot") {
        c_pangkat <- .5
        z_pangkat <- .2
        c_log <- -.1
        z_log <- .8
        c_eks <- 2.67309
        z_eks <- 0.0266384
        c_rasio <- -.375504
        z_rasio <- .134462
        d_rasio <- .0488608
        d_logis <- 2.66111
        z_logis <- 0.0870047
        f_logis <- 2.25108
        d_w <- 2.66049
        z_w <- 0.013117
        f_w <- 1.24835
      }
    }
    
    # Model
    if(input$model == "Linear") {
      mdl <- lm(
        formula = banyak_spesies ~ luas,
        data = dat()
      )
    } else if(input$model == "Pangkat") {
      mdl <- nls(
        banyak_spesies ~ c*(luas^z),
        data = dat(),
        start = list(c = c_pangkat, z = z_pangkat)
      )
    } else if(input$model == "Logaritma") {
      mdl <- nls(
        banyak_spesies ~ c + z * log10(luas),
        data = dat(),
        start = list(c = c_log, z = z_log)
      )
    } else if(input$model == "Eksponensial Negatif") {
      mdl <- nls(
        banyak_spesies ~ c * (1 - exp(-z * luas)),
        data = dat(),
        start = list(c = c_eks, z = z_eks)
      )
    } else if(input$model == "Fungsi Rasional") {
      mdl <- nls(
        banyak_spesies ~ (c + z * luas) / (1 + d * luas),
        data = dat(),
        start = list(c = c_rasio, z = z_rasio, d = d_rasio)
      )
    } else if(input$model == "Logistik") {
      mdl <- nls(
        banyak_spesies ~ d / (1 + exp(-z * luas + f)),
        data = dat(),
        start = list(d = d_logis, z = z_logis, f = f_logis)
      )
    } else if(input$model == "Weibull") {
      mdl <- nls(
        banyak_spesies ~ d * (1 - exp(-z * luas^f)),
        data = dat(),
        start = list(d = d_w, z = z_w, f = f_w)
      )
    }
    
    mdl
    
  })
  
  koef_model <- reactive({
    coefficients(model())
  })
  
  plot_model <- reactive({
    luas_min <- min(dat()$luas)
    luas_maks <- max(dat()$luas)
    luas <- seq(from = luas_min, to = luas_maks, length.out = 500)
    if(input$model == "Linear") {
      pred_spesies <- koef_model()[1] + koef_model()[2] * luas
    } else if(input$model == "Pangkat") {
      pred_spesies <- koef_model()["c"] * luas^(koef_model()["z"])
    } else if(input$model == "Logaritma") {
      pred_spesies <- koef_model()["c"] + koef_model()["z"] * log10(luas)
    } else if(input$model == "Eksponensial Negatif") {
      pred_spesies <- koef_model()["c"] * (1 - exp(-1 * koef_model()["z"] * luas))
    } else if(input$model == "Fungsi Rasional") {
      pred_spesies <- (koef_model()["c"] + koef_model()["z"] * luas) / (1 + koef_model()["d"] * luas)
    } else if(input$model == "Logistik") {
      pred_spesies <- koef_model()["d"] / (1 + exp(-1 * koef_model()["z"] * luas + koef_model()["f"]))
    } else if(input$model == "Weibull") {
      pred_spesies <- koef_model()["d"] * (1 - exp(-1 * koef_model()["z"] * (luas ^ koef_model()["f"])))
    }
    
    data <- tibble(
      luas = luas,
      pred_spesies = pred_spesies,
      keterangan = "model"
    )
    
    data
    
  })
  
  ## plot ----
  output$plot <- renderPlotly({
    trans <- input$transform
    pred <- fitted.values(model())
    residu <- residuals(model())
    xlab_luas <- ifelse(
      trans, "log(Luas)", "Luas"
    )
    
    # Data
    data <- dat() %>% 
      mutate(
        pred_banyak_spesies = pred,
        galat = residu
      )
    # Plot
    plot <- data %>% 
      ggplot(aes(x = luas, y = banyak_spesies)) + 
      list(
        theme_minimal(),
        if(input$galat) geom_segment(
          aes(
            x = luas, y = banyak_spesies,
            xend = luas, yend = pred_banyak_spesies
          ),
          linewidth = .4,
          alpha = .75,
          color = "#8971E1"
        ),
        geom_point(
          aes(
            text = sprintf(
              "<b>Luas:</b> %s<br><b>Banyak Spesies:</b> %s",
              round(luas, 2), round(banyak_spesies, 2)
            )
          ),
          color = "#E32D91",
          size = 3,
          alpha = .8
        ),
        if(input$plot_model) geom_line(
          data = plot_model(),
          aes(
            x = luas,
            y = pred_spesies
          ),
          color = "#4EA6DC",
          linewidth = 1
        ),
        if(input$plot_model) geom_point(
          data = plot_model(),
          aes(
            x = luas,
            y = pred_spesies,
            text = sprintf(
              "<b>Luas:</b> %s<br><b>Prediksi Banyak Spesies:</b> %s",
              round(luas, 2), round(pred_spesies, 2)
            )
          ),
          color = "#08519c",
          alpha = 0
        ),
        labs(
          x = xlab_luas,
          y = "Banyak Spesies"
        )
      )
    
    ggplotly(plot, tooltip = c("text"))
    
  })
  
  ## tabel ----
  output$tabel <- renderTable(
    striped = TRUE,
    hover = TRUE,
    width = "100%",{
    pred <- fitted.values(model())
    residu <- residuals(model())
    
    data <- dat() %>% 
      mutate(
        prediksi_banyak_spesies = round(pred, 2),
        galat = round(residu, 3)
      )
    if(input$transform) {
      data <- data %>% 
        rename(
        `ID Pulau` = ID_pulau,
        `log(Luas)` = luas,
        `Banyak Spesies` = banyak_spesies,
        `Banyak Spesies (Pred. Model)` = prediksi_banyak_spesies,
        `Galat` = galat
      )
    } else {
      data <- data %>% 
        rename(
          `ID Pulau` = ID_pulau,
          `Luas` = luas,
          `Banyak Spesies` = banyak_spesies,
          `Banyak Spesies (Pred. Model)` = prediksi_banyak_spesies,
          `Galat` = galat
        )
    }
    
    data
    
  })
  
  ## pers_model ----
  output$pers_model <- renderUI({
    # Model
    if(input$model == "Linear") {
      c <- round(koef_model()[1], 4)
      z <- round(koef_model()[2], 4)
      
      if(input$transform) {
        withMathJax(
          sprintf("$$y = %.03f \\log x %.03f$$", z, c)
        )
      } else {
        withMathJax(
          sprintf("$$y = %.03f x + %.03f$$", z, c)
        )
      }
      
    } else if(input$model == "Pangkat") {
      c <- round(koef_model()["c"], 4)
      z <- round(koef_model()["z"], 4)
      
      if(input$transform) {
        withMathJax(
          sprintf("$$y = %.03f{\\left( \\log x \\right)^{%.03f}}$$", c, z)
        )
      } else {
        withMathJax(
          sprintf("$$y = %.03f{x^{%.03f}}$$", c, z)
        )
      }
    } else if(input$model == "Logaritma") {
      c <- round(koef_model()["c"], 4)
      z <- round(koef_model()["z"], 4)
      
      if(input$transform) {
        withMathJax(
          sprintf("$$y = %.03f + %.03f \\log \\left( \\log x \\right)$$", c, z)
        )
      } else {
        withMathJax(
          sprintf("$$y = %.03f + %.03f \\log x$$", c, z)
        )
      }
    } else if(input$model == "Eksponensial Negatif") {
      c <- round(koef_model()["c"], 4)
      z <- round(koef_model()["z"], 4)
      
      if(input$transform) {
        withMathJax(
          sprintf("$$y = %.03f\\left( {1 - {e^{ - \\left( %.03f \\log x \\right)}}} \\right)$$", c, z)
        )
      } else {
        withMathJax(
          sprintf("$$y = %.03f \\left( {1 - {e^{ - %.03f x}}} \\right)$$", c, z)
        )
      }
    } else if(input$model == "Fungsi Rasional") {
      c <- round(koef_model()["c"], 4)
      z <- round(koef_model()["z"], 4)
      d <- round(koef_model()["d"], 4)
      
      if(input$transform) {
        withMathJax(
          sprintf("$$y = \\frac{{%.03f + %.03f \\log x}}{{1 + \\left( %.03f \\log x \\right)}}$$", c, z, d)
        )
      } else {
        withMathJax(
          sprintf("$$y = \\frac{{%.03f + %.03f x}}{{1 + %.03f x}}$$", c, z, d)
        )
      }
    } else if(input$model == "Logistik") {
      d <- round(koef_model()["d"], 4)
      z <- round(koef_model()["z"], 4)
      f <- round(koef_model()["f"], 4)
      
      if(input$transform) {
        withMathJax(
          sprintf("$$y = \\frac{%.03f}{{1 + {e^{\\left( { - %.03f \\log x + %.03f} \\right)}}}}$$", d, z, f)
        )
      } else {
        withMathJax(
          sprintf("$$y = \\frac{%.03f}{{1 + {e^{\\left( { - %.03f x + %.03f} \\right)}}}}$$", d, z, f)
        )
      }
    } else if(input$model == "Weibull") {
      d <- round(koef_model()["d"], 4)
      z <- round(koef_model()["z"], 4)
      f <- round(koef_model()["f"], 4)
      
      if(input$transform) {
        withMathJax(
          sprintf("$$y = %.03f\\left( {1 - {e^{\\left( { - %.03f{\\left( \\log x \\right)^{%.03f}}} \\right)}}} \\right)$$", d, z, f)
        )
      } else {
        withMathJax(
          sprintf("$$y = %.03f\\left( {1 - {e^{\\left( { - %.03f{x^{%.03f}}} \\right)}}} \\right)$$", d, z, f)
        )
      }
    }
  })
  
}

# Jalankan aplikasi ----
shinyApp(ui, server)

