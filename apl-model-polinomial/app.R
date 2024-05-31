library(shiny)
library(bslib)
library(tidyr)
library(dplyr)
library(purrr)
library(plotly)
library(scales)
library(broom)

# Data ----
data_model_polinomial <- load(
  url("https://raw.githubusercontent.com/ydkristanto/matematika-tl-11/main/data/data_model_polinom.RData")
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
        "Harga Rumah" = "rumah",
        "Preferensi Musik" = "musik"
      ),
      selected = "pangan"
    ),
    ### Wilayah ----
    conditionalPanel(
      "input.data == 'pangan' | input.data == 'gabah' | input.data == 'rumah'",
      uiOutput("wilayah")
    ),
    ### Wisatawan ----
    conditionalPanel(
      "input.data == 'borobudur'",
      selectInput(
        "wisatawan",
        div("Wisatawan:", style = "font-weight: bold;"),
        choices = c("Domestik", "Mancanegara"),
        selected = "Mancanegara",
        multiple = TRUE,
        selectize = TRUE
      )
    ),
    ### Derajat model ----
    sliderInput(
      "derajat",
      div("Derajat Polinomial:", style = "font-weight: bold;"),
      value = 1,
      min = 1,
      max = 10,
      step = 1,
      ticks = FALSE
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
    icon = shiny::icon("chart-simple"),
    navset_card_underline(
      title = "Penyajian Data",
      #### Plot ----
      nav_panel(
        title = "Plot",
        plotlyOutput("plot")
      ),
      #### Plot Galat ----
      nav_panel(
        title = "Plot Galat",
        plotlyOutput("plot_galat")
      ),
      #### Tabel ----
      nav_panel(
        title = "Tabel",
        tableOutput("tabel_data")
      )
    )
  ),
  nav_panel(
    ### Informasi ----
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
          
        )
      )
    )
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
  
  ## Input wilayah ----
  # Daftar provinsi
  prov_gabah <- data_gabah$wilayah |>
    unique() |> 
    setdiff(c("Indonesia", "Papua Barat Daya", "Papua Selatan", "Papua Tengah", "Papua Pegunungan"))
  prov_pangan <- data_pangan$wilayah |>
    unique() |> 
    setdiff(c("Indonesia"))
  prov_rumah <- data_rumah$wilayah |>
    unique() |> 
    setdiff(c("Indonesia"))
  pilihan_prov <- list(
    gabah = prov_gabah,
    pangan = prov_pangan,
    rumah = prov_rumah
  )
  # Memperbarui pilihan wilayah
  output$wilayah <- renderUI({
    selectizeInput(
      "wilayah",
      div("Wilayah:", style = "font-weight: bold;"),
      choices = list(
        Negara = as.list(c("Indonesia")),
        Provinsi = pilihan_prov[[input$data]]
      ),
      selected = "Indonesia",
      multiple = TRUE,
      options = list(maxItems = 5)
    )
  })
  
  ## Data ----
  data <- reactive({
    if (input$data == "borobudur") {
      data <- data_borobudur |> 
        group_by(wisatawan, bulan) |> 
        summarise(
          rerata = mean(frekuensi, na.rm = TRUE)
        ) |> 
        filter(wisatawan %in% input$wisatawan)
    } else if (input$data == "gabah") {
      data <- data_gabah |> 
        filter(wilayah %in% input$wilayah) |> 
        drop_na()
    } else if (input$data == "musik") {
      data <- data_musik
    } else if (input$data == "pangan") {
      data <- data_pangan |> 
        filter(wilayah %in% input$wilayah) |> 
        drop_na()
    } else if (input$data == "rumah") {
      data <- data_rumah |> 
        filter(wilayah %in% input$wilayah) |> 
        drop_na()
    }
    
    data
    
  })
  
  ## Perbarui derajat maksimum ----
  observe({
    if (input$data == "borobudur") {
      d_maks <- 10
    } else if (input$data == "gabah") {
      dat <- data_gabah |> 
        filter(wilayah %in% input$wilayah) |> 
        drop_na() |> 
        group_by(wilayah) |> 
        summarise(n = n())
      d_maks <- min(dat$n) - 1
    } else if (input$data == "musik") {
      d_maks <- 10
    } else if (input$data == "pangan") {
      dat <- data_pangan |> 
        filter(wilayah %in% input$wilayah) |> 
        drop_na() |> 
        group_by(wilayah) |> 
        summarise(n = n())
      d_maks <- min(dat$n) - 1
    } else if (input$data == "rumah") {
      dat <- data_rumah |> 
        filter(wilayah %in% input$wilayah) |> 
        drop_na() |> 
        group_by(wilayah) |> 
        summarise(n = n())
      d_maks <- min(dat$n) - 1
    }
    updateSliderInput(
      getDefaultReactiveDomain(),
      "derajat",
      max = d_maks
    )
  })
  
  ## Model ----
  model <- reactive({
    if(input$data == "borobudur") {
      if(length(input$wisatawan) == 1) {
        mdl <- lm(rerata ~ poly(bulan, degree = input$derajat, raw = FALSE),
                  data = data())
      } else {
        mdl <- data() |> 
          group_by(wisatawan) |> 
          do(model = lm(rerata ~ poly(bulan, degree = input$derajat, raw = FALSE), data = .))
      }
    } else if(input$data == "gabah") {
      if(length(input$wilayah) == 1) {
        mdl <- lm(produksi_ton ~ poly(
          tahun, degree = input$derajat, raw = FALSE
        ),
        data = data())
      } else {
        data_grup <- data() |> 
          group_by(wilayah) |> 
          nest()
        mdl <- data_grup |> 
          mutate(model = map(data, ~ lm(produksi_ton ~ poly(tahun, degree = input$derajat, raw = FALSE), data = .x)))
      }
    } else if(input$data == "musik") {
      mdl <- lm(rerata_rating ~ poly(
        usia_lagu, degree = input$derajat, raw = FALSE
      ),
      data = data())
    } else if(input$data == "pangan") {
      if(length(input$wilayah) == 1) {
        mdl <- lm(pou ~ poly(
          tahun, degree = input$derajat, raw = FALSE
        ),
        data = data())
      } else {
        data_grup <- data() |> 
          group_by(wilayah) |> 
          nest()
        mdl <- data_grup |> 
          mutate(model = map(data, ~ lm(pou ~ poly(tahun, degree = input$derajat, raw = FALSE), data = .x)))
      }
    } else if(input$data == "rumah") {
      if(length(input$wilayah) == 1) {
        mdl <- lm(harga ~ poly(
          tahun, degree = input$derajat, raw = FALSE
        ),
        data = data())
      } else {
        data_grup <- data() |> 
          group_by(wilayah) |> 
          nest()
        mdl <- data_grup |> 
          mutate(model = map(data, ~ lm(harga ~ poly(tahun, degree = input$derajat, raw = FALSE), data = .x)))
      }
    }
    
    mdl
  })
  
  ## Data model ----
  data_model <- reactive({
    if(input$data == "borobudur") {
      if(length(input$wisatawan) == 1) {
        pred <- fitted.values(model())
        residu <- residuals(model())
        data_model <- data() |> 
          mutate(pred = pred, residu = residu)
      } else {
        pred_res <- model() %>%
          do(augment(.$model)) |> 
          tail(24) |> 
          select(.fitted, .resid) |> 
          rename(
            pred = .fitted,
            residu = .resid
          )
        data_model <- bind_cols(data(), pred_res)
      }
    } else if(input$data == "gabah") {
      if(length(input$wilayah) == 1) {
        pred <- fitted.values(model())
        residu <- residuals(model())
        data_model <- data() |> 
          mutate(pred = pred, residu = residu)
      } else {
        data_aug <- model() |> 
          mutate(
            pred = map(model, fitted.values),
            residu = map(model, resid)
          ) |> 
          unnest(cols = c(data, pred, residu))
        pred_res <- data_aug %>%
          select(wilayah, tahun, produksi_ton, pred, residu)
        data_model <- pred_res |> 
          ungroup()
      }
    } else if(input$data == "musik") {
      pred <- fitted.values(model())
      residu <- residuals(model())
      pred_res <- tibble(pred = pred, residu = residu)
      data_model <- bind_cols(data(), pred_res)
    } else if(input$data == "pangan") {
      if(length(input$wilayah) == 1) {
        pred <- fitted.values(model())
        residu <- residuals(model())
        data_model <- data() |> 
          mutate(pred = pred, residu = residu)
      } else {
        data_aug <- model() |> 
          mutate(
            pred = map(model, fitted.values),
            residu = map(model, resid)
          ) |> 
          unnest(cols = c(data, pred, residu))
        pred_res <- data_aug %>%
          select(wilayah, tahun, pou, pred, residu)
        data_model <- pred_res |> 
          ungroup()
      }
    } else if(input$data == "rumah") {
      if(length(input$wilayah) == 1) {
        pred <- fitted.values(model())
        residu <- residuals(model())
        pred_res <- data.frame(pred = pred, residu = residu)
        data_model <- bind_cols(data(), pred_res)
      } else {
        data_aug <- model() |> 
          mutate(
            pred = map(model, fitted.values),
            residu = map(model, resid)
          ) |> 
          unnest(cols = c(data, pred, residu))
        pred_res <- data_aug %>%
          select(wilayah, tahun, harga, pred, residu)
        data_model <- pred_res |> 
          ungroup()
      }
    }
    
    data_model
    
  })
  
  ## Plot ----
  output$plot <- renderPlotly({
    if (input$data == "borobudur") {
      plot <- data() |> 
        ggplot(aes(x = bulan, y = rerata)) + 
        list(
          if (length(input$wisatawan) > 1) aes(color = wisatawan),
          if(input$galat) geom_segment(
            data = data_model(),
            aes(x = bulan, xend = bulan, y = pred, yend = rerata),
            linewidth = .5,
            alpha = .8
          ),
          geom_point(size = 2),
          geom_smooth(
            method = "lm",
            formula = y ~ poly(x, degree = input$derajat),
            se = FALSE
          ),
          if (length(input$wisatawan) > 1) scale_color_viridis_d(name = "Wisatawan"),
          scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)),
          theme_minimal(),
          labs(
            x = "Bulan",
            y = "Rerata Banyak Wisatawan"
          )
        )
    } else if (input$data == "gabah") {
      plot <- data() |> 
        ggplot(aes(x = tahun, y = produksi_ton)) + 
        list(
          if(length(input$wilayah) > 1) aes(color = wilayah),
          if(input$galat) geom_segment(
            data = data_model(),
            aes(x = tahun, xend = tahun, y = pred, yend = produksi_ton),
            linewidth = .5,
            alpha = .8
          ),
          geom_point(size = 2),
          geom_smooth(
            method = "lm",
            formula = y ~ poly(x, degree = input$derajat),
            se = FALSE
          ),
          if (length(input$wilayah) > 1) scale_color_viridis_d(name = "Wilayah"),
          theme_minimal(),
          labs(
            x = "Tahun",
            y = "Produksi Gabah (Ton)"
          )
        )
    } else if (input$data == "musik") {
      plot <- data() |> 
        ggplot(aes(x = usia_lagu, y = rerata_rating)) + 
        list(
          if(input$galat) geom_segment(
            data = data_model(),
            aes(x = usia_lagu, xend = usia_lagu,
                y = pred, yend = rerata_rating),
            linewidth = .5,
            alpha = .5
          ),
          geom_point(
            aes(color = rerata_rating),
            size = 2,
            show.legend = FALSE
          ),
          geom_smooth(
              method = "lm",
              formula = y ~ poly(x, degree = input$derajat),
              se = FALSE
            ),
          scale_color_viridis_b(),
          theme_minimal(),
          labs(
              x = "Usia Lagu",
              y = "Rerata Rating"
            )
        )
    } else if (input$data == "pangan") {
      plot <- data() |> 
        ggplot(aes(x = tahun, y = pou)) + 
        list(
          if(length(input$wilayah) > 1) aes(color = wilayah),
          if(input$galat) geom_segment(
            data = data_model(),
            aes(x = tahun, xend = tahun, y = pred, yend = pou),
            linewidth = .5,
            alpha = .8
          ),
          geom_point(size = 2),
          geom_smooth(
            method = "lm",
            formula = y ~ poly(x, degree = input$derajat),
            se = FALSE
          ),
          if (length(input$wilayah) > 1) scale_color_viridis_d(name = "Wilayah"),
          scale_y_continuous(labels = scales::label_percent(scale = 1)),
          theme_minimal(),
          theme(legend.title = element_text(hjust = .5)),
          labs(
            x = "Tahun",
            y = "Prevalensi Ketidakcukupan\nKonsumsi Pangan"
          )
        )
    } else if (input$data == "rumah") {
      plot <- data() |> 
        ggplot(aes(x = tahun, y = harga)) + 
        list(
          if(length(input$wilayah) > 1) aes(color = wilayah),
          if(input$galat) geom_segment(
            data = data_model(),
            aes(x = tahun, xend = tahun, y = pred, yend = harga),
            linewidth = .5,
            alpha = .8
          ),
          geom_point(size = 2),
          geom_smooth(
            method = "lm",
            formula = y ~ poly(x, degree = input$derajat),
            se = FALSE
          ),
          if (length(input$wilayah) > 1) scale_color_viridis_d(name = "Wilayah"),
          scale_x_continuous(breaks = seq(from = 2008, to = 2018, by = 2)),
          theme_minimal(),
          labs(
            x = "Tahun",
            y = "Harga Unit Perumahan"
          )
        )
    }
    
    ggplotly(plot)
  })
  
  ## Plot galat ----
  output$plot_galat <- renderPlotly({
    if (input$data == "borobudur") {
      plot <- data_model() |> 
        ggplot(aes(x = pred, y = residu)) + 
        list(
          geom_hline(yintercept = 0, linetype = "dashed"),
          geom_point(size = 2),
          geom_smooth(
            method = "loess",
            formula = y ~ x,
            se = FALSE
          ),
          if(length(input$wisatawan) > 1) facet_wrap(
            vars(wisatawan),
            scales = "free_x"
          ),
          theme_minimal(),
          labs(
            x = "Rerata Banyak Wisatawan (Pred. Model)",
            y = "Galat"
          )
        )
    } else if (input$data == "gabah") {
      plot <- data_model() |> 
        ggplot(aes(x = pred, y = residu)) + 
        list(
          geom_hline(yintercept = 0, linetype = "dashed"),
          geom_point(size = 2),
          geom_smooth(
            method = "loess",
            formula = y ~ x,
            se = FALSE
          ),
          if(length(input$wilayah) > 1) facet_wrap(
            vars(wilayah),
            scales = "free_x",
            ncol = 3
          ),
          theme_minimal(),
          labs(
            x = "Produksi Gabah (Ton) (Pred. Model)",
            y = "Galat"
          )
        )
    } else if (input$data == "musik") {
      plot <- data_model() |> 
        ggplot(aes(x = pred, y = residu)) + 
        list(
          geom_hline(yintercept = 0, linetype = "dashed"),
          geom_point(size = 2),
          geom_smooth(
            method = "loess",
            formula = y ~ x,
            se = FALSE
          ),
          theme_minimal(),
          labs(
            x = "Rerata Rating (Pred. Model)",
            y = "Galat"
          )
        )
    } else if (input$data == "pangan") {
      plot <- data_model() |> 
        ggplot(aes(x = pred, y = residu)) + 
        list(
          geom_hline(yintercept = 0, linetype = "dashed"),
          geom_point(size = 2),
          geom_smooth(
            method = "loess",
            formula = y ~ x,
            se = FALSE
          ),
          scale_x_continuous(labels = scales::label_percent(scale = 1)),
          if(length(input$wilayah) > 1) facet_wrap(
            vars(wilayah),
            scales = "free_x",
            ncol = 3
          ),
          theme_minimal(),
          labs(
            x = "Prevalensi Ketidakcukupan Konsumsi Pangan (Pred. Model)",
            y = "Galat"
          )
        )
    } else if (input$data == "rumah") {
      plot <- data_model() |> 
        ggplot(aes(x = pred, y = residu)) + 
        list(
          geom_hline(yintercept = 0, linetype = "dashed"),
          geom_point(size = 2),
          geom_smooth(
            method = "loess",
            formula = y ~ x,
            se = FALSE
          ),
          if(length(input$wilayah) > 1) facet_wrap(
            vars(wilayah),
            scales = "free_x",
            ncol = 3
          ),
          theme_minimal(),
          labs(
            x = "Harga Unit Perumahan (Pred. Model)",
            y = "Galat"
          )
        )
    }
    
    ggplotly(plot)
  })
  
  ## Tabel data ----
  output$tabel_data <- renderTable(
    striped = TRUE,
    hover = TRUE,
    width = "100%",{
    data_model() |> 
        rename(
          galat = residu
        )
  })
  
}

# Aplikasi Shiny ----
shinyApp(ui, server)
