library(shiny)
library(bslib)
library(tidyr)
library(dplyr)
library(purrr)
library(forcats)
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
            "Dasbor Shiny ini bertujuan untuk menggunakan fungsi polinomial untuk mengeksplorasi dan memodelkan data di sektor pangan (prevalensi ketidakcukupan konsumsi pangan dan produksi gabah), pariwisata (wisatawan Candi Borobudur), ekonomi (harga per unit pembangunan rumah), dan musik.

Bagi pendidik, aplikasi ini dapat digunakan untuk memfasilitasi peserta didik bermatematika. Misalnya, permasalahan berikut dapat diajukan kepada peserta didik.

> Untuk data Preferensi Musik, misalnya, fungsi polinomial berderajat berapakah yang paling efektif dan efisien untuk memodelkan `Rerata Rating` terhadap `Usia Lagu`? Mengapa?

Dalam menjawab permasalahan tersebut, peserta didik dapat berdiskusi dengan teman-temannya dalam kelompok. Untuk melakukannya, mereka dapat mengamati diagram pencar (dan modelnya) atau tabel yang disediakan. Diagram pencar tersebut dapat dilihat pada tab Plot sedangkan tabelnya dapat dilihat pada tab Data dalam laman Eksplorasi."
          )
        ),
        nav_panel(
          #### Alat ----
          title = "Alat",
          markdown(
            "Aplikasi ini dikembangkan dengan menggunakan bahasa pemrograman [R](https://www.R-project.org/) dan paket [Shiny.](https://CRAN.R-project.org/package=shiny) Paket [shinylive](https://CRAN.R-project.org/package=shinylive) digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah. Selain itu, aplikasi ini juga menggunakan paket [bslib,](https://CRAN.R-project.org/package=bslib) [tidyr,](https://CRAN.R-project.org/package=tidyr) [dplyr,](https://CRAN.R-project.org/package=dplyr) [purrr,](https://CRAN.R-project.org/package=purrr) [broom,](https://CRAN.R-project.org/package=broom) [ggplot2,](https://ggplot2.tidyverse.org) [forcats,](https://CRAN.R-project.org/package=forcats) [plotly,](https://plotly-r.com) dan [scales.](https://CRAN.R-project.org/package=scales)"
          )
        ),
        nav_panel(
          #### Pengembang ----
          title = "Pengembang",
          markdown(
            "Pengembang dan pemelihara aplikasi ini adalah [Yosep Dwi Kristanto](https://people.usd.ac.id/~ydkristanto), seorang dosen dan peneliti di program studi [Pendidikan Matematika,](https://usd.ac.id/s1pmat) [Universitas Sanata Dharma,](https://www.usd.ac.id/) Yogyakarta."
          )
        ),
        nav_panel(
          #### Kode Sumber ----
          title = "Kode Sumber",
          markdown(
            "Kode sumber aplikasi ini tersedia di repositori [Github.](https://github.com/ydkristanto/matematika-tl-11) Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan [buat sebuah isu](https://github.com/ydkristanto/matematika-tl-11/issues) atau lebih baik lagi [minta penarikan](https://github.com/ydkristanto/matematika-tl-11/pulls) di repositori tersebut."
          )
        )
      ),
      #### Data ----
      navset_card_underline(
        title = "Data",
        nav_panel(
          title = "Pangan",
          markdown(
            "Data sektor pangan yang digunakan dalam dasbor ini adalah [data prevalensi ketidakcukupan konsumsi pangan](https://www.bps.go.id/id/statistics-table/2/MTQ3MyMy/prevalensi-ketidakcukupan-konsumsi-pangan--persen-.html) dan [data produksi gabah,](https://www.bps.go.id/id/statistics-table/2/MTQ5OCMy/luas-panen--produksi--dan-produktivitas-padi-menurut-provinsi.html) yang keduanya diperoleh dari BPS. Prevalensi ketidakcukupan konsumsi pangan (*Prevalence of Undernourishment*, PoU) merupakan kondisi saat suatu populasi tertentu mengkonsumsi jumlah makanan yang tidak cukup untuk memenuhi energi yang dibutuhkan untuk hidup normal, aktif, dan sehat.
            
            Pada periode 2017–2023, rata-rata PoU provinsi-provinsi di Indonesia disajikan seperti berikut."
          ),
          div(
            plotOutput("ringkasan_pou"),
            style = "height:390px;"
          ),
          markdown(
            "Rata-rata produksi gabah provinsi-provinsi di Indonesia pada periode 2018–2023 ditunjukkan seperti berikut."
          ),
          div(
            plotOutput("ringkasan_gabah"),
            style = "height:390px;"
          )
        ),
        nav_panel(
          title = "Pariwisata",
          markdown(
          "Data sektor pariwisata yang digunakan dalam dasbor ini merupakan data [jumlah pengunjung obyek wisata Candi Borobudur](https://magelangkab.bps.go.id/indicator/16/327/1/pengunjung-candi-borobudur.html) pada periode 2008–2022. Data tersebut diperoleh dari BPS Kabupaten Magelang.
          
          Berdasarkan asal wisatawannya, jumlah pengunjung obyek pariwisata tersebut setiap bulannya disajikan sebagai berikut."
          ),
          div(
            plotOutput("ringkasan_borobudur"),
            style = "height:200px;"
          )
        ),
        nav_panel(
          title = "Ekonomi",
          markdown(
            "Untuk sektor ekonomi, dasbor ini menggunakan [data rerata harga per unit pembangunan rumah oleh perum perumnas untuk provinsi-provinsi di Indonesia](https://www.bps.go.id/id/statistics-table/2/MjU2IzI=/rata-rata-harga-unit-pembangunan-rumah-oleh-perum-perumnas.html) yang periode maksimumnya adalah 2008–2018. Data ini diperoleh dari BPS.
            
            Rerata harga per unit untuk provinsi-provinsi tersebut disajikan sebagai berikut."
          ),
          div(
            plotOutput("ringkasan_rumah"),
            style = "height:390px;"
          )
        ),
        nav_panel(
          title = "Musik",
          markdown(
            "Data musik yang digunakan dalam dasbor ini bersumber dari Davies dkk. ([2022](https://doi.org/10.1007/s11002-022-09626-7)). Berdasarkan artikel yang mereka publikasikan di [*Marketing Letters*](https://link.springer.com/journal/11002), mereka mensurvei 1036 responden yang semuanya merupakan warga Amerika Serikat dengan rerata usia 48,6 tahun (SD = 17,6) dan 53% perempuan. Responden-responden tersebut diminta untuk memberikan rating terhadap [beberapa lagu hits yang diberikan.](https://link.springer.com/article/10.1007/s11002-022-09626-7/tables/2) Lagu-lagu tersebut antara lain [Stayin’ Alive (Bee Gees),](https://open.spotify.com/track/5ubvP9oKmxLUVq506fgLhk?si=a1d2166d657240ac) [Lean on Me (Bill Withers),](https://open.spotify.com/track/5zCJvrT3C7cIfHsR5iG95l?si=0541628dea324579) dan [I Get Around (Beach Boys).](https://open.spotify.com/track/3mXexrmtPJ1KdWN37rYePx?si=cb906fdeeb594278) Rating tersebut merentang dari 0 (sangat tidak suka) sampai 10 (sangat suka).
            
            Sebagai gambaran umum, berikut ini rerata rating 10 lagu yang memiliki rerata rating tertinggi berdasarkan survei tersebut."
          ),
          div(
            plotOutput("ringkasan_lagu"),
            style = "height:390px;"
          ),
          markdown(
            "Data tersebut digunakan untuk mengetahui bagaimana hubungan usia lagu terhadap rerata ratingnya. Usia lagu ini merupakan hasil pengurangan tahun rilis sebuah lagu dengan tahun lahir responden. Misalnya, jika seorang responden lahir pada tahun 1985 memberikan rating terhadap lagu yang rilis pada tahun 2002, usia lagunya adalah 17."
          )
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
            y = "Prevalensi Ketidakcukupan\nKonsumsi Pangan (PoU)"
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
            y = "Rerata Harga/Unit\n(Juta Rupiah)"
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
            x = "Rerata Harga/Unit (Juta Rupiah) (Pred. Model)",
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
    data <- data_model() |> 
        rename(
          Galat = residu
        )
    if(input$data == "borobudur") {
      tabel <- data |> 
        rename(
          Wisatawan = wisatawan,
          Bulan = bulan,
          `Rerata Banyak Wisatawan` = rerata,
          `Pred. Model` = pred
        )
    } else if(input$data == "gabah") {
      tabel <- data |> 
        rename(
          Wilayah = wilayah,
          Tahun = tahun,
          `Produksi Gabah (Ton)` = produksi_ton,
          `Pred. Model` = pred
        )
    } else if(input$data == "pangan") {
      tabel <- data |> 
        rename(
          Wilayah = wilayah,
          Tahun = tahun,
          `PoU (%)` = pou,
          `Pred. Model` = pred
        )
    } else if(input$data == "rumah") {
      tabel <- data |> 
        rename(
          Wilayah = wilayah,
          Tahun = tahun,
          `Rerata Harga/Unit (Juta Rupiah)` = harga,
          `Pred. Model` = pred
        )
    } else if(input$data == "musik") {
      tabel <- data |> 
        select(-frekuensi) |> 
        mutate(usia_lagu = as.integer(round(usia_lagu))) |> 
        rename(
          `Usia Lagu` = usia_lagu,
          `Rerata Rating` = rerata_rating,
          `Pred. Model` = pred
        )
    }
    
    tabel
    
  })
  
  ## Ringkasan PoU ----
  output$ringkasan_pou <- renderPlot({
    pou_idn <- data_pangan |> 
      filter(wilayah == "Indonesia")
    rerata_pou_idn <- mean(pou_idn$pou)
    plot <- data_pangan |> 
      filter(wilayah != "Indonesia") |> 
      group_by(wilayah) |> 
      summarise(
        rerata_pou = mean(pou), .groups = "drop"
      ) |> 
      mutate(wilayah = fct_reorder(wilayah, rerata_pou)) |> 
      ggplot(aes(x = rerata_pou, y = wilayah)) + 
      geom_vline(xintercept = rerata_pou_idn, linetype = "dashed") + 
      geom_point(aes(color = rerata_pou), show.legend = FALSE, size = 2) + 
      geom_label(
        x = rerata_pou_idn - 1.25, y = 4,
        label = "Indonesia",
        angle = 90
      ) + 
      scale_x_continuous(labels = scales::label_percent(scale = 1)) + 
      scale_color_viridis_b() + 
      theme_minimal() + 
      theme(axis.title.y = element_blank()) + 
      labs(x = "Rerata PoU")
    plot
    
  })
  
  ## Ringkasan produksi gabah ----
  output$ringkasan_gabah <- renderPlot({
    plot <- data_gabah |> 
      filter(wilayah != "Indonesia") |> 
      drop_na() |> 
      group_by(wilayah) |> 
      summarise(
        rerata_produksi = mean(produksi_ton, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      mutate(
        wilayah = fct_reorder(wilayah, rerata_produksi)
      ) |> 
      ggplot(aes(x = rerata_produksi, y = wilayah)) + 
      geom_point(
        aes(color = rerata_produksi),
        size = 2,
        show.legend = FALSE
      )+ 
      scale_color_viridis_b() + 
      theme_minimal() + 
      theme(axis.title.y = element_blank()) + 
      labs(x = "Rerata Produksi Gabah (Ton)")
    
    plot
    
  })
  
  ## Ringkasan wisatawan Borobudur ----
  output$ringkasan_borobudur <- renderPlot({
    plot <- data_borobudur |> 
      ggplot(aes(x = tanggal_bawah, y = frekuensi)) + 
      geom_line(
        aes(group = wisatawan, color = wisatawan),
        show.legend = FALSE
      ) + 
      geom_point(aes(color = wisatawan), show.legend = FALSE) + 
      facet_wrap(vars(wisatawan), scales = "free_y", ncol = 1) + 
      scale_color_viridis_d() + 
      theme_minimal() + 
      labs(
        x = "Waktu",
        y = "Banyak Wisatawan"
      )
    
    plot
  })
  
  ## Ringkasan harga rumah ----
  output$ringkasan_rumah <- renderPlot({
    harga_rmh_idn <- data_rumah |> 
      filter(wilayah == "Indonesia")
    rerata_harga_rmh_idn <- harga_rmh_idn$harga |> 
      mean()
    plot <- data_rumah |> 
      filter(wilayah != "Indonesia") |> 
      drop_na() |> 
      group_by(wilayah) |> 
      summarise(
        rerata_harga = mean(harga, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      mutate(
        wilayah = fct_reorder(wilayah, rerata_harga)
      ) |> 
      ggplot(aes(x = rerata_harga, y = wilayah)) + 
      geom_point(
        aes(color = rerata_harga),
        size = 2,
        show.legend = FALSE
      ) + 
      scale_color_viridis_b() + 
      theme_minimal() + 
      theme(axis.title.y = element_blank()) + 
      labs(x = "Rerata Harga per Unit (Juta Rupiah)")
    
    plot
    
  })
  
  ## Ringkasan lagu teratas ----
  output$ringkasan_lagu <- renderPlot({
    daftar_lagu <- tribble(
      ~tahun_rilis, ~judul, ~penampil,
      1950, "Play a Simple Melody", "Bing and Gary Crosby",
      1952, "You Belong to Me", "Jo Stafford",
      1954, "Sh Boom Sh Boom", "The Crew Cuts",
      1956, "My Prayer", "The Platters",
      1958, "Patricia", "Perez Prado",
      1960, "Running Bear", "Johnny Preston",
      1962, "Roses are Red", "Bobby Vinton",
      1964, "I Get Around", "Beach Boys",
      1966, "The Last Train to Clarksville", "The Monkees",
      1968, "People Got to be Free", "The Rascals",
      1970, "Raindrops Keep Fallin’ on My Head", "B.J. Thomas",
      1972, "Lean on Me", "Bill Withers",
      1974, "The Sound of Philadelphia", "MFSB ft. Three Degrees",
      1976, "Play that Funky Music", "Wild Cherry",
      1978, "Stayin’ Alive", "Bee Gees",
      1980, "Crazy Little Thing Called Love", "Queen",
      1982, "Don’t You Want Me", "Human League",
      1984, "Footloose", "Kenny Loggins",
      1986, "Party All the Time", "Eddie Murphy",
      1988, "Sweet Child O’ Mine", "Guns N’ Roses",
      1990, "Vogue", "Madonna",
      1992, "Under the Bridge", "Red Hot Chilli Peppers",
      1994, "All She Wants", "Ace of Base",
      1996, "Missing", "Everything but the Girl",
      1998, "Crush", "Jennifer Paige",
      2000, "Say My Name", "Destiny’s Child",
      2002, "Dilemma", "Nelly ft. Kelly Rowland",
      2004, "Hey Ya", "OutKast",
      2006, "Sexy Back", "Justin Timberlake",
      2008, "Lollipop", "Lil Wayne",
      2010, "California Gurls", "Katy Perry",
      2012, "Payphone", "Maroon 5",
      2014, "Counting Stars", "One Republic",
      2016, "Work", "Rihanna"
    ) |> 
      mutate(
        tahun_rilis = as.integer(tahun_rilis)
      )
    
    lagu_teratas <- tribble(
      ~tahun_rilis, ~rerata_rating,
      1978, 7.71,
      1972, 7.42,
      1964, 7.2,
      1984, 6.86,
      1988, 6.53,
      1966, 6.48,
      1970, 6.4,
      1976, 6.37,
      1992, 6.34,
      1980, 6.29
    ) |> 
      arrange(tahun_rilis)
    
    daftar_lagu_teratas <- lagu_teratas |> 
      left_join(daftar_lagu, by = join_by(tahun_rilis))
    
    daftar_lagu_teratas |> 
      mutate(
        judul = fct_reorder(judul, rerata_rating)
      ) |> 
      ggplot(aes(x = rerata_rating, y = judul)) + 
      geom_point(
        aes(color = rerata_rating),
        size = 3,
        show.legend = FALSE
      ) + 
      scale_y_discrete(labels = scales::label_wrap(20)) + 
      scale_color_viridis_b() + 
      theme_minimal(base_size = 12) + 
      theme(axis.title.y = element_blank()) + 
      labs(
        x = "Rerata Rating"
      )
    
  })
  
}

# Aplikasi Shiny ----
shinyApp(ui, server)
