#' Mengekspor aplikasi Shiny dalam folder "apl"
#' ke folder "docs".
shinylive::export(
  appdir = "apl-derajat-radian",
  destdir = "docs",
  subdir = "derajat-radian"
)
shinylive::export(
  appdir = "apl-simulasi-tabungan",
  destdir = "docs",
  subdir = "simulasi-tabungan"
)
shinylive::export(
  appdir = "apl-plotly",
  destdir = "docs",
  subdir = "contoh-plotly"
)

# Menguji aplikasi dalam folder "docs".
httpuv::runStaticServer("docs")
