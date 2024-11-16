#' Mengekspor aplikasi Shiny dalam folder "apl"
#' ke folder "docs".
shinylive::export(
  appdir = "apl-derajat-radian",
  destdir = "docs",
  subdir = "derajat-radian"
)
shinylive::export(
  appdir = "apl-jelajah-raja-ampat",
  destdir = "docs",
  subdir = "jelajah-raja-ampat"
)
shinylive::export(
  appdir = "apl-kalkulator-matahari",
  destdir = "docs",
  subdir = "kalkulator-matahari"
)
shinylive::export(
  appdir = "apl-model-polinomial",
  destdir = "docs",
  subdir = "model-polinomial"
)
shinylive::export(
  appdir = "apl-simulasi-tabungan",
  destdir = "docs",
  subdir = "simulasi-tabungan"
)

# Menguji aplikasi dalam folder "docs".
httpuv::runStaticServer("docs")
