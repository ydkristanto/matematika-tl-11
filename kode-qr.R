library(qrcode)
library(rsvg)

# Umum ----
## Laman Desmos untuk Siswa ----
student_desmos <- add_logo(
  qr_code(
    "https://student.desmos.com/?lang=id",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(student_desmos)

# Polinomial ----
## Perilaku Ujung Polinomial ----
ai_2_ujung_polinom <- add_logo(
  qr_code(
    "https://www.desmos.com/calculator/74aktkqfno",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_2_ujung_polinom)

# Fungsi Trigonometri ----
## Sifat Genap-Ganjil ----
ai_3_derajat_rad <- add_logo(
  qr_code(
    "https://people.usd.ac.id/~ydkristanto/app/derajat-radian/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_3_derajat_rad)

## Sifat Genap-Ganjil ----
ai_3_genap_ganjil <- add_logo(
  qr_code(
    "https://www.desmos.com/calculator/vyxivvfaed",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_3_genap_ganjil)

## Kincir Ria ----
ai_3_kincir_ria <- add_logo(
  qr_code(
    "https://teacher.desmos.com/activitybuilder/custom/660fd039ffb3066c53670d52?collections=660a994597f79f6450932fd2",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_3_kincir_ria)

## Transformasi Sinus dan Kosinus ----
ai_3_trans_sin_cos <- add_logo(
  qr_code(
    "https://teacher.desmos.com/activitybuilder/custom/66276cd7784d657b8857c626?collections=660a994597f79f6450932fd2&lang=id",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_3_trans_sin_cos)

## Kalkulator Matahari ----
ai_3_kalk_matahari <- add_logo(
  qr_code(
    "https://people.usd.ac.id/~ydkristanto/app/kalkulator-matahari/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_3_kalk_matahari)

## Daftar Putar Fungsi Trigonometri ----
sb_3_dft_putar_trig <- add_logo(
  qr_code(
    "https://youtube.com/playlist?list=PLlXuAh5HFy3dLrL5wdUWaVIAHbRGgWsdo&si=thLJ0tayW2R40OJ5",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(sb_3_dft_putar_trig)

## Ilustrasi Grafik Fungsi Trigonometri ----
sb_3_anim_sin_cos <- add_logo(
  qr_code(
    "https://yos3prens.wordpress.com/2013/02/27/ilustrasi-grafik-fungsi-trigonometri/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(sb_3_anim_sin_cos)

## Melukis Grafik Fungsi Tangen ----
sb_3_anim_tan <- add_logo(
  qr_code(
    "https://yos3prens.wordpress.com/2013/02/28/melukis-grafik-fungsi-tangen/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(sb_3_anim_tan)
