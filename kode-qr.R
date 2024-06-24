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
## Pemodelan Fungsi Polinomial ----
ai_2_model_polinom <- add_logo(
  qr_code(
    "https://people.usd.ac.id/~ydkristanto/app/model-polinomial/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_2_model_polinom)

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

## Pembuat Nol, Persamaan, dan Grafik Fungsi Polinomial ----
ai_2_trans_sin_cos <- add_logo(
  qr_code(
    "https://teacher.desmos.com/activitybuilder/custom/663c8646e1925145ca5c679b?collections=660a994597f79f6450932fd2&lang=id",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_2_trans_sin_cos)

## Suku Banyak dan Cara Horner ----
sb_2_horner <- add_logo(
  qr_code(
    "https://yos3prens.wordpress.com/2016/01/19/suku-banyak-dan-cara-horner/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(sb_2_horner)

## Metode-metode Faktorisasi Suku Banyak ----
sb_2_faktor_polinom <- add_logo(
  qr_code(
    "https://yos3prens.wordpress.com/2012/09/27/metode-metode-faktorisasi-suku-banyak/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(sb_2_faktor_polinom)

# Fungsi Trigonometri ----
## ATP Fungsi Trigonometri ----
atp_3_fungsi_trig <- add_logo(
  qr_code(
    "https://kristantomath.com/2024/07/01/atp-fungsi-trigonometri/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(atp_3_fungsi_trig)
## Derajat-Radian ----
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

## Kincir Ria ----
ai_3_sin_lingkaran <- add_logo(
  qr_code(
    "https://www.desmos.com/calculator/aphag292zz",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_3_sin_lingkaran)

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

## Kasus Ambigu ----
ap_3_segitiga_dss <- add_logo(
  qr_code(
    "https://www.desmos.com/geometry/u8hxb907e6",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ap_3_segitiga_dss)

# Fungsi dan Pemodelannya ----
## Jelajah Kepulauan Raja Ampat ----
ai_6_jelajah_raja_ampat <- add_logo(
  qr_code(
    "https://people.usd.ac.id/~ydkristanto/app/jelajah-raja-ampat/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_6_jelajah_raja_ampat)

## Pembahasan Jelajah Kepulauan Raja Ampat ----
ai_6_matematika_raja_ampat <- add_logo(
  qr_code(
    "https://kristantomath.com/2024/05/02/matematika-raja-ampat/",
    ecl = "H"
  ),
  logo = "plot/logo-kemdikbud.svg",
  ecl = "L"
)
plot(ai_6_matematika_raja_ampat)
