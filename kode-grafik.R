# Paket ----
library(tidyverse)
library(patchwork)
library(readxl)
palet_warna <- c(
  "#E32D91", "#4EA6DC", "#4775E7",
  "#8971E1", "#C830CC"
)

# Tayangan konten digital ----
tayangan_konten_2018 <- read_csv("data/tayangan-konten-2018.csv")
tayangan_konten_2018 %>% 
  mutate(
    Tanggal = as.numeric((Tanggal - ymd("2018-01-01")))
  ) %>% 
  ggplot(
    aes(x = Tanggal, y = Penayangan)
  ) +
  geom_point(
    size = 3,
    color = "#4775E7",
    alpha = .8
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 4),
    se = TRUE,
    color = "#E32D91",
    linewidth = 1.5
  ) +
  theme_minimal()

# Rerata harga pembangunan rumah di perumahan ----
#' Data: https://www.bps.go.id/id/statistics-table/2/MjU2IzI=/rata-rata-harga-unit-pembangunan-rumah-oleh-perum-perumnas.html
#' Data diakses pada 2024-04-24
load("data/harga_rumah.RData")
daftar_prov <- c(
  "Kalimantan Tengah", "Sumatera Selatan",
  "Bengkulu", "Jawa Barat"
)
p <- harga_rumah %>% 
  filter(provinsi %in% daftar_prov) %>% 
  ggplot(aes(x = tahun, y = rerata_harga)) + 
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 3),
    alpha = .2,
    color = "#43ADA5"
  ) +
  geom_point(
    color = "#E32D91",
    size = 2,
    alpha = .8
  ) +
  scale_x_continuous(
    breaks = seq(2007, 2019, 2)
  ) +
  theme_minimal(base_size = 9) +
  facet_wrap(vars(provinsi), nrow = 2) +
  labs(
    x = "Tahun",
    y = "Harga (Juta Rupiah)",
    title = "Rata-Rata Harga Tiap Unit\nPembangunan Rumah oleh Perum Perumnas",
    caption = "Data: Badan Pusat Statistik, Perum Perumnas"
  )

ggsave(
  filename = "plot/harga_rumah.png",
  plot = p,
  width = 12,
  height = 6,
  units = "cm",
  dpi = 300,
  bg = "white"
)

# Harga gabah ----
#' Data diunduh dari https://www.bps.go.id/id/statistics-table/2/MTAzNCMy/rata-rata-harga-gabah-bulanan-menurut-kualitas--komponen-mutu-dan-hpp-di-tingkat-petani.html
#' Data diunduh pada 2024-04-24
load("data/harga_gabah.RData")
harga_gabah %>% 
  ggplot(aes(x = tanggal, y = harga_GKP)) + 
  geom_point(
    size = 3,
    color = "#E32D91",
    alpha = .6
  ) + 
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 4),
    color = "#4EA6DC",
    alpha = .2,
    linewidth = 1.5
  ) + 
  theme_minimal() + 
  labs(
    title = "Rata-Rata Harga Gabah Kering Panen Bulanan\ndi Tingkat Petani",
    x = "Waktu",
    y = "Harga (Rupiah/Kg)",
    caption = "Data: Badan Pusat Statistik (BPS) Indonesia"
  )

# Prevalensi Ketidakcukupan Konsumsi Pangan ----
load("data/PoU.RData")
PoU %>% 
  group_by(provinsi) %>% 
  summarise(rerata_pou = mean(pou), .groups = "drop") %>% 
  arrange(-rerata_pou) %>% 
  print(n = 35)
p1 <- PoU %>% 
  filter(provinsi == "Indonesia") %>% 
  ggplot(aes(x = tahun, y = pou)) + 
  geom_point(
    size = 1,
    color = "#E32D91"
  ) + 
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 4),
    color = "#43ADA5",
    alpha = .1,
    linewidth = 1
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal(base_size = 6) + 
  labs(
    title = "Prevalensi Ketidakcukupan\nKonsumsi Pangan",
    x = "Tahun",
    y = "Proporsi"
  )
p1_1 <- PoU %>% 
  filter(provinsi == "Indonesia") %>% 
  ggplot(aes(x = tahun, y = pou)) + 
  geom_point(
    size = 1,
    color = "#E32D91"
  ) + 
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 4),
    color = "#43ADA5",
    alpha = .1,
    linewidth = 1
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal(base_size = 9) + 
  labs(
    x = "Tahun",
    y = "Proporsi",
    caption = "Data: Badan Pusat Statistik, Susenas"
  )

PoU_ID <- filter(PoU, provinsi == "Indonesia")
model_ID <- lm(
  formula = pou ~ poly(tahun, 4),
  data = PoU_ID
)

ggsave(
  filename = "plot/prev_pangan.svg",
  plot = p1_1,
  width = 9.6,
  height = 4.8,
  units = "cm",
  dpi = 300,
  bg = "white"
)

# Produksi padi ----
load("data/produksi_padi.RData")
#' Data diperoleh dari https://www.bps.go.id/id/statistics-table/2/MTQ5OCMy/luas-panen--produksi--dan-produktivitas-padi-menurut-provinsi.html
#' Data diunduh pada 2024-04-24

p2 <- produksi_padi %>% 
  filter(provinsi == "Indonesia") %>% 
  mutate(tahun = as.numeric(tahun),
         produksi_jtton = as.numeric(produksi_ton) / 10^6) %>% 
  ggplot(aes(x = tahun, y = produksi_jtton)) + 
  geom_point(
    size = 1,
    color = "#E32D91"
  ) + 
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 3),
    color = "#43ADA5",
    alpha = .1,
    linewidth = 1
  ) + 
  scale_y_continuous(
    labels = scales::label_comma(
      big.mark = ".",
      decimal.mark = ","
    )
  ) + 
  scale_x_continuous(breaks = seq(2018, 2022, 2), limits = c(2017, 2023)) + 
  theme_minimal(base_size = 6) + 
  labs(
    title = "Produksi Gabah",
    x = "Tahun",
    y = "Produksi\n(Juta Ton)",
    caption = "Data: Badan Pusat Statistik, Susenas"
  )

p2_1 <- produksi_padi %>% 
  filter(provinsi == "Indonesia") %>% 
  mutate(tahun = as.numeric(tahun),
         produksi_jtton = as.numeric(produksi_ton) / 10^6) %>% 
  ggplot(aes(x = tahun, y = produksi_jtton)) + 
  geom_point(
    size = 1,
    color = "#E32D91"
  ) + 
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 3),
    color = "#43ADA5",
    alpha = .1,
    linewidth = 1
  ) + 
  scale_y_continuous(
    labels = scales::label_comma(
      big.mark = ".",
      decimal.mark = ","
    )
  ) + 
  scale_x_continuous(breaks = seq(2018, 2022, 2), limits = c(2017, 2023)) + 
  theme_minimal(base_size = 9) + 
  labs(
    x = "Tahun",
    y = "Produksi\n(Juta Ton)",
    caption = "Data: Badan Pusat Statistik, Susenas"
  )

ggsave(
  filename = "plot/produksi_gabah.svg",
  plot = p2_1,
  width = 9.6,
  height = 4.8,
  units = "cm",
  dpi = 300,
  bg = "white"
)

plot <- p1 / p2
ggsave(
  filename = "plot/statistik_pangan.svg",
  plot = plot,
  width = 8,
  height = 6,
  units = "cm",
  dpi = 300
)

# Pengunjung Borobudur ----
pengunjung_borobudur <- read_excel("data/pengunjung_borobudur.xlsx")
pengunjung_borobudur <- pengunjung_borobudur %>% 
  pivot_longer(
    cols = c(-turis, -bulan),
    names_to = "tahun",
    values_to = "banyak_pengunjung"
  ) %>% 
  mutate(
    tanggal_bawah = as.Date(paste(tahun, bulan, "01", sep = "-")),
    tahun = as.numeric(tahun)
  )

rrt_bulanan <- pengunjung_borobudur %>% 
  group_by(turis, bulan) %>% 
  summarise(rerata_pengunjung = mean(banyak_pengunjung), .groups = "drop")
data_bulanan <- rrt_bulanan %>% 
  filter(turis == "Mancanegara")
model <- lm(
  formula = rerata_pengunjung ~ poly(bulan, degree = 3, raw = TRUE),
  data = data_bulanan
)
coefficients(model)

p_borobudur <- data_bulanan %>% 
  ggplot(aes(x = bulan, y = rerata_pengunjung)) + 
  geom_point(
    size = 1.5,
    color = "#E32D91"
  ) + 
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, degree = 3),
    color = "#43ADA5",
    alpha = .15,
    linewidth = 1.25
  ) + 
  scale_x_continuous(breaks = seq(1:12)) + 
  theme_minimal(base_size = 9) + 
  labs(
    x = "Bulan",
    y = "Rerata Banyaknya\nWisatawan Mancanegara",
    caption = "Data: BPS Kabupaten Magelang"
  )

ggsave(
  filename = "plot/pengunjung_borobudur.svg",
  plot = p_borobudur,
  width = 9.6,
  height = 5.4,
  units = "cm",
  dpi = 300,
  bg = "white"
)

# Pontianak dan Quito ----
library(tidyverse)
library(ggthemes)

peta_dunia <- map_data("world") %>% 
  mutate(long = ifelse(long < 0, long + 360, long)) %>% 
  filter(long >= 95 & long <= 327 & abs(lat) <= 50)

negara <- peta_dunia %>% 
  distinct(region) %>% 
  mutate(
    id = ifelse(
      region == "Ecuador" | region == "Indonesia", "1", "0"
    )
  )

negara %>% ggplot() +
  geom_map(map = peta_dunia, aes(fill = id, map_id = region)) +
  expand_limits(
    x = peta_dunia$long,
    y = peta_dunia$lat
  ) +
  annotate("segment", x = 109, xend = 282, y = 0, yend = 0,
           color = "#4EA6DC") +
  geom_point(
    data = data.frame(xx = c(109, 282), yy = c(0, 0)),
    aes(x = xx, y = yy),
    color = "#4EA6DC"
  ) +
  coord_fixed() +
  scale_fill_manual(
    values = c("grey80", "#E32D91")
  ) +
  theme_map() +
  theme(legend.position = "none")

# Bumi dari atas
library(ggOceanMaps)
library(ggthemes)

# Posisi Taman Nasional Tesso Nilo dan Tugu Khatulistiwa Halmahera Selatan
basemap(limits = 30)
basemap(limits = c(95, 141, -11, 6)) +
  annotate(
    "segment",
    x = 102, xend = 128, y = 0, yend = 0,
    color = "#4EA6DC", linewidth = 1
  ) +
  geom_point(
    data = data.frame(x = c(102, 128), y = c(0, 0)),
    aes(x = x, y = y), color = "#E32D91", size = 2
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )

# Denpasar dan Perth ----
load("data/sun_data.RData")
data <- sun_data %>% 
  select(city, date, day_length) %>% 
  filter(
    city %in% c("denpasar", "perth"),
    date >= as.POSIXct("2023-01-01", format = "%Y-%m-%d"),
    date <= as.POSIXct("2024-12-31", format = "%Y-%m-%d")
  ) %>% 
  mutate(
    month = floor_date(date, unit = "month"),
    city = str_to_title(city)
  ) %>% 
  group_by(city, month) %>% 
  summarise(
    day_length_avg = mean(day_length, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(city) %>% 
  mutate(
    month_n = seq_along(month)
  ) %>% 
  ungroup() %>% 
  rename(
    Kota = city,
    Bulan = month_n,
    `Lama hari (jam)` = day_length_avg
  )

data %>% 
  ggplot(aes(x = Bulan, y = `Lama hari (jam)`)) +
  geom_point(
    aes(color = Kota, shape = Kota),
    size = 1.5
  ) +
  ylim(0, 18) +
  scale_color_manual(
    values = c(
      "Denpasar" = "#E32D91",
      "Perth" = "#43ADA5"
    )
  ) +
  theme_minimal(base_size = 7) +
  theme(
    legend.position = "bottom",
    # plot.title = element_text(face = "bold", hjust = .5),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    title = "Perbandingan Lamanya Hari di Kota Denpasar dan Perth"
  )
# Denpasar
data %>% 
  filter(Kota == "Denpasar") %>% 
  ggplot(aes(x = month, y = `Lama hari (jam)`)) +
  geom_point(
    color = "#E32D91"
  ) + 
  scale_x_date(
    labels = scales::date_format("%b %Y", locale = "id"),
    breaks = "2 month"
  ) + 
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal(base_size = 5) + 
  theme(
    legend.position = "bottom",
    # plot.title = element_text(face = "bold", hjust = .5),
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )
# Perth
data %>% 
  filter(Kota == "Perth") %>% 
  ggplot(aes(x = month, y = `Lama hari (jam)`)) +
  geom_point(
    color = "#4EA6DC"
  ) + 
  scale_x_date(
    labels = scales::date_format("%b %Y", locale = "id"),
    breaks = "2 month"
  ) + 
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal(base_size = 5) + 
  theme(
    legend.position = "bottom",
    # plot.title = element_text(face = "bold", hjust = .5),
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )

# Denpasar dan Perth
ggplot(
  data = data.frame(x = 0:25),
  aes(x = x)
) + 
  geom_function(
    aes(color = "Perth"),
    fun = function(x) 0.49 * cos(0.52 * x) + 12.12,
    xlim = c(1, 24)
  ) + 
  geom_function(
    aes(color = "Denpasar"),
    fun = function(x) 2.02 * cos(0.52 * x) + 12.14,
    xlim = c(1, 24)
  ) + 
  scale_color_manual(
    name = "Kota",
    values = c(
      "Denpasar" = "#E32D91",
      "Perth" = "#4EA6DC"
    )
  ) + 
  theme_minimal(base_size = 5) + 
  theme(
    legend.position = "top"
  ) + 
  labs(
    x = "Bulan (mulai Januari 2023)",
    y = "Lama hari (jam)"
  )

ggsave(
  filename = "plot/denpasar-perth.png",
  plot = last_plot(),
  width = 9.6,
  height = 5.4,
  units = "cm",
  dpi = 300
)

# Model hubungan antara spesies dan luas (SAR) ----
# Data: Schrader dkk. (2020), https://doi.org/10.3897/BDJ.8.e55275
data_pulau <- read_excel("data/data_pulau.xlsx")

data_pulau %>% 
  filter(species_number > 0) %>% 
  ggplot(aes(x = island_area, y = species_number)) + 
  geom_smooth(alpha = .1) +
  geom_point(size = 3) + 
  scale_x_continuous(trans = "log10") + 
  scale_color_viridis_b() + 
  theme_minimal()



