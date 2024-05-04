library(tidyverse)
library(readr)
library(plotly)

# Data ----
data_jayapura <- read_csv(
  "data/jayapura_1981-2024.csv", 
  skip = 10
) %>% 
  mutate(
    T2M = na_if(T2M, -999),
    PRECTOTCORR = na_if(PRECTOTCORR, -999),
    wilayah = "Jayapura"
  ) %>% 
  rename(
    tahun = YEAR,
    hari = DOY,
    suhu = T2M,
    curah_hujan = PRECTOTCORR
  )
data_mamasa <- read_csv(
  "data/mamasa_1981-2024.csv", 
  skip = 10
) %>% 
  mutate(
    T2M = na_if(T2M, -999),
    PRECTOTCORR = na_if(PRECTOTCORR, -999),
    wilayah = "Mamasa"
  ) %>% 
  rename(
    tahun = YEAR,
    hari = DOY,
    suhu = T2M,
    curah_hujan = PRECTOTCORR
  )
data_ruteng <- read_csv(
  "data/ruteng_1981-2024.csv", 
  skip = 10
) %>% 
  mutate(
    T2M = na_if(T2M, -999),
    PRECTOTCORR = na_if(PRECTOTCORR, -999),
    wilayah = "Ruteng"
  ) %>% 
  rename(
    tahun = YEAR,
    hari = DOY,
    suhu = T2M,
    curah_hujan = PRECTOTCORR
  )
data_sby <- read_csv(
  "data/sby_1981-2024.csv", 
  skip = 10
) %>% 
  mutate(
    T2M = na_if(T2M, -999),
    PRECTOTCORR = na_if(PRECTOTCORR, -999),
    wilayah = "Surabaya"
  ) %>% 
  rename(
    tahun = YEAR,
    hari = DOY,
    suhu = T2M,
    curah_hujan = PRECTOTCORR
  )
data_iklim <- bind_rows(
  data_jayapura,
  data_mamasa,
  data_ruteng,
  data_sby
) %>% 
  mutate(
    tanggal = as.Date(paste(tahun, hari, sep = "-"), format = "%Y-%j")
  ) %>% 
  select(-hari)

data_iklim_bulanan <- data_iklim %>% 
  mutate(bulan = floor_date(tanggal, unit = "month")) %>% 
  group_by(wilayah, tahun, bulan) %>% 
  summarise(
    suhu = mean(suhu, na.rm = TRUE),
    curah_hujan = mean(curah_hujan, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(nama_bulan = month(bulan, label = TRUE, abbr = FALSE, locale = "id"))

data_iklim_sederhana <- data_iklim %>% 
  drop_na() %>% 
  mutate(
    triwulan = ceiling(month(tanggal) / 3),
    triwulan_lbl = if_else(
      triwulan == 1, "Jan-Mar",
      if_else(
        triwulan == 2, "Apr-Jun",
        if_else(
          triwulan == 3, "Jul-Sep", "Okt-Des"
        )
      )
    ),
    dasawarsa = ceiling((tahun - 1980) / 10),
    dasawarsa_lbl = if_else(
      dasawarsa == 1, "1981-1990",
      if_else(
        dasawarsa == 2, "1991-2000",
        if_else(
          dasawarsa == 3, "2001-2010",
          if_else(
            dasawarsa == 4, "2011-2020", "2021-2024"
          )
        )
      )
    )
  ) %>% 
  group_by(wilayah, dasawarsa, dasawarsa_lbl, triwulan, triwulan_lbl) %>% 
  summarise(
    suhu = mean(suhu, na.rm = TRUE),
    curah_hujan = mean(curah_hujan, na.rm = TRUE),
    .groups = "drop"
  )

suhu_sby_sederhana <- data_iklim_sederhana %>% 
  filter(
    wilayah == "Surabaya",
    dasawarsa < 5
  ) %>% 
  select(dasawarsa, triwulan, suhu) %>% 
  group_by(dasawarsa) %>% 
  arrange(-triwulan)
suhu_jypr_sederhana <- data_iklim_sederhana %>% 
  filter(
    wilayah == "Jayapura",
    dasawarsa < 5
  ) %>% 
  select(dasawarsa, triwulan, suhu) %>% 
  group_by(dasawarsa) %>% 
  arrange(-triwulan)
matriks_suhu_sby_sederhana <- matrix(
  data = round(suhu_sby_sederhana$suhu, 2),
  nrow = 4, byrow = TRUE,
  dimnames = list(
    c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Okt-Des"),
    c("1981-1990", "1991-2000", "2001-2010", "2011-2020")
  )
)
matriks_suhu_jypr_sederhana <- matrix(
  data = round(suhu_jypr_sederhana$suhu, 2),
  nrow = 4, byrow = TRUE,
  dimnames = list(
    c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Okt-Des"),
    c("1981-1990", "1991-2000", "2001-2010", "2011-2020")
  )
)

matriks_slsh_suhu_sby_jypr <- matriks_suhu_sby_sederhana - matriks_suhu_jypr_sederhana

# Visualisasi data ----
heatmap_bulanan <- data_iklim_bulanan %>% 
  filter(
    tahun <= 2023,
    wilayah %in% c("Ruteng", "Surabaya")
  ) %>% 
  ggplot(aes(x = tahun, y = nama_bulan)) + 
  geom_tile(aes(fill = suhu)) + 
  facet_wrap(vars(wilayah), nrow = 1) + 
  scale_fill_viridis_c(
    name = "Suhu"
  ) + 
  theme_minimal(base_size = 16) + 
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Suhu Rata-Rata Bulanan Ruteng dan Surabaya (1981-2023)",
    caption = "Data: NASA POWER (power.larc.nasa.gov)"
  )

heatmap_sederhana <- data_iklim_sederhana %>% 
  filter(
    wilayah %in% c("Jayapura", "Surabaya"),
    dasawarsa < 5
  ) %>% 
  mutate(
    triwulan_lbl = fct_reorder(triwulan_lbl, triwulan),
    dasawarsa_lbl = fct_reorder(dasawarsa_lbl, dasawarsa)
  ) %>% 
  ggplot(aes(x = dasawarsa_lbl, y = triwulan_lbl)) + 
  geom_tile(aes(fill = suhu)) + 
  scale_fill_viridis_c(name = "Rata-Rata\nSuhu (°C)") + 
  facet_wrap(vars(wilayah)) + 
  theme_minimal(base_size = 8) + 
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(vjust = 1, hjust = 1)
  ) + 
  labs(
    caption = "Data: NASA POWER (power.larc.nasa.gov)"
  )
ggsave(
  filename = "plot/suhu_jypr_sby_matriks.png",
  plot = heatmap_sederhana,
  width = 12,
  height = 6,
  units = "cm",
  dpi = 300,
  bg = "white"
)

suhu_selisih_matriks <- data_iklim_sederhana %>% 
  filter(
    wilayah %in% c("Jayapura", "Surabaya"),
    dasawarsa < 5
  ) %>% 
  select(-curah_hujan) %>% 
  pivot_wider(names_from = wilayah, values_from = suhu) %>% 
  mutate(selisih_suhu = Jayapura - Surabaya) %>% 
  mutate(
    triwulan_lbl = fct_reorder(triwulan_lbl, triwulan),
    dasawarsa_lbl = fct_reorder(dasawarsa_lbl, dasawarsa)
  ) %>% 
  ggplot(aes(x = dasawarsa_lbl, y = triwulan_lbl)) + 
  geom_tile(aes(fill = selisih_suhu)) + 
  scale_fill_viridis_c(name = "Selisih Rata-Rata\nSuhu (°C)", option = "magma") + 
  theme_minimal(base_size = 8) + 
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(vjust = 1, hjust = 1)
  ) + 
  labs(
    caption = "Data: NASA POWER (power.larc.nasa.gov)"
  )
ggsave(
  filename = "plot/suhu_selisih_matriks.png",
  plot = suhu_selisih_matriks,
  width = 9.6,
  height = 5.4,
  units = "cm",
  dpi = 300,
  bg = "white"
)

palet_slsh <- colorRampPalette(
  c("#ca0020", "#f7f7f7", "#0571b0")
)
heatmap(
  matriks_slsh_suhu_sby_jypr,
  col = palet_slsh(100),
  Colv = NA,
  Rowv = NA
)

diagram_garis <- data_iklim_bulanan %>% 
  filter(
    tahun <= 2023,
    wilayah %in% c("Jayapura", "Surabaya")
  ) %>% 
  ggplot(aes(x = bulan, y = suhu)) + 
  geom_line(
    aes(group = wilayah, color = wilayah),
    linewidth = .5
  ) + 
  scale_color_viridis_d(name = "") + 
  theme_minimal(base_size = 8) + 
  theme(legend.position = "bottom") + 
  labs(
    x = "Waktu",
    y = "Rata-Rata Suhu (°C)",
    caption = "Data: NASA POWER (power.larc.nasa.gov)"
  )
ggsave(
  filename = "plot/suhu_jypr_sby_garis.png",
  plot = diagram_garis,
  width = 12,
  height = 6,
  units = "cm",
  dpi = 300,
  bg = "white"
)
