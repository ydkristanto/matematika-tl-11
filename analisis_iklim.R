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
  scale_fill_viridis_c(name = "Suhu") + 
  facet_wrap(vars(wilayah)) + 
  theme_minimal(base_size = 16) + 
  theme(
    axis.title = element_blank()
  ) + 
  labs(
    title = "Suhu Rata-Rata Jayapura dan Surabaya",
    caption = "Data: NASA POWER (power.larc.nasa.gov)"
  )

data_iklim_bulanan %>% 
  filter(
    tahun <= 2023,
    wilayah %in% c("Ruteng", "Surabaya")
  ) %>% 
  group_by(wilayah, tahun) %>% 
  summarise(
    suhu = mean(suhu, na.rm = TRUE),
    curah_hujan = mean(curah_hujan, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = tahun, y = suhu)) + 
  geom_line(
    aes(group = wilayah, color = wilayah),
    linewidth = 1
  ) + 
  scale_color_viridis_d() + 
  theme_minimal()
