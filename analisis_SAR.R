library(tidyverse)
library(readxl)


# Data ----
data_pulau <- read_excel(
  "data/data_pulau.xlsx", 
  na = "NA"
)
data_spesies <- read_excel(
  "data/data_spesies.xlsx", 
  na = "NA"
)
data_komunitas <- read_excel(
  "data/data_komunitas.xlsx", 
  na = "NA"
)
data_tanaman <- read_excel(
  "data/data_tanaman.xlsx", 
  na = "NA"
)
save(
  data_pulau, data_komunitas, data_spesies, data_tanaman,
  file = "data/data_raja_ampat.RData"
)

data_transek <- data_komunitas %>% 
  group_by(island_ID, transect_ID) %>% 
  summarise(banyak_spesies = n(), .groups = "drop")
data_transek_luas <- left_join(
  data_transek,
  select(data_pulau, island_ID, island_area),
  by = join_by(island_ID)
)

data_pulau_nol <- data_pulau %>% 
  select(island_ID, island_area, species_number) %>% 
  filter(species_number == 0) %>% 
  mutate(
    transect_ID = NA
  ) %>% 
  rename(banyak_spesies = species_number) %>% 
  select(
    island_ID, transect_ID, banyak_spesies, island_area
  )

data_transek_luas <- bind_rows(
  data_transek_luas, data_pulau_nol
)

data_transek_luas %>% 
  group_by(island_ID, island_area) %>% 
  summarise(
    banyak_spesies = mean(banyak_spesies, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = island_area, y = banyak_spesies)) + 
  geom_point() + 
  geom_smooth() + 
  scale_x_continuous(transform = "log10")


# Analisis model ----

## Mempersiapkan data ----
load("data/data_raja_ampat.RData")

# Tingkat pulau
dat_pulau <- data_pulau %>% 
  select(island_ID, island_area, species_number) %>% 
  rename(
    ID_pulau = island_ID,
    luas_riil = island_area,
    banyak_spesies = species_number
  ) %>% 
  mutate(
    luas_trans = log10(luas_riil)
  )

# Tingkat transek dan subtransek
n_spesies_transek <- data_komunitas %>% 
  arrange(transect_ID, plot_ID) %>% 
  group_by(island_ID, transect_ID) %>% 
  distinct(species_ID) %>% 
  summarise(banyak_spesies = n(), .groups = "drop") %>% 
  rename(
    ID_pulau = island_ID,
    ID_transek = transect_ID
  )

n_spesies_subtransek <- data_komunitas %>% 
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

dat_transek <- left_join(
  n_spesies_transek,
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

dat_subtransek <- left_join(
  n_spesies_subtransek,
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

# Data untuk pemodelan
data_model <- function(analisis = "pulau", transform = TRUE) {
  if (analisis == "pulau") {
    dat <- dat_pulau
  } else if (analisis == "transek") {
    dat <- dat_transek
  } else if (analisis == "subtransek") {
    dat <- dat_subtransek
  }
  
  if (transform) {
    data <- dat %>% 
      mutate(luas = luas_trans) %>% 
      select(ID_pulau, luas, banyak_spesies)
  } else {
    data <- dat %>% 
      mutate(luas = luas_riil) %>% 
      select(ID_pulau, luas, banyak_spesies)
  }
  
  return(data)
}

## Pilih data ----
data_model(analisis = "transek", transform = TRUE)

## Pemodelan ----

#' Kode: 1 = linear, 2 = pangkat
# Linear
m_1_pulau_ntrans <- lm(
  formula = banyak_spesies ~ luas,
  data = data_model(analisis = "pulau", transform = FALSE)
)
m_1_pulau_trans <- lm(
  formula = banyak_spesies ~ luas,
  data = data_model(analisis = "pulau", transform = TRUE)
)
m_1_transek_ntrans <- lm(
  formula = banyak_spesies ~ luas,
  data = data_model(analisis = "transek", transform = FALSE)
)
m_1_transek_trans <- lm(
  formula = banyak_spesies ~ luas,
  data = data_model(analisis = "transek", transform = TRUE)
)
m_1_subtransek_ntrans <- lm(
  formula = banyak_spesies ~ luas,
  data = data_model(analisis = "subtransek", transform = FALSE)
)
m_1_subtransek_trans <- lm(
  formula = banyak_spesies ~ luas,
  data = data_model(analisis = "subtransek", transform = TRUE)
)
linear <- list(
  m_1_pulau_ntrans, m_1_pulau_trans,
  m_1_transek_ntrans, m_1_transek_trans,
  m_1_subtransek_ntrans, m_1_subtransek_trans
)

# Pangkat
m_2_pulau_ntrans <- nls(
  banyak_spesies ~ c*(luas^z),
  data = data_model(analisis = "pulau", transform = FALSE),
  start = list(c = .9, z = .3)
)
m_2_pulau_trans <- nls(
  banyak_spesies ~ c*(luas^z),
  data = data_model(analisis = "pulau", transform = TRUE),
  start = list(c = 1.1, z = 2.1)
)
m_2_transek_ntrans <- nls(
  banyak_spesies ~ c*(luas^z),
  data = data_model(analisis = "transek", transform = FALSE),
  start = list(c = .9, z = .2)
)
m_2_transek_trans <- nls(
  banyak_spesies ~ c*(luas^z),
  data = data_model(analisis = "transek", transform = TRUE),
  start = list(c = 1.4, z = 1.4)
)
m_2_subtransek_ntrans <- nls(
  banyak_spesies ~ c*(luas^z),
  data = data_model(analisis = "subtransek", transform = FALSE),
  start = list(c = .5, z = .2)
)
m_2_subtransek_trans <- nls(
  banyak_spesies ~ c*(luas^z),
  data = data_model(analisis = "subtransek", transform = TRUE),
  start = list(c = .8, z = 1)
)

# Logaritma
m_3_pulau_ntrans <- nls(
  banyak_spesies ~ c + z * log10(luas),
  data = data_model(analisis = "pulau", transform = FALSE),
  start = list(c = -5.3, z = 6.2)
)
m_3_pulau_trans <- nls(
  banyak_spesies ~ c + z * log10(luas),
  data = data_model(analisis = "pulau", transform = TRUE),
  start = list(c = 2, z = 23)
)
m_3_transek_ntrans <- nls(
  banyak_spesies ~ c + z * log10(luas),
  data = data_model(analisis = "transek", transform = FALSE),
  start = list(c = -1.7, z = 2.8)
)
m_3_transek_trans <- nls(
  banyak_spesies ~ c + z * log10(luas),
  data = data_model(analisis = "transek", transform = TRUE),
  start = list(c = 1.5, z = 11.3)
)
m_3_subtransek_ntrans <- nls(
  banyak_spesies ~ c + z * log10(luas),
  data = data_model(analisis = "subtransek", transform = FALSE),
  start = list(c = -.1, z = .8)
)
m_3_subtransek_trans <- nls(
  banyak_spesies ~ c + z * log10(luas),
  data = data_model(analisis = "subtransek", transform = TRUE),
  start = list(c = .8, z = 3.6)
)

# Eksponensial
m_4_pulau_ntrans <- nls(
  banyak_spesies ~ c * (1 - exp(-z * luas)),
  data = data_model(analisis = "pulau", transform = FALSE),
  start = list(c = 19.4805, z = 0.00105999)
)
m_4_pulau_trans <- nls(
  banyak_spesies ~ c * (1 - exp(-z * luas)),
  data = data_model(analisis = "pulau", transform = TRUE),
  start = list(c = -2.01532, z = -.63726)
)
m_4_transek_ntrans <- nls(
  banyak_spesies ~ c * (1 - exp(-z * luas)),
  data = data_model(analisis = "transek", transform = FALSE),
  start = list(c = 7.58628, z = 0.0124218)
)
m_4_transek_trans <- nls(
  banyak_spesies ~ c * (1 - exp(-z * luas)),
  data = data_model(analisis = "transek", transform = TRUE),
  start = list(c = -5.83774, z = -.248316)
)
m_4_subtransek_ntrans <- nls(
  banyak_spesies ~ c * (1 - exp(-z * luas)),
  data = data_model(analisis = "subtransek", transform = FALSE),
  start = list(c = 2.67309, z = 0.0266384)
)
m_4_subtransek_trans <- nls(
  banyak_spesies ~ c * (1 - exp(-z * luas)),
  data = data_model(analisis = "subtransek", transform = TRUE),
  start = list(c = 16.3731, z = 0.0544549)
)

# Rasional
m_5_pulau_ntrans <- nls(
  banyak_spesies ~ (c + z * luas) / (1 + d * luas),
  data = data_model(analisis = "pulau", transform = FALSE),
  start = list(c = .664516, z = .0294826, d = .00133312)
)
m_5_pulau_trans <- nls(
  banyak_spesies ~ (c + z * luas) / (1 + d * luas),
  data = data_model(analisis = "pulau", transform = TRUE),
  start = list(c = -2.56523, z = 3.36139, d = -.130095)
)
m_5_transek_ntrans <- nls(
  banyak_spesies ~ (c + z * luas) / (1 + d * luas),
  data = data_model(analisis = "transek", transform = FALSE),
  start = list(c = -.74314, z = .165243, d = .0207741)
)
m_5_transek_trans <- nls(
  banyak_spesies ~ (c + z * luas) / (1 + d * luas),
  data = data_model(analisis = "transek", transform = TRUE),
  start = list(c = -2.99824, z = 4.28796, d = .137511)
)
m_5_subtransek_ntrans <- nls(
  banyak_spesies ~ (c + z * luas) / (1 + d * luas),
  data = data_model(analisis = "subtransek", transform = FALSE),
  start = list(c = -.375504, z = .134462, d = .0488608)
)
m_5_subtransek_trans <- nls(
  banyak_spesies ~ (c + z * luas) / (1 + d * luas),
  data = data_model(analisis = "subtransek", transform = TRUE),
  start = list(c = -1.64504, z = 2.883, d = .570726)
)

# Logistik
m_6_pulau_ntrans <- nls(
  banyak_spesies ~ d / (1 + exp(-z * luas + f)),
  data = data_model(analisis = "pulau", transform = FALSE),
  start = list(d = 18.3346, z = 0.00280074, f = 2.11491)
)
m_6_pulau_trans <- nls(
  banyak_spesies ~ d / (1 + exp(-z * luas + f)),
  data = data_model(analisis = "pulau", transform = TRUE),
  start = list(d = 26.4334, z = 1.56928, f = 4.79603)
)
m_6_transek_ntrans <- nls(
  banyak_spesies ~ d / (1 + exp(-z * luas + f)),
  data = data_model(analisis = "transek", transform = FALSE),
  start = list(d = 7.36549, z = 0.0791336, f = 3.18376)
)
m_6_transek_trans <- nls(
  banyak_spesies ~ d / (1 + exp(-z * luas + f)),
  data = data_model(analisis = "transek", transform = TRUE),
  start = list(d = 7.60477, z = 3.77735, f = 6.26236)
)
m_6_subtransek_ntrans <- nls(
  banyak_spesies ~ d / (1 + exp(-z * luas + f)),
  data = data_model(analisis = "subtransek", transform = FALSE),
  start = list(d = 2.66111, z = 0.0870047, f = 2.25108)
)
m_6_subtransek_trans <- nls(
  banyak_spesies ~ d / (1 + exp(-z * luas + f)),
  data = data_model(analisis = "subtransek", transform = TRUE),
  start = list(d = 2.69196, z = 3.5155, f = 4.84676)
)

# Weibull
m_7_pulau_ntrans <- nls(
  banyak_spesies ~ d * (1 - exp(-z * luas^f)),
  data = data_model(analisis = "pulau", transform = FALSE),
  start = list(d = 21.9586, z = 0.0125515, f = 0.608648)
)
m_7_pulau_trans <- nls(
  banyak_spesies ~ d * (1 - exp(-z * luas^f)),
  data = data_model(analisis = "pulau", transform = TRUE),
  start = list(d = 33.1737, z = 0.0226077, f = 2.78488)
)
m_7_transek_ntrans <- nls(
  banyak_spesies ~ d * (1 - exp(-z * luas^f)),
  data = data_model(analisis = "transek", transform = FALSE),
  start = list(d = 7.5056, z = 0.00474649, f = 1.28947)
)
m_7_transek_trans <- nls(
  banyak_spesies ~ d * (1 - exp(-z * luas^f)),
  data = data_model(analisis = "transek", transform = TRUE),
  start = list(d = 7.56392, z = 0.0783054, f = 4.27308)
)
m_7_subtransek_ntrans <- nls(
  banyak_spesies ~ d * (1 - exp(-z * luas^f)),
  data = data_model(analisis = "subtransek", transform = FALSE),
  start = list(d = 2.66049, z = 0.013117, f = 1.24835)
)
m_7_subtransek_trans <- nls(
  banyak_spesies ~ d * (1 - exp(-z * luas^f)),
  data = data_model(analisis = "subtransek", transform = TRUE),
  start = list(d = 2.67584, z = 0.239406, f = 3.3757)
)

# Ringkasan model
df_rss <- data.frame(
  ID = character(),
  model = numeric(),
  tingkat = character(),
  transform = character(),
  rss = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:7) {
  for (j in c("pulau", "transek", "subtransek")) {
    for (k in c("ntrans", "trans")) {
      # Get model name
      model_name <- paste0("m_", i, "_", j, "_", k)
      
      # Extract model object
      model <- get(model_name)
      
      # Calculate residuals sum of squares
      nilai_rss <- sum(residuals(model)^2)
      
      # Add row to data frame
      df_rss <- rbind(
        df_rss, data.frame(
          ID = model_name,
          model = i,
          tingkat = j,
          transform = k,
          rss = nilai_rss
        )
      )
    }
  }
}

data_rss <- tibble(df_rss)
nama_model <- tibble(
  model = 1:7,
  nama_model = c("Linear", "Pangkat", "Logaritma", "Eksponensial Negatif", "Fungsi Rasional", "Logistik", "Weibull")
)
data_rss <- left_join(data_rss, nama_model, by = join_by(model)) %>% 
  mutate(
    tingkat = str_to_title(tingkat),
    transform = ifelse(transform == "ntrans", "Luas", "log(Luas)")
  ) %>% 
  group_by(tingkat, transform) %>% 
  mutate(
    peringkat = rank(rss, ties.method = "average")
  ) %>% 
  ungroup() %>% 
  select(ID, model, nama_model, peringkat, tingkat, transform, rss) %>% 
  arrange(tingkat, transform)

## Visualiasi galat ----
data_rss %>% 
  mutate(nama_model = fct_reorder(nama_model, peringkat, .fun = mean)) %>% 
  ggplot(aes(x = rss, y = nama_model)) + 
  geom_col(aes(fill = rss), show.legend = FALSE) + 
  facet_grid(facets = tingkat ~ transform) + 
  scale_fill_viridis_c(name = "JKG", option = "D") + 
  theme_minimal() + 
  theme(
    axis.title.y = element_blank()
  ) + 
  labs(
    x = "Jumlah Kuadrat Galat"
  )

