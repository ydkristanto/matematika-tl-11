library(haven)
library(tidyverse)

# Data ----
# Data berikut merupakan lampiran dari artikel "The power of nostalgia: Age and preference for popular music", https://doi.org/10.1007/s11002-022-09626-7
lokasi <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs11002-022-09626-7/MediaObjects/11002_2022_9626_MOESM1_ESM.sav"

data_usia_musik <- read_sav(file = lokasi)
data_usia_musik_rapi <- data_usia_musik %>% 
  select(Q1, starts_with("Q19_1_")) %>% 
  pivot_longer(
    cols = -Q1,
    names_to = "tahun_rilis",
    values_to = "rating"
  ) %>% 
  mutate(
    tahun_rilis = 2 * as.integer(sub(".*_", "", tahun_rilis)) + 1948,
    usia_lagu = tahun_rilis - Q1
  ) %>% 
  rename(tahun_lahir = Q1) %>% 
  select(tahun_lahir, tahun_rilis, usia_lagu, rating) %>% 
  drop_na() %>%   # Hilangkan NA di tahun_lahir
  filter(rating <= 11)

n_usia_lagu <- data_usia_musik_rapi %>% 
  group_by(usia_lagu) %>% 
  summarise(n = n(), .groups = "drop")

data_usia_musik_rapi_filter <- left_join(
  data_usia_musik_rapi, n_usia_lagu, by = join_by(usia_lagu)
) %>% 
  filter(n >= 100)



data_usia_lagu_agg <- data_usia_musik_rapi_filter %>% 
  select(usia_lagu, rating, n) %>% 
  group_by(usia_lagu, n) %>% 
  summarise(rerata_rating = mean(rating))
data_usia_lagu_agg %>% 
  print(n = 104)

# Visualisasi Data ----
data_usia_lagu_agg %>% 
  ggplot(aes(x = usia_lagu, y = rerata_rating)) + 
  geom_point(
    aes(color = rerata_rating),
    size = 4,
    alpha = .8,
    show.legend = F
  ) + 
  geom_line(
    stat = "smooth",
    method = "lm",
    formula = y ~ poly(x, degree = 8),
    se = F,
    linewidth = 1.5
  ) + 
  geom_vline(
    xintercept = 15.923,
    linetype = "dashed"
  ) + 
  annotate(
    geom = "label",
    x = 15,
    y = 1.5,
    label = "Usia Lagu: 15,9",
    angle = 90,
    vjust = 0
  ) + 
  scale_color_viridis_c(
    name = "Frekuensi"
  ) + 
  scale_y_continuous(
    breaks = seq(from = 0, to = 10, by = 2),
    limits = c(0, 10)
  ) + 
  theme_minimal() + 
  theme(legend.position = "right") + 
  labs(
    x = "Usia Lagu*",
    y = "Rerata Rating",
    caption = "*Selisih tahun rilis lagu dengan tahun lahir responden"
  )

