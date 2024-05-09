library(tidyverse)
library(readxl)
library(ggExtra)
palet_warna <- c(
  "#E32D91", "#4EA6DC", "#4775E7",
  "#8971E1", "#C830CC"
)

# Vertebrata ----

## Mempersiapkan data ----
# Data to: Remotely sensed tree height and density explain global gliding vertebrate richness, https://doi.org/10.5061/dryad.j6q573nm4
data_vert <- read_csv("data/data_1x1_RS_verts.csv")

dat <- data_vert %>% 
  select(
    country, island,
    vert_all, mam_all, amp_all, rep_all,
    x, y
  ) %>% 
  rename(
    negara = country,
    pulau = island,
    n_vert = vert_all,
    n_mam = mam_all,
    n_amf = amp_all,
    n_rep = rep_all,
    bujur = x,
    lintang = y
  )
dat_panjang <- dat %>% 
  pivot_longer(
    cols = starts_with("n_"),
    names_to = "jenis",
    values_to = "banyak_spesies"
  ) %>% 
  mutate(
    jenis = ifelse(
      jenis == "n_vert", "Vertebrata",
      ifelse(
        jenis == "n_mam", "Mamalia",
        ifelse(
          jenis == "n_amf", "Amfibi",
          ifelse(
            jenis == "n_rep", "Reptil", NA
          )
        )
      )
    )
  )
dat_reptil_sederhana <- dat_panjang %>% 
  filter(
    jenis == "Reptil",
    banyak_spesies > 0
  ) %>% 
  mutate(
    bujur = round(bujur, 2),
    lintang = round(lintang, 2)
  ) %>% 
  group_by(jenis, lintang) %>% 
  summarise(
    banyak_spesies = sum(banyak_spesies, na.rm = TRUE),
    .groups = "drop"
  )

## Visualisasi data ----
ragam_reptil_tanpa_trans <- dat_panjang %>% 
  filter(
    jenis == "Reptil",
    banyak_spesies > 0
  ) %>% 
  ggplot(aes(x = lintang, y = banyak_spesies)) + 
  geom_point(alpha = .05, color = "#E32D91") + 
  theme_minimal(base_size = 8) + 
  labs(
    x = "Lintang (Derajat)",
    y = "Banyak Spesies",
    caption = "Data: B. Wagner, H. Kreft, C. Nitschke, & J. Schrader / Dryad / CC0 1.0 DEED"
  )

model_ragam_reptil_tanpa_trans <- ragam_reptil_tanpa_trans + 
  geom_line(
    stat = "smooth",
    method = "lm",
    formula = y ~ poly(x, 2),
    color = "#4EA6DC",
    linewidth = 1
  )

model_ragam_reptil <- ragam_reptil_tanpa_trans + 
  geom_line(
    stat = "smooth",
    method = "lm",
    formula = y ~ poly(x, 2),
    color = "#4EA6DC",
    linewidth = 1
  ) + 
  scale_y_continuous(
    transform = "log1p"
  )

ragam_reptil_tanpa_trans_dist <- ggMarginal(
  ragam_reptil_tanpa_trans,
  type = "histogram",
  fill = "#E32D91",
  color = "white",
  size = 5,
  bins = 10
)

model_ragam_reptil_tanpa_trans_dist <- ggMarginal(
  model_ragam_reptil_tanpa_trans,
  type = "histogram",
  fill = "#E32D91",
  color = "white",
  size = 5,
  bins = 10
)

model_ragam_reptil_dist <- ggMarginal(
  model_ragam_reptil,
  type = "histogram",
  fill = "#E32D91",
  color = "white",
  size = 5
)

ggsave(
  filename = "plot/model_ragam_reptil_tanpa_trans_dist.png",
  plot = model_ragam_reptil_tanpa_trans_dist,
  width = 9.6,
  height = 5.4,
  units = "cm",
  dpi = 300,
  bg = "white"
)

ragam_reptil_trans <- dat_reptil_sederhana %>% 
  ggplot(aes(x = lintang, y = banyak_spesies)) + 
  geom_point(alpha = .05) + 
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, degree = 2),
    se = TRUE, alpha = .2
  ) + 
  scale_color_viridis_d() + 
  scale_y_continuous(
    transform = "log10",
    labels = scales::label_number_auto()
  ) + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  labs(
    x = "Lintang (Derajat)",
    y = "Banyak Spesies",
    caption = "Data: B. Wagner, H. Kreft, C. Nitschke, & J. Schrader / Dryad / CC0 1.0 DEED"
  )

ggMarginal(
  ragam_reptil_trans,
  type = "histogram",
  fill = "grey",
  size = 10
)

# Tumbuhan kayu ----

## Mempersiapkan data ----
# Data: A new dataset on plant occurrences on small islands, including species abundances and functional traits across different spatial scales, https://doi.org/10.3897/BDJ.8.e55275

data_spesies_pulau <- read_excel("data/data_pulau.xlsx") %>% 
  select(island_area, species_number) %>% 
  filter(species_number > 0) %>% 
  rename(
    luas = island_area,
    banyak_spesies = species_number
  )

## Visualisasi data ----
data_spesies_pulau %>% 
  ggplot(aes(x = luas, y = banyak_spesies)) + 
  geom_point(size = 3) + 
  theme_minimal()
