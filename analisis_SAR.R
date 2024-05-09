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
