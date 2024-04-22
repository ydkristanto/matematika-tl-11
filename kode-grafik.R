# Paket ----
library(tidyverse)

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
