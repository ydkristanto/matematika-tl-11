# Paket ----
library(tidyverse)

# Tayangan konten digital ----
tayangan_konten_2018 <- read_csv("data/tayangan-konten-2018.csv")
tayangan_konten_2018 %>% 
  ggplot(
    aes(x = Tanggal, y = Penayangan)
  ) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 4),
    se = TRUE
  ) +
  theme_bw()
