library(tidyverse)
library(readxl)

# Data ----
# Data: https://www.fs.usda.gov/rds/archive/Catalog/RDS-2016-0005
data_pohon <- read_csv("data/TS3_Raw_tree_data.csv")
# Data: http://repository.ipb.ac.id/handle/123456789/10813
data_pohon_aminudin <- read_excel("data/data_pohon_aminudin.xlsx")
pohon_jati_2 <- data_pohon_aminudin %>% 
  filter(
    jenis == "Jati",
    plot ==2
  ) %>% 
  mutate(
    ln_diameter = log(diameter_cm)
  ) %>% 
  select(
    diameter_cm, ln_diameter, tinggi_m
  )


# Eksplorasi
data_pohon %>% 
  group_by(TreeType, CommonName) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  arrange(TreeType, -n) %>% 
  print(n = 201)
data_cem <- data_pohon %>% 
  select(TreeType, ScientificName, CommonName, Age, `DBH (cm)`, `TreeHt (m)`) %>% 
  filter(TreeType == "CEM")
data_pohon_aminudin %>% 
  group_by(jenis) %>% 
  summarise(n = n(), .groups = "drop")

# Visualisasi data ----
data_pohon %>% 
  filter(TreeType == "CEM") %>% 
  ggplot(aes(x = `DBH (cm)`, y = `TreeHt (m)`)) + 
  geom_point() + 
  geom_smooth()
data_cem %>% 
  ggplot(aes(x = `DBH (cm)`, y = `TreeHt (m)`, color = CommonName)) + 
  geom_smooth(alpha = .3) + 
  geom_point(size = 1.5, alpha = .6) + 
  
  facet_wrap(facets = vars(CommonName), ncol = 2) + 
  scale_color_viridis_d(direction = -1) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(
    x = "Diameter Batang Setinggi Dada (cm)",
    y = "Tinggi (m)",
    caption = "Data: McPherson dkk. / Forest Service Research Data Archive"
  )
ggsave(
  filename = "plot/diameter_tinggi_pohon.png",
  plot = last_plot(),
  width = 1280,
  height = 1280,
  units = "px",
  bg = "white"
)
pohon_jati_2 %>% 
  ggplot(aes(x = diameter_cm, y = tinggi_m)) + 
  geom_point() + 
  geom_smooth() + 
  theme_minimal()
pohon_jati_2 %>% 
  select(tinggi_m, diameter_cm) %>% 
  print(n = 31)


