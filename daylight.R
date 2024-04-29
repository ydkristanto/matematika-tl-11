library(rvest)
library(tidyverse)

# Data collection from timeanddate.com ----
# Function to scrape and tidy the table for a specific month and year
scrape_and_tidy_table <- function(month, year, city) {
  # Construct the URL
  url <- paste0("https://www.timeanddate.com/sun/", city, "?month=", month, "&year=", year)
  
  # Read the HTML content of the website 
  webpage <- read_html(url)
  
  # Select the table using CSS selector 
  table_node <- html_nodes(webpage, "table")
  
  # Extract the table content 
  table_content <- html_table(table_node)[[2]]
  
  # Tidying data
  table_content_1 <- table_content
  colnames(table_content_1) <- paste0("var", 1:ncol(table_content))
  table_content_1 <- table_content_1 %>% 
    select(1:13) %>% 
    slice(-(1:2), -n()) %>% 
    drop_na() %>% 
    mutate(
      var1 = as.integer(var1),
      month = month,
      year = year,
      city = city
    )
  
  table_content_2 <- table_content[,14:26]  %>% 
    drop_na()
  colnames(table_content_2) <- paste0("var", 1:ncol(table_content_2))
  table_content_2 <- table_content_2 %>% 
    mutate(
      var1 = as.integer(var1),
      month = month,
      year = year,
      city = city
    )
  
  table_content_tidy <- bind_rows(
    table_content_1,
    table_content_2
  )
  
  return(table_content_tidy)
}

# Initialize an empty list to store the scraped data frames
all_data <- list()

# Iterate over years, months, and cities
cities <- c(
  "austria/linz",
  "australia/perth",
  "france/paris",
  "indonesia/denpasar",
  "japan/tokyo",
  "kenya/nairobi",
  "mexico/mexico-city",
  "south-korea/seoul",
  "turkey/istanbul",
  "uk/london",
  "usa/new-york",
  "indonesia/yogyakarta",
  "indonesia/jakarta"
)

for (year in 2021:2030) {
  for (month in 1:12) {
    for (city in cities) {
      # Scrape and tidy the table for the current month, year, and city
      scraped_data <- scrape_and_tidy_table(month, year, city)
      
      # Append the scraped data to the list
      all_data[[length(all_data) + 1]] <- scraped_data
    }
  }
}

# Combine all scraped data frames into a single data frame
daylight_data_2 <- bind_rows(all_data) %>%
  mutate(download_timestamp = Sys.time())

daylight_data_2 <- daylight_data_2 %>% 
  mutate(
    day = var1,
    sunrise_time = str_extract(var2, "\\d{2}:\\d{2}"),
    sunset_time = str_extract(var3, "\\d{2}:\\d{2}"),
    day_length = as.numeric(hms(var4)) / 3600,
    day_length_diff = var5,
    AST_start = var6,
    AST_end = var7,
    NT_start = var8,
    NT_end = var9,
    CT_start = var10,
    CT_end = var11,
    SN_time = str_extract(var12, "\\d{2}:\\d{2}"),
    SN_mil_km = as.numeric(gsub(",", "", var13)),
    date = as.Date(paste(year, month, day, sep = "-")),
    country = str_split(city, "/", simplify = TRUE)[, 1],
    city = str_split(city, "/", simplify = TRUE)[, 2]
  ) %>% 
  select(-contains("var"))

sun_data <- bind_rows(
  daylight_data,
  daylight_data_2
)

# View the combined data
print(daylight_data)

save(
  sun_data,
  file = "data/sun_data.RData"
)


## Daylength plot ----
daylight_data %>% 
  filter(
    city %in% c("denpasar", "linz"),
    date >= as.Date("2023-01-01"),
    date <= as.Date("2024-12-31")
  ) %>% 
  mutate(
    city = str_to_title(city)
  ) %>% 
  ggplot(aes(x = date, y = day_length)) +
  geom_point(
    aes(color = city),
    size = 3,
    alpha = .2
  ) +
  scale_color_manual(
    name = "City",
    values = c("Denpasar" = "#4FA6DC", "Linz" = "#E23092")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = .5
    ),
    legend.position = "bottom"
  ) +
  labs(
    title = "Comparison of Daylength between Denpasar and Linz",
    x = "Date",
    y = "Daylength (hours)"
  )

## Sunset plot ----
daylight_data %>% 
  filter(
    city %in% c("denpasar", "linz"),
    date >= as.Date("2023-01-01"),
    date <= as.Date("2024-12-31")
  ) %>% 
  mutate(
    city = str_to_title(city),
    sunset_time = as.numeric(hm(sunset_time)) / 3600
  ) %>% 
  ggplot(aes(x = date, y = day_length)) +
  geom_point(
    aes(color = city),
    size = 3,
    alpha = .3
  ) +
  scale_color_manual(
    name = "City",
    values = c("Denpasar" = "#4FA6DC", "Linz" = "#E23092")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = .5
    ),
    legend.position = "bottom"
  ) +
  labs(
    title = "Comparison of Daylength between Denpasar and Linz",
    x = "Date",
    y = "Daylength (hours)"
  )

# Data {suncalc} ----
library(tidyverse)
library(suncalc)
library(lutz)

## Persiapan data ----
load("data/koordinat_kota.RData")
koordinat_kota <- koordinat_kota %>% 
  select(kota, lintang, bujur) %>% 
  rename(
    city = kota,
    lat = lintang,
    lon = bujur
  )
date_v <- seq(
  from = as.Date("2023-01-01"),
  to = as.Date("2023-12-31"),
  by = "day"
)
tanggal_koordinat <- data.frame(
  date = rep(date_v, length(koordinat_kota$lat)),
  lat = rep(koordinat_kota$lat, each = length(date_v)),
  lon = rep(koordinat_kota$lon, each = length(date_v)),
  city = rep(koordinat_kota$city, each = length(date_v))
)

## Ambil data ----
dat <- suncalc::getSunlightTimes(
  data = tanggal_koordinat
) %>% 
  as_tibble() %>%
  left_join(
    koordinat_kota,
    by = join_by(lat, lon)
    ) %>% 
  mutate(
    tz = lutz::tz_lookup_coords(lat, lon, "fast", warn = F)
  )
dat_filtered <- dat %>% 
  filter(city %in% c("New York", "Auckland")) %>% 
  pivot_longer(
    cols = solarNoon:goldenHour,
    names_to = "event",
    values_to = "time"
  ) %>% 
  mutate(
    time_diff = local_time(time, tz = tz, units = "secs")
  ) %>% 
  mutate(time_to = hms::hms(seconds_to_period(time_diff))) %>% 
  filter(event == "sunrise")
dat_filtered %>% 
  ggplot(aes(x = date, y = time_to)) + 
  geom_point(aes(color = city), alpha = .3) + 
  facet_wrap(facets = vars(city), nrow = 2) +
  theme_bw()

# Data Proyek ----



