library(rvest)
library(tidyverse)

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
  "usa/new-york"
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
daylight_data <- bind_rows(all_data) %>%
  mutate(download_timestamp = Sys.time())

daylight_data <- daylight_data %>% 
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
    SN_mil_km = var13
  ) %>% 
  select(-contains("var"))

daylight_data <- daylight_data %>% 
  mutate(
    SN_mil_km = as.numeric(gsub(",", "", SN_mil_km))
  )

daylight_data <- daylight_data %>% 
  mutate(
    date = as.Date(paste(year, month, day, sep = "-"))
  )

daylight_data <- daylight_data %>% 
  mutate(
    country = str_split(city, "/", simplify = TRUE)[, 1],
    city = str_split(city, "/", simplify = TRUE)[, 2]
  )

# View the combined data
print(daylight_data)

save(
  daylight_data,
  file = "data/daylight_data.RData"
)
