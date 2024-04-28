library(tidyverse)

dat <- daylight_data %>% 
  select(
    city, date, sunrise_time, sunset_time,
    AST_start, AST_end, NT_start, NT_end, CT_start, CT_end
  ) %>% 
  rename(
    day_start = sunrise_time,
    day_end = sunset_time
  ) %>% 
  mutate(
    city = str_to_title(str_replace(city, "-", " ")),
    day_start = hms::parse_hm(day_start),
    day_end = hms::parse_hm(day_end),
    AST_start = hms::parse_hm(AST_start),
    AST_end = hms::parse_hm(AST_end),
    NT_start = hms::parse_hm(NT_start),
    NT_end = hms::parse_hm(NT_end),
    CT_start = hms::parse_hm(CT_start),
    CT_end = hms::parse_hm(CT_end),
  )
day_dat <- dat %>% 
  select(city, date, day_start, day_end) %>% 
  mutate(event = rep("Day", nrow(dat))) %>% 
  rename(start = day_start, end = day_end)
AST_dat <- dat %>% 
  select(city, date, AST_start, AST_end) %>% 
  mutate(event = rep("Astronomical Twilight", nrow(dat))) %>% 
  rename(start = AST_start, end = AST_end)
NT_dat <- dat %>% 
  select(city, date, NT_start, NT_end) %>% 
  mutate(event = rep("Nautical Twilight", nrow(dat))) %>% 
  rename(start = NT_start, end = NT_end)
CT_dat <- dat %>% 
  select(city, date, CT_start, CT_end) %>% 
  mutate(event = rep("Civil Twilight", nrow(dat))) %>% 
  rename(start = CT_start, end = CT_end)
tidy_dat <- bind_rows(
  day_dat,
  CT_dat,
  NT_dat,
  AST_dat
) %>% 
  mutate(
    event = factor(
      event,
      c(
        "Astronomical Twilight", "Nautical Twilight",
        "Civil Twilight", "Day"
      )
    )
  ) %>% 
  arrange(city, date)
tidy_dat_filtered <- tidy_dat %>% 
  filter(
    date >= as.Date("2023-01-01"),
    date <= as.Date("2023-12-31"),
    city %in% c("Perth", "New York")
  )
tidy_dat_filtered %>% 
  ggplot(aes(x = date, ymin = start, ymax = end, fill = event)) + 
  geom_ribbon() + 
  scale_y_time(
    limits = c(hms::parse_hm("00:00"), hms::parse_hm("24:00"))
  ) +
  facet_wrap(vars(city), ncol = 2) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom"
  )


