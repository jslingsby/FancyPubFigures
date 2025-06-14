library(googlesheets4)
library(googledrive)
library(tidyverse)
library(viridis)
library(readxl)

### Download data from Google Drive
drive_download("https://docs.google.com/spreadsheets/d/1UTI18UGdAi4kL32lYuGNXUcrh-SJQgVT/edit?gid=1278247470#gid=1278247470", overwrite = TRUE)

### Get data
df_list <- map(set_names(excel_sheets("BioTeachingLoads.xlsx")),
               read_excel, path = "BioTeachingLoads.xlsx")

list2env(df_list, envir = .GlobalEnv); rm(df_list)

Fields <- c("Lecturer", "Tot Units", "Tot Wt Units")

dat <- bind_rows(
  select(`2022`, all_of(Fields)),
  select(`2023`, all_of(Fields)),
  select(`2024`, all_of(Fields)),
  select(`2025`, all_of(Fields)), .id = "Year"
)

dat <- dat %>%
  na.omit() %>%
  mutate(Year = dat$Year %>% 
  case_match("1" ~ "2022", 
             "2" ~ "2023", 
             "3" ~ "2024", 
             "4" ~ "2025"))


dat %>%
  ggplot(aes(x = `Tot Units`, fill = Lecturer)) +
  geom_histogram() +
  facet_wrap(~ Year, scales = "free_y") +
  theme_minimal() +
  labs(#title = "Total Teaching Units by Lecturer and Year",
       x = "Teaching Units") +
  #scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("teaching_units.png", width = 8, height = 5)
ggsave("teaching_units.pdf", width = 8, height = 5)


# dat %>%
#   ggplot(aes(x = `Tot Wt Units`, fill = Lecturer)) +
#   geom_histogram() +
#   facet_wrap(~ Year, scales = "free_y") +
#   theme_minimal() +
#   labs(#title = "Total Weighted Teaching Units by Lecturer and Year",
#        x = "Teaching Units") +
#   #scale_fill_viridis(discrete = TRUE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
