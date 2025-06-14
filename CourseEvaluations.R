library(googlesheets4)
library(tidyverse)
library(viridis)

dat = read_sheet("https://docs.google.com/spreadsheets/d/1ksIl-C8-tWthVJOR-LF1i77BWyF2DhUekLmV45iqWYM/edit?gid=0#gid=0")

dat %>% select(Year, `Standardised Score`, Topic, Course) %>%
  filter(Topic != "Knowledge") %>%
  mutate(Topic = factor(Topic, 
                        levels = c("Overall", "Difficulty", "Pace", "Preparation", "Communication", "Enthusiasm", "Availability", "Approachability", "Notes", "Slides", "Responses"))) %>%
  na.omit() %>%
  ggplot(aes(x = `Year`, y = `Standardised Score`, colour = `Course`)) +
  geom_point() +
  geom_line() +
  facet_wrap("Topic") +
  theme_minimal() +
  ylim(3,5) +
  scale_color_viridis(discrete = TRUE) + 
  theme(axis.text.x = element_text(size = 8, angle = 315, vjust = 0.5, hjust=-0.1))

ggsave("course_feedback.png", width = 8, height = 5)
ggsave("course_feedback.pdf", width = 8, height = 5)

g + dat %>% select(Year, `Standardised Score`, Topic, Course, Class) %>%
  filter(Topic != "Knowledge", Class == "Preparation") %>%
  na.omit() %>%
  ggplot(aes(x = `Year`, y = `Standardised Score`, colour = `Topic`)) +
  geom_point() +
  geom_line() +
  facet_wrap("Course") +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "magma")
