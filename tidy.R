# Import the raw data and tidy it for analysis

# Import libraries --------------------------------------------------------
library(tidyverse)
library(janitor)
library(snakecase)


# Read the raw files ------------------------------------------------------

cost_attendance_raw <- read_csv("raw-data/cost-attendance.csv") %>% clean_names()


# Tidy the Cost Attendance Table ------------------------------------------

cost_attendance <- cost_attendance_raw %>%
  pivot_longer(cols=-c(unit_id, institution_name, sector_of_institution_hd2021),
               names_to="student_category_year",
               values_to="cost") %>% 
  separate_wider_regex(cols="student_category_year", patterns=c("total_price_for_", student_category = ".*", "_\\d.*_\\d.*drvic", year = ".*", "_rv"), too_few = "align_start") %>% 
  mutate(student_category = to_upper_camel_case(str_remove(student_category, "in_state_students_living_"), sep_out = " "))

write_rds(cost_attendance, file = "data/cost_attendance.rds")

# Example Uses of Tidy Data -----------------------------------------------

# Summary
cost_attendance_summary <- cost_attendance %>%
  filter(!is.na(cost)) %>% 
  group_by(sector_of_institution_hd2021, student_category, year) %>% 
  summarize(mean_cost_per_category_by_sector_and_year = mean(cost)) %>% 
  ungroup()

# Plot
cost_attendance_summary %>%
  rename(sector = sector_of_institution_hd2021) %>% 
  filter(sector %in% c(1,2)) %>% 
  mutate(sector = ifelse(sector == 1, 'Public', 'Private')) %>%
  ggplot(aes(x=year,
             y=mean_cost_per_category_by_sector_and_year,
             fill = factor(student_category, levels = c("Off Campus With Family", "Off Campus Not With Family", "On Campus")))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(limits = c(0,50000), breaks = seq(0, 50000, by = 10000)) + 
  scale_fill_brewer(name = "Student Category", palette = "Accent") +
  labs(
    title = "Tuition Comparison By Year and Student Category",
    x="",
    y="Mean Cost"
  ) +
  theme(legend.position="left",
        panel.background = element_rect(fill = "white", colour="grey"),
        panel.grid.major = element_line(colour = "lightgrey", linetype = 3)) +
  facet_wrap(~sector, ncol = 2)

# Line Plot
cost_attendance_summary %>%
  rename(sector = sector_of_institution_hd2021) %>% 
  filter(sector %in% c(1,2)) %>% 
  mutate(sector = ifelse(sector == 1, 'Public', 'Private')) %>%
  mutate(student_category = factor(student_category, levels = c("Off Campus With Family", "Off Campus Not With Family", "On Campus"))) %>% 
  ggplot(aes(x=year,
             y=mean_cost_per_category_by_sector_and_year,
             group = student_category,
             color = student_category)) +
  geom_line() + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(limits = c(0,50000), breaks = seq(0, 50000, by = 10000)) + 
  scale_color_brewer(name = "Student Category", palette = "Set1") +
  labs(
    title = "Tuition Comparison By Year and Student Category",
    x="",
    y="Mean Cost"
  ) +
  theme(legend.position="left",
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "white", colour="grey"),
        panel.grid.major = element_line(colour = "lightgrey", linetype = 3)) +
  facet_wrap(~sector)
  
