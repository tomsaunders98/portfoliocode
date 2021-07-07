library(tidyverse)
library(InewsTheme)

uptake <- read.csv("uptake.csv") %>%
  mutate(
    date = as.Date(date, format="%d/%m/%Y")
  ) %>%
  rename(
    first_dose = cumVaccinationFirstDoseUptakeByPublishDatePercentage,
    second_dose = cumVaccinationSecondDoseUptakeByPublishDatePercentage
  ) %>%
  select(date, first_dose, second_dose) %>%
  mutate(
    first_dose = first_dose - second_dose
  ) %>%
  pivot_longer(c("first_dose", "second_dose"), names_to="d_type", values_to="uptake") %>%
  mutate(
    uptake = uptake/100,
    d_type = ifelse(d_type == "first_dose", "First dose", "Second dose")
  )

ggplot(data=uptake,aes(x=date, y=uptake, group = d_type, fill = d_type))+
  geom_area(aes(group = d_type)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), labels=scales::percent) +
  theme_inews_basic() +
  labs(title = "Covid-19 vaccination uptake", subtitle="Vaccination uptake (%) for first and second dose", caption = "source: Covid-19 dashboard \n © Inews") +
  scale_fill_inews()

save_inews("uptake_test.png")

vars <- read.csv("deet_on_var.csv") %>%
  mutate(
    week = as.Date(week, format = "%d/%m/%Y")
  ) %>%
  head(-1) %>%
  pivot_longer(!week, names_to="variant", values_to="prev")
  

ggplot(data=vars,aes(x=week, y=prev, group = variant, fill = variant))+
  geom_area(aes(group = variant)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), labels=scales::percent) +
  theme_inews_basic() +
  labs(title = "Covid-19 variant prevalence", subtitle="UK prevalence for four 'variants of concern' in UK", caption = "source: PHE technical briefing 15 \n © Inews") +
  scale_fill_inews()


save_inews("varplot.png")

