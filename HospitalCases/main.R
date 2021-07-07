library(tidyverse)
library(InewsTheme)
library(grid)

cases <- read.csv("cases.csv") %>%
  mutate(
    date = as.Date(date, format="%d/%m/%Y")
  ) %>%
  rename(
    adm = newAdmissionsRollingRate,
    case = newCasesBySpecimenDateRollingRate
  ) %>%
  select(date, adm, case) %>%
  mutate(
    casebyadm = (adm/case),
    adm = adm,
    adm_r = adm,
    case = case,
    case_r = case
  ) %>%
  rename(
    Admissions = adm,
    Cases = case
  ) %>%
  pivot_longer(!c(date, case_r, adm_r, casebyadm), names_to="type", values_to="rate") %>%
  mutate(
    alpha_v = ifelse(type == "Cases per admission", 1, 0.2),
    chart = ifelse(date < as.Date("01/05/2021", format="%d/%m/%Y"), "Second Wave", "Current Wave")
  ) %>%
  subset(
    date >as.Date("01/08/2020", format="%d/%m/%Y")
  ) %>%
  subset(
    !(date > as.Date("01/11/2020", format="%d/%m/%Y") & date < as.Date("01/05/2021", format="%d/%m/%Y"))
  ) %>%
  drop_na() %>%
  mutate(
    pct_inc_case = ifelse(chart=="Second Wave", case_r/8.1, case_r/20.9),
    pct_inc_adm = ifelse(chart=="Second Wave", adm_r/1.3, adm_r/1.3)
  ) %>% select(pct_inc_case,pct_inc_adm,date,chart) %>%
  mutate(
    pct_inc_case_r = pct_inc_case,
    pct_inc_adm_r = pct_inc_adm
  ) %>%
  pivot_longer(!c(date, pct_inc_case_r, pct_inc_adm_r, chart), names_to="type", values_to="rate") %>%
  mutate(
    type = ifelse(type=="pct_inc_adm", "Admissions", "Cases")
  )

cases$chart <- factor(cases$chart, levels=c("Second Wave", "Current Wave"))

ggplot(cases) +
  facet_wrap(vars(chart), scales="free") +
  geom_line(aes(x = date, y = rate, group = type, colour = type)) +
  geom_ribbon(aes(x=date, ymin = pct_inc_adm_r, ymax=pct_inc_case_r), fill = "#828282", alpha=0.5) +
  theme_inews_basic() +
  scale_colour_inews() +
  scale_y_continuous(labels = scales::percent, trans="log10") +
  labs(title = "Covid cases", subtitle = "% inc in cases and admissions from wave start", caption = "source: Covid-19 dashboard") +
  scale_alpha_continuous(limits = c(0, 1),guide="none")
save_inews("test.png")

cba <- read.csv("case_by_age.csv") %>%
  pivot_longer(!Measure, names_to="date", values_to="adm") %>%
  mutate(
    date = as.Date(date, format="X%d.%m.%Y"),
    chart = ifelse(date < as.Date("01/03/2021", format="%d/%m/%Y"), "Second Wave", "Current Wave")
  ) %>%
  subset(
    date >as.Date("01/08/2020", format="%d/%m/%Y") & !(Measure == "Unknown" | Measure =="Total")
  ) %>%
  drop_na() %>%
  group_by(Measure) %>%
  mutate(
    rol_c = ra(adm)
  ) %>% ungroup

cba$Measure = factor(cba$Measure, levels=c("0-5", "6-17", "18-54", "55-64", "65-74", "75-84", "85+"))
cba$chart <- factor(cba$chart, levels=c("Second Wave", "Current Wave"))

ggplot(cba, aes(x = date, y = rol_c, group = Measure, colour = Measure)) +
  facet_wrap(vars(chart), scales="free_x") +
  geom_line() +
  theme_inews_basic() +
  scale_colour_inews() +
  scale_y_continuous(trans='log10', labels = scales::comma) +
  labs(title = "Hospital Admissions", subtitle = "Rolling average of admissions by age group", caption = "source: Covid-19 dashboard")

save_inews("test5.png")