library(tidyverse)
library(InewsTheme)
library(janitor)
library(readxl)

scotland <- read.csv("ScotlandCases.csv") %>%
  select(Date:DailyPositive) %>%
  rename(date = Date) %>%
  mutate(
    date = as.character(date)
  )



addleter <- function(text, letter, n){
  lhs <- paste0('^(.{', n-1, '})(.+)$')
  rhs <- paste0('\\1', letter, '\\2')
  return(gsub(lhs, rhs, text))
  
}

scotland$date <- sapply(scotland$date, function(x) addleter(x, "-", 5))
scotland$date <- sapply(scotland$date, function(x) addleter(x, "-", 8))

inc_results <- Sys.Date()

scotland <- scotland %>%
  mutate(
    date = as.Date(date),
    AgeGroup = ifelse(AgeGroup %in% c("0 to 14", "15 to 19"), "<19", "19+")
  ) %>%
  subset(date < (Sys.Date()-3)) %>% #last 2-3 days are incomplete so remove
  group_by(date, AgeGroup) %>%
  summarise(
    cases = sum(DailyPositive, na.rm = T)
  ) %>%
  group_by(date) %>%
  mutate(
    tcase = sum(cases, na.rm = T)
  ) %>% ungroup %>%
  mutate(perc = cases/tcase) %>%
  group_by(AgeGroup) %>%
  arrange(date) %>%
  mutate(
    cases_adj = ra(cases)
  ) %>% ungroup %>%
  mutate(
    AgeGroup = ifelse(AgeGroup == "<19", "0-18", "19+")
  )

ydata <- scotland %>%
  subset(AgeGroup == "0-18") %>%
  arrange(date) %>%
  mutate(
    perc = ra(perc),
    s = ifelse(date %in% c(as.Date("2021-08-10"), as.Date("2021-06-29"), as.Date("2021-04-19")), perc, NA)
  ) %>%
  subset(date > as.Date("2021-04-01"))

ggplot(ydata, aes(x=date)) +
  geom_line(aes(y=perc), colour="#E33A11") +
  geom_point(aes(y = s), colour="#a6cee3") +
  theme_inews_basic() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "% of Covid-19 cases among school age children in Scotland", subtitle = "People aged 0 - 18",  caption = "source: Public Health Scotland")

save_inews("ScotlandPerc.png")


ggplot(scotland, aes(x=date, y=cases_adj, group=AgeGroup, fill=AgeGroup)) +
  scotland %>% subset(AgeGroup == "0-18") %>% head(cases)() +
  theme_inews_basic() +
  scale_colour_inews() +
  labs(title = "Covid-19 cases in Scotland among school age population", caption="source: Public Health Scotland")

save_inews("ScotlandCases.png")


model <- read_excel("infectionsurvey.xlsx", sheet = 9, skip = 4)

mod <- model %>%
  select(Date, starts_with("Age")) %>%
  drop_na() %>%
  mutate(
    Date = convert_to_date(Date)
  ) %>%
  pivot_longer(!Date, names_to="Age", values_to="perc") %>%
  mutate(
    Age = as.numeric(stringr::str_extract(Age, "\\d+")),
    perc = as.numeric(perc)
  )

mod$age_group <- cut(mod$Age, c(0,3,11,18,85))

mod <- mod %>%
  mutate(
    age_group = ifelse(age_group == "(0,3]", "0-3", 
                       ifelse(age_group == "(3,11]", "Primary School",
                              ifelse(age_group == "(11,18]", "Secondary School", "19+")))
  ) %>%
  group_by(Date, age_group) %>%
  summarise(
    perc = mean(perc)
  ) %>%
  select(date = Date, age = age_group, perc)

ggplot(mod, aes(x=date, y =perc, group=age, colour=age)) +
  geom_line()
  
vac <- read.csv("vaccines.csv") %>%
  select(date = Date,Sex,AgeGroup, Population, Dose, nvac = CumulativeNumberVaccinated, pvac = CumulativePercentCoverage) %>%
  mutate(
    date = as.character(date)
  ) %>%
  subset(AgeGroup %in% c("16 - 17", "18 - 29", "30 - 39", "40 years and over")) %>%
  mutate(
    AgeGroup = ifelse(AgeGroup %in% c("18 - 29", "30 - 39"), "18 - 39", AgeGroup)
  )

vac$date <- sapply(vac$date, function(x) addleter(x, "-", 5))
vac$date <- sapply(vac$date, function(x) addleter(x, "-", 8))

vac <- vac %>%
  mutate(
    date = as.Date(date)
  ) %>%
  group_by(date, AgeGroup, Dose) %>%
  summarise(
    nvac = sum(nvac),
    pop = sum(Population),
    pvac = nvac/pop
  )

d1 <- vac %>%
  subset(Dose == "Dose 1")
ggplot(d1, aes(x=date, y=pvac, group=AgeGroup, colour=AgeGroup)) +
  geom_line() +
  theme_inews_basic() +
  labs(title = "First dose take up in Scotland by age group ", caption = "source: Public Health Scotland") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  scale_colour_inews()

save_inews("Scotlandvac.png")