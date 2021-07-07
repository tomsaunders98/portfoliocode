library(InewsTheme)
library(tidyverse)

vac_uk_ov <- read.csv("../venv/out/vaccine_uk_ov.csv")

tot_vac <- vac_uk_ov %>%
  select(date, cumVaccinesGivenByPublishDate) %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d")
  ) %>%
  rename(
    vac = cumVaccinesGivenByPublishDate
  )
no_vac <- max(tot_vac$vac)

ggplot(data=tot_vac,aes(x=date, y=vac, colour = "#E33A11"))+
  geom_line() +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), labels=scales::comma) +
  theme_inews_basic() +
  labs(caption = "source: Covid-19 dashboard \n Copyright: I") +
  theme(
    legend.position = "none",
    plot.caption = element_text(size = rel(0.5))
  ) +
  annotate("text", family = "Bitter", colour="#ea392c", fontface="bold", size=24, x = as.Date("30/01/2021", format= "%d/%m/%Y"), y = 50000000, label = "i") +
  annotate("text", family = "Bitter", fontface="bold", size=20, x = as.Date("05/04/2021", format= "%d/%m/%Y"), y = 50000000, label = "Vaccine") +
  annotate("text", family = "Bitter", fontface="bold", size=13, x = as.Date("26/03/2021", format= "%d/%m/%Y"), y = 33000000, label = "Tracker")

save_inews("cov_photo.png")