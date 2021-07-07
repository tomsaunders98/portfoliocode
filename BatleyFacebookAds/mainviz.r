library(rjson)
library(InewsTheme)
library(tidyverse)
library(jsonlite)
library(grid)

bspen <- read.csv("bspen.csv", na.strings = "nan")
bspen_wd <- jsonlite::fromJSON(txt="bspen.json")

batley <- c("batley", "stephenson", "leadbeater")
amersham <- c("amersham", "natasa", "fleet")
hartlepool <- c("williams", "mortimer", "hartelpool")

flat_demos <- bspen_wd %>%
  unnest(demos) %>%
  rename(
    spend = m_spend
  ) %>%
  mutate(
    funder = ifelse(funder == "Jill Mortimer", "The Conservative Party", funder),
    percentage = as.numeric(percentage),
    elec = ifelse(elec %in% batley, "batley", ifelse(elec %in% amersham, "amersham", "hartlepool")),
    spend = as.numeric(spend)
  ) %>%
  group_by(elec, funder, age, url) %>%
  mutate(
    ngperc = sum(percentage)
  ) %>% ungroup %>%
  pivot_wider(names_from = gender, values_from = percentage) %>%
  select(!c(unknown, male, female)) %>%
  mutate(
    spend_adj = ngperc * spend
  ) %>%
  unite("id", c(funder, elec), sep = "_", remove = FALSE) %>%
  mutate(
    elec = tools::toTitleCase(elec)
  )

bspen_t_spend <- bspen %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d"),
    elec = ifelse(elec %in% batley, "batley", ifelse(elec %in% amersham, "amersham", "hartlepool")),
    funder = ifelse(funder == "Jill Mortimer", "The Conservative Party", funder)
  ) %>%
  group_by(elec, funder) %>%
  summarise(
    spending = sum(m_spend)
  ) %>%
  unite("id", c(funder, elec), sep = "_")

flat_demos <- merge(flat_demos, bspen_t_spend, by = "id")

flat_demos <- flat_demos %>%
  group_by(elec, funder, age) %>%
  summarise(
    perc = sum(spend_adj)/spending
  ) %>% unique

ggplot(flat_demos, aes(x = funder, fill = age, y = perc)) +
  facet_wrap(vars(elec), ncol = 1) +
  geom_bar( stat="identity", position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_inews() +
  theme_inews_facet() +
  labs(title = "Age split of Facebook adverts", subtitle = "As a % of total spent by party", caption = "Facebook Ad Library")

save_inews("demos.png", type="facet", height_i = 15)

################### 
## By Age


flat_demos_g <- bspen_wd %>%
  rename(
    spend = m_spend
  ) %>%
  unnest(demos) %>%
  mutate(
    funder = ifelse(funder == "Jill Mortimer", "The Conservative Party", funder),
    percentage = as.numeric(percentage),
    elec = ifelse(elec %in% batley, "batley", ifelse(elec %in% amersham, "amersham", "hartlepool")),
    spend = as.numeric(spend)
  ) %>%
  group_by(elec, funder, gender, url) %>%
  mutate(
    ngperc = sum(percentage)
  ) %>% ungroup %>%
  pivot_wider(names_from = age, values_from = percentage) %>%
  select(funder, elec, ngperc, gender, spend) %>%
  mutate(
    spend_adj = ngperc * spend
  ) %>%
  unite("id", c(funder, elec), sep = "_", remove = FALSE) %>%
  mutate(
    elec = tools::toTitleCase(elec),
    gender = tools::toTitleCase(gender)
  )


flat_demos_g <- merge(flat_demos_g, bspen_t_spend, by = "id")

flat_demos_g <- flat_demos_g %>%
  group_by(elec, funder, gender) %>%
  summarise(
    perc = sum(spend_adj)/spending
  ) %>% unique

ggplot(flat_demos_g, aes(x = funder, fill = gender, y = perc)) +
  facet_wrap(vars(elec), ncol = 1) +
  geom_bar( stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_inews() +
  theme_inews_facet() +
  labs(title = "Gender split of Facebook adverts", subtitle = "As a % of total spent by party", caption = "Facebook Ad Library")

save_inews("demos_gender.png", type="facet", height_i = 15)




bspen_spen <- bspen %>%
  subset(elec == "batley") %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d")
  ) %>%
  group_by(funder, date) %>%
  summarise(
    spending = sum(m_spend),
    ads = n(),
    t_imp = sum(impact),
    t_l_imp = sum(l_impact),
    t_u_imp = sum(u_impact)
  ) 


ggplot(bspen_spen, aes(x = date, group = funder, colour = funder, fill = funder)) +
  geom_line(aes(y = cumsum(t_imp))) +
  geom_ribbon(aes(ymin = cumsum(t_l_imp), ymax = cumsum(t_u_imp)), alpha = 0.5, colour = NA) +
  theme_inews_basic() +
  scale_colour_manual(values = c("#152eea", "#a52303")) +
  scale_fill_manual(values = c("#a6cee3", "#e33a11")) +
  labs(title="Ad Impressions in Batley and Spen election", subtitle="Estimated impressions for Conservatives/Labour", caption="source: Facebook Ad Library") +
  scale_y_continuous(labels=scales::comma)
        
save_inews("cumspendbat.png")



bspel_all <- bspen %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d"),
    elec = ifelse(elec %in% batley, "batley", ifelse(elec %in% amersham, "amersham", "hartlepool")),
    funder = ifelse(funder == "Jill Mortimer", "The Conservative Party", funder)
  ) %>%
  group_by(elec, funder, date) %>%
  summarise(
    spending = sum(m_spend),
    ads = n(),
    l_sp = sum(l_spend),
    u_sp = sum(u_spend)
  ) %>%
  mutate(
    t_sp = cumsum(spending),
    t_ads = cumsum(ads),
    t_l_sp = cumsum(l_sp),
    t_u_sp = cumsum(u_sp),
    elec = tools::toTitleCase(elec)
  )
  


ggplot(bspel_all, aes(x = date, group = funder, colour = funder)) +
  scale_colour_manual(values = c("#a6cee3", "#e33a11")) +
  facet_wrap(vars(elec), scales="free_x") +
  geom_line(aes(y = t_ads)) +
  theme_inews_facet() +
  labs(title="Number of ads placed in by-elections (2021)", subtitle="Total ads for Conservative/Labour candidates", caption="source: Facebood Ad Library") +
  theme(
    axis.text.x = element_text(size = rel(0.5)),
    axis.ticks.x = element_line(size = rel(0.5)),
    plot.caption = element_text(size = rel(0.5))
  )

save_inews("allspendelec.png", type="facet")

adsp <- read.csv("adsp.csv")

ggplot(adsp, aes(x = Name, y = Spend, fill = Name)) +
  facet_wrap(vars(elec), ncol = 1, scales = "free_y") +
  geom_bar(stat="identity") +
  theme_inews_basic() +
  labs(title="Total ad spending in Batley and Spen", subtitle="FB spending for Conservative/Labour pages (£)", caption="source: Facebood Ad Library") +
  scale_fill_manual(values = c("#a6cee3", "#e33a11", "#db9213")) +
  coord_flip()
  

save_inews("tspend.png")



