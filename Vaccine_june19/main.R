library(InewsTheme)
library(tidyverse)
library(ggbeeswarm)
library(modelr)
library(ggrepel)

utla <- read.csv("utla_uptake.csv") %>%
  mutate(
    date = as.Date(date, "%Y-%m-%d")
  ) %>%
  select(
    date,
    location = areaName,
    first_dose = cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,
    second_dose = cumVaccinationSecondDoseUptakeByVaccinationDatePercentage
  )
uk <- read.csv("UK_uptake.csv") %>%
  mutate(
    date = as.Date(date, "%Y-%m-%d")
  ) %>%
  select(
    date,
    uk_first_dose = cumVaccinationFirstDoseUptakeByPublishDatePercentage,
    uk_second_dose = cumVaccinationSecondDoseUptakeByPublishDatePercentage
  ) 




utla <- merge(utla, uk, by="date")



utla <- utla %>%
  group_by(location) %>%
  mutate(
    final_first = ifelse(date == max(date), second_dose, NA),
    blue_ribbon_upper = ifelse(second_dose > uk_second_dose, second_dose, NA),
    blue_ribbon_lower = ifelse(second_dose > uk_second_dose, uk_second_dose, NA),
    red_ribbon_upper = ifelse(second_dose < uk_second_dose, uk_second_dose, NA),
    red_ribbon_lower = ifelse(second_dose < uk_second_dose, second_dose, NA),
    text_color = ifelse(max(second_dose) > max(uk_second_dose), TRUE, FALSE)
    
  ) 


new_levels <- utla %>%
  drop_na(final_first) %>%
  arrange(final_first) %>%
  pull(location)

utla$location = factor(utla$location, levels=new_levels) 


ggplot(utla, aes(x = date)) +
  facet_wrap(vars(location), ncol=5, labeller = label_wrap_gen(width=20)) +
  geom_line(aes(y = second_dose)) +
  geom_line(aes(y = uk_second_dose), linetype="dashed", colour="#454647") +
  geom_ribbon(aes(ymin = red_ribbon_lower, ymax = red_ribbon_upper, na.rm=T), fill="#E33A11")  +
  geom_ribbon(aes(ymin = blue_ribbon_lower, ymax = blue_ribbon_upper, na.rm=T), fill="#a6cee3") +
  geom_text(aes(label = round(final_first), y = final_first, colour = text_color), nudge_y = -40, nudge_x = -10, na.rm = TRUE) + 
  theme_inews_facet() +
  scale_colour_inews() +
  theme(
    plot.caption = element_text(hjust=0.5, margin = margin(t=1, unit="cm")),
    plot.title = element_text(hjust=0.5, margin=margin(b=10)),
    plot.subtitle = element_text(hjust=0.5),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "County vaccination coverage vs UK average", subtitle="The % of eligible population with two vaccine doses", caption="source: ONS")

save_inews("county.png", width_i = 20, height_i = 150, l_size=FALSE)


##IMD
ltla <- read.csv("ltla_uptake.csv") %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d")
  ) %>%
  select(
    first_dose = cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,
    second_dose = cumVaccinationSecondDoseUptakeByVaccinationDatePercentage,
    code = areaCode,
    location = areaName,
    date,
  ) %>%
  group_by(location) %>%
  subset(date == max(date)) %>% select(!date)

imd <- read.csv("imd.csv")
imd$rank_group <- cut_interval(imd$score, 3, labels=F)
imd <- imd %>% select(
  code, rank_group,score
)
ltla <- merge(ltla, imd, by="code")




pop <- read.csv("pop.csv")
ltla <- merge(ltla, pop, by="code")

region <- read.csv("region.csv") %>%
  select(
    code = LAD21CD,
    region = RGN21NM
  )
ltla <- merge(ltla, region, by="code") %>%
  mutate(
    first_dose = first_dose/100,
    second_dose = second_dose/100
  ) %>%
  select(second_dose,rank_group,pop,location,region)

ggplot(ltla, aes(x = second_dose, y = rank_group, colour = region, size = pop)) +
  geom_beeswarm(groupOnX = FALSE, cex = 1.5, priority = "descending") +
  theme_inews_basic() +
  scale_colour_inews() +
  scale_size(guide="none") +
  scale_y_reverse(breaks = c(5,4,3,2,1)) +
  scale_x_continuous(labels=scales::percent) +
  labs(title = "Second dose coverage by deprivation + region", subtitle = "Size indicates population, 3 implies most deprivated areas*", caption="*Deprivation scored using indices of multiple deprivation \nsource: ONS/Covid-19 Dashboard")

save_inews("depriv.png", width_i = 25, height_i = 15)
### Projection

#UK Raw numbers as % of total pop for projection

uk <- read.csv("ukpop_proj.csv") %>%
  mutate(
  date = as.Date(date, "%Y-%m-%d")
) %>%
  select(
    date,
    uk_first_dose = cumPeopleVaccinatedFirstDoseByPublishDate,
    uk_second_dose = cumPeopleVaccinatedSecondDoseByPublishDate
  ) %>%
  mutate(
    uk_first_dose = uk_first_dose/67081234,
    uk_second_dose = uk_second_dose/67081234
  )





uk$projects <- cut_interval(uk$date, 25, labels=F)
uk_proj <- uk %>%
  select(projects, dose = uk_second_dose, date) %>% #Select Dose here
  group_by(projects) %>%
  do(
    proj = lm(dose~date, data=.)
  ) %>% select(projects, proj)

date_vals <- uk %>%
  group_by(projects) %>%
  mutate(
    d_proj=list(seq(max(date), as.Date("2021/12/30"), by = "day"))
  ) %>% select(projects, d_proj) %>% distinct(projects, .keep_all = T)

frame <- data.frame(date=rep(seq(min(uk$date), as.Date("2021/12/30"), by = "day"), each = max(uk$projects))) %>%
  group_by(date) %>%
  mutate(
    projects = c(1:25)
  )



uk_merge <- merge(uk_proj, frame, by="projects", all.y = T) %>%
  group_by(projects) %>%
  do(modelr::add_predictions(., first(.$proj))) %>%
  distinct(date, .keep_all = T) %>%
  select(projects,pr_date = date, pred)
  

tot_uk <- merge(uk, uk_merge, by="projects") %>%
  rename(dose = uk_second_dose) %>%
  select(!uk_first_dose) %>%
  group_by(projects) %>%
  mutate(
    pred = ifelse(pr_date < min(date), NA, pred),
    pr_date = ifelse(pr_date < min(date), NA, pr_date),
    pr_date = as.Date(pr_date, origin = "1970-01-01")
  ) %>%
  drop_na(pr_date) %>% ungroup %>%
  mutate(
    cur_p = ifelse(projects == 25, TRUE, FALSE),
    final_p = ifelse(date == max(date), dose, NA),
    o_age = ifelse(cur_p == TRUE & pr_date %in% c(as.Date("2021-10-15"), as.Date("2021-11-30")), pred, NA)
  ) %>%
  subset(projects > 11)

ggplot(tot_uk) +
  geom_line(aes(x = date, y = dose), colour="#E33A11", size=1.1) +
  geom_line(aes(x = pr_date, y = pred, group = projects, colour=cur_p, size=cur_p, alpha=cur_p), na.rm = T, linetype="dashed") +
  geom_point(aes(x = date, y=final_p), colour="#E33A11", size=2, na.rm = T) +
  geom_point(aes(x = pr_date, y =o_age), colour="#E33A11", size=2, na.rm = T) + 
  scale_color_manual(values = c("#858780", "#E33A11"), guide="none") +
  scale_size_manual(values = c(0.5, 1.1), guide="none") +
  scale_alpha_manual(values = c(0.5, 1), guide="none") +
  scale_y_continuous(limits = c(0, 1), labels=scales::percent) +
  scale_x_date(expand = expansion(mult = c(0, .1))) +
  annotate("text", x = as.Date("2021-10-15"), y = 0.5, label="Equivalent to everyone above the age of \n 19 fully vaccinated", na.rm = T,family="Bitter", size=rel(2.5)) +
  annotate("text", x = as.Date("01/09/2021", format="%d/%m/%Y"), y = 0.2, label="Dashed lines are linear projections based on the preceding week's data. \n Grey lines imply previous projections. ", family="Bitter", size=rel(2.5)) +
  annotate("segment", x = as.Date("2021-10-15"), xend = as.Date("2021-10-15"), y = 0.55,yend =  0.7644311, na.rm = T, size=rel(0.5), colour = "#898c89") +
  annotate("text", x = as.Date("2021-11-30"), y = 0.7, label="Equivalent to everyone above the age of \n 14 fully vaccinated. \n (Only 18+ currently eligible for vaccine).", na.rm = T,family="Bitter", size=rel(2.5)) +
  annotate("segment", x = as.Date("2021-11-30"), xend = as.Date("2021-11-30"), y = 0.75,yend =  0.8811385, na.rm = T, size=rel(0.5), colour = "#898c89") +
  theme_inews_basic() +
  labs(title = "Second dose coverage (actual vs projected at current rate)", subtitle="Second dose coverage for the UK (for entire population)", caption = "source: Covid-19 dashboard")
  
save_inews("tot.png", width_i = 20)

######## Most popular day


