library(tidyverse)
library(InewsTheme)
library(sf)
library(gganimate)


alltax_wm <- read.csv("alltax.csv", na.strings = "") %>%
  select(Local.authority:Band.H) %>%
  drop_na() %>%
  mutate_at(vars(-Local.authority), as.numeric) %>%
  mutate(
    Band.A = (Band.A-552.85)/552.85,
    Band.B = (Band.B -644.99)/644.99,
    Band.C = (Band.C - 737.13)/737.13,
    Band.D = (Band.D - 829.27)/829.27,
    Band.E = (Band.E - 1013.55)/1013.55,
    Band.F = (Band.F - 1197.83)/1197.83,
    Band.G = (Band.G - 1382.12)/1382.12,
    Band.H = (Band.H - 1658.54)/1658.54,
    Local.authority = ifelse(Local.authority=="St Helens", "St. Helens", Local.authority)
  ) %>%
  select(
    tax_wm = Band.D,
    Local.authority
  )
  # pivot_longer(-Local.authority, names_to="band", values_to="tax") %>%
  # group_by(Local.authority) %>%
  # summarise(
  #   tax_wm = mean(tax),
  # ) %>%
  # drop_na(tax_wm)



alltax <- read.csv("alltax.csv", na.strings = "") %>%
  select(Local.authority:Band.H) %>%
  drop_na() %>%
  mutate_at(vars(-Local.authority), as.numeric) %>%
  mutate(
    Local.authority = ifelse(Local.authority=="St Helens", "St. Helens", Local.authority)
  ) %>%
  select(
    bands = Band.D,
    Local.authority
  )
  # pivot_longer(-Local.authority, names_to="band", values_to="tax") %>%
  # group_by(Local.authority) %>%
  # summarise(
  #   bands = mean(tax),
  # ) %>%
  # drop_na(bands)

alltax <- merge(alltax_wm, alltax, by="Local.authority")

alltax_bot <- read.csv("alltax.csv", na.strings = "") %>%
  select(Local.authority:Band.H) %>%
  drop_na() %>%
  mutate_at(vars(-Local.authority), as.numeric) %>%
  mutate(
    Local.authority = ifelse(Local.authority=="St Helens", "St. Helens", Local.authority)
  ) %>%
  select(Local.authority, Band.A, Band.B, Band.C, Band.D) %>%
  pivot_longer(-Local.authority, names_to="band", values_to="tax") %>%
  group_by(Local.authority) %>%
  summarise(
    bands_bot = mean(tax),
  ) %>%
  drop_na(bands_bot)

alltax <- merge(alltax_bot, alltax, by="Local.authority")


lad2021 <- st_read("lad2021//Local_Authority_Districts_(May_2021)_UK_BGC.shp")

# st_geometry(lad2021) <- NULL
# 
# lad2021 <- lad2021 %>%
#   select(name = LAD21NM,
#          code = LAD21CD,
#          LONG,
#          LAT)

refname <- function(x){
  x = gsub(", County of", "", x)
  x = gsub("UA", "", x)
  x = gsub("&", "and", x)
  x = gsub("-", " ", x)
  x = gsub("Council", "", x)
  x = gsub("County", "", x)
  x = gsub(", City of", "", x)
  x = trimws(x)
  return(x)
}

alltax$Local.authority = sapply(alltax$Local.authority, refname)
lad2021$LAD21NM = sapply(lad2021$LAD21NM, refname)


map <- merge(lad2021, alltax, by.x="LAD21NM", by.y="Local.authority")
setdiff(alltax$Local.authority, lad2021$LAD21NM)

hp <- read.csv("MedianHousePrices.csv")

## Update to latest codes (2021)

changes <- read.csv("Changes.csv") %>%
  subset(
    YEAR > 2018
  ) %>%
  select(GEOGCD, GEOGCD_P, YEAR)

hp <- merge(hp, changes, by.x="code", by.y="GEOGCD_P", all.x = T) %>%
  mutate(
    code = ifelse(!is.na(GEOGCD), GEOGCD, code)
  ) %>%
  group_by(code) %>%
  summarise(
    hp = mean(hp)
  ) %>%
  drop_na(hp)


map <- merge(map, hp, by.x = "LAD21CD", by.y="code")
setdiff(hp$code, map$code)

changes <- read.csv("Changes.csv") %>%
  subset(
    YEAR > 2017
  ) %>%
  select(GEOGCD, GEOGCD_P, YEAR)

## Same for Index of Multiple deprivation (but bind to income first to save time)

IMD <- read.csv("IMD_LA.csv") %>%
  select(
    code,
    name,
    imd = IMD...Average.rank
  )

IMD <- merge(IMD, changes, by.x="code", by.y="GEOGCD_P", all.x = T) %>%
  mutate(
    code = ifelse(!is.na(GEOGCD), GEOGCD, code)
  ) %>%
  group_by(code) %>%
  summarise(
    imd = mean(imd)
  ) %>%
  drop_na(imd)

changes <- read.csv("Changes.csv") %>%
  subset(
    YEAR > 2011
  ) %>%
  select(GEOGCD, GEOGCD_P, YEAR)

income <- read.csv("income_real.csv") %>%
  group_by(code) %>%
  summarise(
    income = mean(income)
  )



income <- merge(income, changes, by.x="code", by.y="GEOGCD_P", all.x = T) %>%
  mutate(
    code = ifelse(!is.na(GEOGCD), GEOGCD, code)
  ) %>%
  group_by(code) %>%
  summarise(
    income = mean(income)
  ) %>%
  drop_na(income)

  
IMD_inc <- merge(IMD, income, by=c("code"))
setdiff(IMD$code, income$code)


## Add changes





## Update




map <- merge(map, IMD_inc, by.x = "LAD21CD", by.y="code")
setdiff(IMD_inc$code, map$code)

write.csv(map, "mapdata_nocomp.csv", row.names = F)

map_d <- map %>%
  mutate(
    hp_p = (bands/hp),
    income_p = (bands/income)*100,
    tax_wm = tax_wm*100
  ) #%>%
  # select(
  #   code,
  #    name,
  #    LONG,
  #    LAT,
  #    bands_bot,
  #    bands,
  #    tax_wm,
  #    hp_p,
  #    income_p
  #  )
### Map for print

median(map_d$hp_p)

ggplot(map_d, aes(fill = hp_p)) +
  geom_sf(size=0, colour = NA) +
  theme_inews_map() +
  #binned_scale("fill", "fermenter", ggplot2:::binned_pal(c("#2166ac", "#7eb5d6", "#e1eced", "#fbe6d4", "#f39a76")), breaks=c(0.5, 0.6, 0.8, 0.9, 1.2))
  scale_fill_fermenter(palette = "RdBu", breaks=c(0.005, 0.006, 0.007, 0.009, 0.012), labels = scales::percent_format(accuracy = 0.1)) +
  #scale_fill_gradient2(low="#2267ad", mid = "#eef2eb", high="#b31a2c", midpoint=0.01001253, labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "The inequality of Council Tax", subtitle = "Average Council Tax compared to average house price", caption="Average council tax calculated as average across Band D\nsource: Ministry of Housing, Communities & Local Government")

#save_inews("fimage.png")

ggsave("plot.eps", dpi=300, device = cairo_ps, width = 25, height = 25, units = "cm")



st_geometry(map_d) <- NULL


ibox <- map_d %>%
  arrange(hp_p) %>%
  tail(5)


breaks <- unname(unlist(classInt::classIntervals(map$imd, 5, style="fisher")["brks"]))

map$imd_r <- as.factor(cut(map$imd, breaks, labels = F, include.lowest = T))


write.csv(map_d, "dwrapdata.csv", row.names = F)

map_p <- map %>%
  mutate(
    z_score = abs(bands - mean(bands))/sd(bands)
  ) %>%
  subset(z_score < 3)

ggplot(map, aes(x=hp, y = bands)) +
  geom_point(aes(group=imd_r, colour=imd_r), alpha=0.5) +
  geom_line(stat="smooth", method = "lm", colour="#cab2d6") +
  theme_inews_basic() +
  scale_colour_inews() +
  scale_x_continuous(labels=scales::comma) +
  labs(title = "Council tax compared to average house price", subtitle="As house prices increase, council tax falls", caption="Council tax calculated as average over Band D\nsource: Ministry of Housing, Communities & Local Government")
  
save_inews("ctax.png")

ggplot(map, aes(x=income, y = bands)) +
  geom_point(aes(group=imd_r, colour=imd_r), alpha=0.5) +
  geom_line(stat="smooth", method = "lm", colour="#cab2d6") +
  theme_inews_basic() +
  scale_colour_inews() +
  scale_x_continuous(labels=scales::comma) +
  labs(title = "Council tax compared to average income", caption="Council tax calculated as average over Band D\nsource: Ministry of Housing, Communities & Local Government")

save_inews("ctax_inc.png")


## Time series

ts <- read.csv("areact_timeseries.csv", na.strings = c("", "-", " - ")) %>%
  select(code:X2021.22) %>%
  pivot_longer(!c(code, ecode, location, current, class, reg), names_to="date", values_to="cost")

ts$date <- sapply(ts$date, function(x) as.Date(paste0(stringr::str_extract(x, "(?<=X).+(?=\\.)"), "-01-01")))

higlight = c("Hammersmith & Fulham", "Hartlepool UA")

ts <- ts %>%
  drop_na(location) %>%
  mutate(
    date = as.Date(unname(date), origin="1970-01-01"),
    alpha = ifelse(location %in% higlight, 1, 0.4),
    reg = ifelse(reg %in% c("SW", "SE", "L", "EE"), "South of England", "North of England"),
    # colour = ifelse(date == as.Date("2014-01-01") & location == "Hammersmith & Fulham", "Hammersmith & Fullham", reg),
    # colour = ifelse(date >= as.Date("2015-01-01") & location == "Hartlepool UA", "Hartlepool", 
    #                 ifelse(location == "Hammersmith & Fulham", "Hammersmith & Fullham", reg)),
    # colour = ifelse(date < as.Date("2014-01-01"), reg, colour),
    # alpha = ifelse(date == as.Date("2014-01-01") & location == "Hammersmith & Fulham", 1, 0.4),
    # alpha = ifelse(date >= as.Date("2015-01-01") & location == "Hartlepool UA", 1, 
    #                 ifelse(location == "Hammersmith & Fulham", 1, 0.4)),
    # alpha = ifelse(date < as.Date("2014-01-01"), 0.6, alpha),
    points = ifelse((date == as.Date("2014-01-01") & location == "Hammersmith & Fulham") | (date == as.Date("2015-01-01") & location == "Hartlepool UA"), cost, NA),
    year = as.factor(format(date, "%Y"))
  )
#reg = ifelse(reg %in% c("Midlands", "North of England", "Yorkshire"), "North", "South")
analysis <- ts %>%
  subset(date > as.Date("2020-01-01")) %>%
  group_by(location) %>%
  summarise(
    cost = mean(cost)
  )

ts$colour <- factor(ts$colour, levels = c("South of England", "Midlands", "North of England", "Yorkshire", "Hammersmith & Fullham", "Other Local authorities", "Hartlepool"))

ggplot(ts, aes(x=date,  group = location, colour=reg, alpha=alpha)) +
  geom_line(aes(y=cost), na.rm=T) +
  geom_point(aes(y=points), show.legend = F, na.rm=T) +
  theme_inews_basic() +
  scale_colour_manual(values = c("#E33A11","#1f78b4")) +
  scale_alpha(guide = "none") +
  labs(title = "Area council tax for local authorities (£)", caption= "Council tax calculated as average over Band D\nsource: Ministry of Housing, Communities & Local Government")
  # theme(
  #   legend.text = element_text(size = rel(1.9)),
  #   plot.title = element_text(size = rel(2.2), colour = "#000000", family = "Bitter", face="bold", hjust = 0),
  #   plot.subtitle = element_text(size = rel(2.1), colour = "#525354", family="Bitter", hjust=0, margin=margin(t=5)),
  #   plot.caption = element_text(family="Bitter", colour = "#898a8c", size = rel(1.5), hjust = 0),
  #   axis.text = element_text(size = rel(1.7), margin=margin(0,0,0,0, unit="pt")),
  # )
  
save_inews("timeseriesstatic.png", expand=T)
  
#   transition_reveal(date) +
#   shadow_mark() +
#   ease_aes('cubic-in-out') 
# 
# animate(plot, fps = 25, frames=25*30, end_pause = 25, width=1920, height=1080, type="cairo")

