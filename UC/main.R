library(InewsTheme)
library(tidyverse)
library(sf)
library(ggbeeswarm)
library(ggdist)
library(patchwork)


## caclulate % change in 2017-2019

res2019 <- read.csv("2019result.csv") %>%
  select(
    code = ons_id,
    name = constituency_name,
    country = country_name,
    res = first_party,
    pop = electorate,
    region = region_name,
    con,lab,ld,green
  )
res2017 <- read.csv("2017result.csv") %>%
  select(
    code = ons_id,
    name = constituency_name,
    country = country_name,
    res = first_party,
    pop = electorate,
    con,lab,ld,green
  )

res2019_f <- res2019 %>%
  subset(country == "England") %>%
  pivot_longer(con:green, names_to="party", values_to="votes") %>%
  mutate(
    res = tolower(res),
    perc = votes/pop
  ) %>%
  filter(res == party | res == "spk") %>%
  select(
    code,
    name,
    res_2019 = res,
    perc_2019 = perc,
    region
  )

res2017_f <- res2017 %>%
  subset(country == "England") %>%
  pivot_longer(con:green, names_to="party", values_to="votes") %>%
  mutate(
    res = tolower(res),
    perc = votes/pop
  ) %>%
  filter(
    res == party | res == "spk" ##Only need winning parties for each constituency
  ) %>%
  select(
    code,
    name,
    res_2017 = res,
    perc_2017 = perc
  )

r_wall <- merge(res2019_f, res2017_f, by=c("code", "name")) %>%
  mutate(
    type = ifelse(res_2019 == "con" & res_2017 == "lab" & region %in% c("East Midlands", "North West", "West Midlands", "North East"), "r_wall", res_2019)
  ) %>%
  select(
    code, name, res = res_2019, type,region
  ) %>%
  mutate(
    l_name = tolower(name)
  )






## Merge UC eligible population

pop_uc <- read.csv("const_pop_by_age.csv") %>%
  select(!All.Ages) %>%
  pivot_longer(!c("PCON11CD", "PCON11NM"), names_to="ages", values_to="pop") %>%
  mutate(
    ages = as.numeric(gsub("X", "", ages))
  ) %>%
  filter(
    ages > 18 & ages < 66
  ) %>%
  group_by(PCON11CD) %>%
  summarise(
    pop = sum(pop, na.rm = T)
  ) %>%
  rename(
    code = PCON11CD
  )

r_wall_pop <-merge(r_wall, pop_uc, by="code")

## merge with UC



UC <- read.csv("uc.csv") %>%
  mutate(
    l_name= tolower(name)
  ) %>%
  select(
    l_name,
    claimants = total,
    n_employ, 
    employ
  )
r_wall_uc <- merge(r_wall_pop, UC, by="l_name") %>%
  mutate(
    p_claimants = claimants/pop,
    p_employ = employ/pop,
    p_n_employ = n_employ/pop
  ) %>%
  select(
    code,
    name,
    p_claimants,
    p_employ,
    p_n_employ,
    type,
    region
  ) 
setdiff(UC$l_name,r_wall_pop$l_name)

wmin <- st_read("const//Westminster_Parliamentary_Constituencies_(December_2020)_UK_BGC.shp")


wmin_uc <- merge(wmin, r_wall_uc, by.x="PCON20CD", by.y="code")
setdiff(r_wall_uc$code, wmin$PCON20CD)

brks <- unname(unlist(classInt::classIntervals(wmin_uc$p_claimants, 5, style="fisher")["brks"]))
brks <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)

ggplot(wmin_uc, aes(fill = p_claimants)) +
  geom_sf(size=0) +
  scale_fill_fermenter(palette = "RdYlBu", breaks=brks, labels=scales::label_percent(accuracy = 1)) +
  theme_inews_map() +
  labs(title = "% of UC claimants by constituency", caption = "source: ONS/DWP")

save_inews("wmin_uc.eps", type="map", device = "eps")


### Beeswarm

r_swam <- r_wall_uc %>%
  mutate(
    type_x = ifelse(type %in% c("r_wall", "con"), "Conservative", 
                    ifelse(type == "lab", "Labour", 
                           ifelse(type == "ld", "Liberal Democrats", type)))
  ) %>%
  filter(
    type_x %in% c("Conservative", "Labour", "Liberal Democrats")
  ) %>%
  mutate(
    type = ifelse(type == "con", "Conservative",
                  ifelse(type == "lab", "Labour",
                         ifelse(type=="ld", "Liberal Democrats","'Red Wall' Conservative seats")))
  )

ggplot(r_swam, aes(x=type_x, y=p_claimants, colour=type)) +
  geom_beeswarm() +
  coord_flip() +
  theme_inews_basic() +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("#a422e5", "#0597F2", "#BF1717", "#D96704")) +
  labs(title = "% of Universal Credit claimants by party", caption = "source: DWP/ONS")

save_inews("r_swam.eps", device="eps")



claimants <- read.csv("claimants.csv") %>%
  mutate(
    date = as.Date(paste0("01 ", tolower(date)), format = "%d %Y %b")
  )

ggplot(claimants, aes(x=date, y=num)) +
  geom_line(colour="#E33A11") +
  theme_inews_basic() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Number of people claiming benefits due to unemployment", subtitle = "(Thousand people)", caption = "source: ONS")

save_inews("claimants.eps", device="eps")

##analysis

r_focus <- r_wall_uc %>%
  filter(type %in% c("r_wall", "con")) %>%
  arrange(desc(p_claimants))


ggplot(r_swam, aes(x=type_x, y=p_claimants, fill=type, colour=type, group=type)) +
  geom_dots(aes(group = NA), thickness=.1) +
  scale_fill_manual(values = c("#a422e5", "#0597F2", "#BF1717", "#D96704")) +
  scale_colour_manual(values = c("#a422e5", "#0597F2", "#BF1717", "#D96704")) +
  stat_pointinterval(position = position_dodge(width = .2, preserve = "single"), show.legend = F, stroke=0.1) +
  coord_flip() +
  theme_inews_basic() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "% of Universal Credit claimants by party", 
    caption = "source: DWP/ONS")

save_inews("r_swam_dots.eps", device="eps")


taperrate <- read.csv("taperrate.csv") %>%
  mutate(
    pay_base = ifelse(pay > 241, 8.91- 8.91*(0.12+0.2), 
                      ifelse(pay > 184 & pay < 241, 8.91- 8.91*0.12, 8.91)),
    pay_uc = ifelse(pay > 241 , 8.91-8.91*(0.63 + 0.12+0.2), 
                    ifelse(pay > 184 & pay < 241, 8.91-8.91*(0.63+0.12), 8.91-8.91*0.63)),
    r_pay = cumsum(pay_base),
    r_uc = cumsum(pay_uc),
    pay_w_uc = r_uc + 411.51/4,
    pay_n_uc = r_uc + 324.84/4,
    pay_w_uc =  pay_w_uc -pay_n_uc,
    pay_n_uc = pay_n_uc - r_uc
  ) %>%
  select(pay_w_uc, pay_n_uc, r_uc, h_worked) %>%
  pivot_longer(!h_worked, names_to="type", values_to="pay")

tpaerline <- read.csv("taperrate.csv") %>%
  mutate(
    pay_base = ifelse(pay > 241, 8.91- 8.91*(0.12+0.2), 
                      ifelse(pay > 184 & pay < 241, 8.91- 8.91*0.12, 8.91)),
    pay_uc = ifelse(pay > 241 , 8.91-8.91*(0.63 + 0.12+0.2), 
                    ifelse(pay > 184 & pay < 241, 8.91-8.91*(0.63+0.12), 8.91-8.91*0.63)),
    r_pay = cumsum(pay_base),
    r_uc = cumsum(pay_uc),
    pay_w_uc = r_uc + 411.51/4,
    pay_n_uc = r_uc + 324.84/4
  ) %>%
  select(pay_w_uc, pay_n_uc, r_uc, h_worked) %>%
  pivot_longer(!h_worked, names_to="type", values_to="pay")

brks <- cumsum(rep(20, 10))

taperrate$type <- factor(taperrate$type, levels = c("pay_w_uc", "pay_n_uc", "r_uc"))
tpaerline$type <- factor(tpaerline$type, levels = c("pay_w_uc", "pay_n_uc", "r_uc"))

ggplot(taperrate, aes(x=h_worked, y=pay, fill=type, group = type)) +
  geom_area(alpha=0.4, outline.type = "upper") +
  geom_line(data=tpaerline, aes(x=h_worked, y=pay, colour=type, group = type), size=1) +
  theme_inews_basic() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#F25A38", "#5F8C32", "#20638C")) +
  scale_fill_manual(values = c("#F25A38", "#5F8C32", "#20638C")) +
  scale_y_continuous(breaks = brks, labels = scales::label_dollar(prefix = "£")) +
  coord_cartesian(expand = FALSE)

save_inews("taperrate.eps", device="eps")

r_wall_only <- r_focus %>%
  filter(type == "r_wall") %>%
  arrange(desc(p_claimants)) %>%
  head(10)

ggplot(r_wall_only, aes(x=reorder(name,p_claimants), y=p_claimants, fill=region)) +
  geom_col() +
  coord_flip() +
  scale_fill_inews() +
  theme_inews_basic() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  labs(caption = "source: DWP")

save_inews("top_ten.eps", device = "eps")

write.csv(r_wall_only, "R_wall_top_ten.csv", row.names = F)
