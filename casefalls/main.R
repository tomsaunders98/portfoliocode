library(InewsTheme)
library(tidyverse)
library(sf)
library(jsonlite)

### Vaccines are leading to fall in cases ###



## Vaccines split by age and gender

age <- read.csv("vacbyage.csv", na.strings = "") %>%
  pivot_longer (!c(type, gender), names_to="age", values_to="coverage") %>%
  drop_na()

ex_age <- function(x){
  #Particular cases
  if (x == "X80."){
    nstring = "80+"
  }
  else if (x == "Under.18"){
    nstring = "0-18"
  }
  else{
    nstring <- gsub(".", "-", x, fixed=TRUE)
    nstring <- gsub("X", "",nstring)
  }
  return(nstring)
}

age$age <- sapply(age$age, ex_age)

age$age <- factor(age$age, levels=c("0-18","18-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))


age_ons <- read.csv("age_ons.csv")
age <- merge(age, age_ons, by=c("age", "gender", "coverage", "type"), all=TRUE)

write.csv(age, "vac_split.csv", row.names = F)

#Take max to bind to limits
lims <- max(age$coverage)

age <- age %>%
  pivot_wider(names_from="type", values_from="coverage") %>%
  group_by(gender, age) %>%
  mutate(
    ons = ons - dose1,
    ons = ifelse(ons < 0, 0, ons),
    dose1 = dose1-dose2
  ) %>% ungroup %>%
  pivot_longer(c(dose1, dose2, ons), names_to = "type", values_to="coverage" ) %>%
  mutate(
    coverage = ifelse(gender == "m", -coverage, coverage),
  ) %>%
  unite(type_a, c(gender, type), remove=FALSE)


abs_l <- function(x){
  return(scales::comma(abs(x)))
}

age$type_a = factor(age$type_a, levels=c("f_dose2","m_dose2", "f_dose1","m_dose1", "f_ons","m_ons"))

age <- age %>%
  arrange(type_a)

ggplot(age[order(age$type_a, decreasing = T),], aes(x = as.factor(age), fill = type_a,
                                                    y = coverage)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels = abs_l, limits = c(-1,1)*lims, breaks = c(-5000000,-2500000, -1000000, 0, 1000000, 2500000, 5000000)) +
  coord_flip() +
  theme_inews_basic() + 
  scale_fill_manual(labels =c("Female (2 doses)", "Male (2 doses)", "Female (1 dose)", "Male (1 dose)", "Female (No vaccine)", "Male (No vaccine)"), values=c("#7B4C99","#777CE6",  "#E664D7", "#8FA6D6", "#85D69C", "#85D69C")) + #"#efb6e8", "#8fd5d6"
  labs(title="Vaccine coverage (as of 22nd July) in England", subtitle = "Vaccines given, by age and gender", caption = "source: PHE Weekly statistics") + 
  theme(
    plot.caption = element_text(size = rel(0.5))
  )

save_inews("vac_cov.png")

##Vaccines with linear projections


uk <- read.csv("vacdata.csv", na.strings = "") %>%
  mutate(
    date = as.Date(date, "%d/%m/%Y")
  ) %>%
  select(
    date,
    uk_first_dose = cumPeopleVaccinatedFirstDoseByVaccinationDate,
    uk_second_dose = cumPeopleVaccinatedSecondDoseByVaccinationDate
  ) %>%
  mutate(
    uk_first_dose = uk_first_dose/56550138,
    uk_second_dose = uk_second_dose/56550138
  ) %>%
  drop_na()





uk$projects <- cut_interval(uk$date, 30, labels=F)
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
    projects = c(1:30)
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
    cur_p = ifelse(projects == 30, TRUE, FALSE),
    final_p = ifelse(date == max(date), dose, NA),
    o_age = ifelse(cur_p == TRUE & pr_date %in% c(as.Date("2021-11-16")), pred, NA)
  ) %>%
  subset(projects > 13)

ggplot(tot_uk) +
  geom_line(aes(x = date, y = dose), colour="#E33A11", size=1.1) +
  geom_line(aes(x = pr_date, y = pred, group = projects, colour=cur_p, size=cur_p, alpha=cur_p), na.rm = T, linetype="dashed") +
  geom_point(aes(x = date, y=final_p), colour="#E33A11", size=2, na.rm = T) +
  geom_point(aes(x = pr_date, y =o_age), colour="#E33A11", size=2, na.rm = T) + 
  scale_color_manual(values = c("#858780", "#E33A11"), guide="none") +
  scale_size_manual(values = c(0.5, 1.1), guide="none") +
  scale_alpha_manual(values = c(0.5, 1), guide="none") +
  scale_y_continuous(limits = c(0, 1), labels=scales::percent) +
  scale_x_date(expand = expansion(mult = c(0, .1)), date_breaks = "1 month", date_labels = "%b") +
  theme_inews_basic() +
  labs(title = "Second dose coverage in England", subtitle = "Actual rate vs. projected rate", caption = "source: Covid-19 dashboard")

save_inews("tot.png")


## World Cup ##

# Active plot
refname <- function(x){
  x = gsub("District", "", x)
  x = gsub("London Borough of", "", x)
  x = gsub("Metropolitan Borough of", "", x)
  x = gsub("Royal Borough of", "", x)
  x = gsub("Borough of", "", x)
  x = gsub("Borough", "", x)
  x = gsub("City of", "", x)
  x = gsub(",", "", x)
  x = gsub("County Borough", "", x)
  x = gsub("Council", "", x)
  x = gsub("Principle Area", "", x)
  x = gsub("Principal Area", "", x)
  x = gsub("St.", "st", x)
  x = gsub("Saint", "st", x)
  x = gsub("&", "and", x)
  x = gsub("City", "", x)
  x = gsub("County of", "", x)
  x = gsub("County", "", x)
  x = tolower(x)
  x = trimws(x)
  x = stringr::str_replace_all(x, pattern=" ", repl="")
  ## Individual Cases
  x = gsub("rhonddacynontaff", "rhonddacynontaf", x)
  x = gsub("orkneyislands", "orkney", x)
  x = gsub("nah-eileanananiar", "nah-eileanansiar", x)
  
  ## Updating (Hacky)
  x = gsub("suffolkcoastal", "eastsuffolk", x)
  x = gsub("waveney", "eastsuffolk", x)
  x = gsub("westsomerset", "somersetwestandtaunton", x)
  x = gsub("tauntondeane", "somersetwestandtaunton", x)
  return(x)
}


mob <- read.csv("mobR.csv", na.strings = "") %>%
  mutate(date = as.Date(date),
         sub_region_2 == ifelse(is.na(sub_region_2), sub_region_1, sub_region_2)
  ) %>%
  select(
    date,
    location = sub_region_2,
    rec = retail_and_recreation_percent_change_from_baseline,
    area = sub_region_1,
    code = place_id
  ) %>% group_by(location) %>%
  mutate(
    rec_avg = ra(rec),
    rec_inc = (rec - lag(rec)) / abs(lag(rec))
  )

mob$location = sapply(mob$location, refname)


final <- mob %>%
  subset(
    date > as.Date("2021-07-05")
  )

activeplot <- final %>%
  group_by(code) %>%
  mutate(
    focus = ifelse(date == as.Date("2021-07-11") & lag(rec) < 0 & rec > 0 & lead(rec) < 0, "Focus", "No Focus"),
    full_focus = ifelse("Focus" %in% focus, "Focus", "No Focus"),
    alpha = ifelse("Focus" %in% focus, 1, 0.5)
  ) %>%
  ungroup %>%
  mutate(
    rec = rec/100
  )

activeplot %>%
  distinct(code, .keep_all = T) %>%
  subset(full_focus == "Focus")
  

ggplot(activeplot, aes(x=date, y=rec, group =code, colour=full_focus, alpha=alpha)) +
  geom_line() +
  theme(
    legend.position = "none"
  ) +
  theme_inews_basic() +
  scale_colour_manual(values=c("#E33A11","#c4c4c4"), labels=c("Above pre-pandemic levels during Euro Final", "No significant change during Euro Final")) +
  scale_alpha(guide="none") +
  labs(title = "Activity during the Euro final", subtitle = "0% implies pre-pandemic levels of activty", caption="source: Google Mobility Reports") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(expand = expansion(mult = c(0,0))) +
  annotate("segment", x = min(activeplot$date), xend = max(activeplot$date), y = 0, yend = 0, linetype = "dashed") +
  guides(colour=guide_legend(nrow=2))

save_inews("Activity.png")

#Scotland v UK 
cases <- read.csv("ncase_region.csv") %>%
  select(
    name = areaName,
    case_rate = newCasesByPublishDateRollingRate,
    date
  ) %>%
  mutate(
    date = as.Date(date, format="%d/%m/%Y")
  )

cases <- cases %>%
  group_by(name) %>%
  arrange(date) %>%
  mutate(
    inc = ifelse(case_rate - lag(case_rate) > 0, 1, 0),
    roll_sum = roll_sum(inc, 5, align = "right", fill = NA),
    breaks = ifelse(roll_sum == 0 & lead(inc) == 1, "BREAK", NA),
    case_l = log(case_rate)
  ) %>%
  subset(date > as.Date("2021-4-27")) %>%
  mutate(
    name = ifelse(name == "England" & date < as.Date("2021-05-03"), NA, name),
    name = ifelse(name == "Wales" & date < as.Date("2021-04-27"), NA, name),
    name = ifelse(name == "Scotland" & date < as.Date("2021-05-01"), NA, name),
    name = ifelse(name == "Northern Ireland" & date < as.Date("2021-05-27"), NA, name),
    days = row_number(name)
  ) %>%
  drop_na(name)

ggplot(cases, aes(x=date, y = case_rate, group=name, colour=name)) +
  geom_line() +
  theme_inews_basic() +
  labs(title = "Covid-19 Cases by Region", subtitle = "Days since the start of current wave*, cases as 7-day rolling rate", caption="source: Covid-19 Dashboard \n The start of wave is defined as an increase in cases after 5 days of falling cases") +
  scale_colour_inews()

save_inews("Scotlandvs.png")

#### Tests in schools ###


## Positive LFD in schools


tschool <- read.csv("test_schools.csv", na.strings = c("", "*")) %>%
  pivot_longer(!c(tests, school), names_to="date", values_to="tv")

tschool$date = sapply(tschool$date, function(x) stringr::str_extract(x, "(?<=\\.\\.\\.).*"))
tschool <- tschool %>%
  mutate(
    date = as.Date(date, format = "%d.%m.%y")
  )

ggplot(tschool, aes(x=date, y = tv, group = school, colour = school)) +
  geom_line() +
  theme_inews_basic() +
  labs(title = "Number of positive LFD tests in schools", caption="Source: PHE Weekly statistics")

save_inews("tschoo.png")

### School age as % of positive tests

cba <- read.csv("casebyage.csv") %>%
  select(date, age, cases) %>%
  subset(
    !(age %in% c("00_59", "60+"))
  ) %>%
  pivot_wider(names_from = age, values_from = cases) 

colnames(cba) <- paste("A", colnames(cba), sep = "_")

cba <- cba %>%
  mutate(
    school_age = A_00_04 + A_05_09 + A_10_14 + A_15_19,
    rest = (select(., !A_date) %>% rowSums(na.rm=T)) - school_age,
    total = school_age + rest,
    school_age = school_age/total,
    rest = rest/total,
    A_date = as.Date(A_date, format="%d/%m/%Y")
  ) %>%
  select(
    date = A_date,
    school_age,
    rest
  ) %>%
  pivot_longer(c(school_age, rest), names_to="type", values_to="cases") %>%
  mutate(
    type = ifelse(type == "school_age", "Ages 0 - 19", "Ages 20+")
  )

ggplot(cba, aes(x=date, y = cases, group = type, fill = type)) +
  geom_area(position = position_stack(reverse = T)) +
  theme_inews_basic() +
  scale_fill_inews() +
  scale_x_date(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  labs(title = "% of Covid cases in those of school age", caption="source: Covid-19 dashboard")

save_inews("agesplit.png")

## Unrelated Hugo Stuff

vc <- read.csv("vc.csv") %>%
  pivot_longer(!type, names_to="date", values_to="vals")

conv <- function(x){
  x <- gsub("\\.", "-", x)
  x <- gsub("X","", x)
  return(x)
}
vc$date <- sapply(vc$date, conv)

ggplot(vc, aes(y=vals, x=date, group=type, fill=type)) +
  geom_col(position = position_dodge()) +
  theme_inews_basic() +
  scale_fill_inews() +
  labs(title = "Crime rates in England and Wales", subtitle = "Crime rates per 1,000 people")

save_inews("rates.png")

## Weather
# 
# m_2021 <- read.csv("cet_mean_2021.csv", na.strings = "-") %>%
#   pivot_longer(!day, names_to="month", values_to="temp") %>%
#   drop_na()
# 
# 
# m_2021$month = sapply(m_2021$month, function (x) paste(x, "/2021", sep = "")) 
# 
# m_2021 <- m_2021 %>%
#   unite(date, c(day, month), sep="/", remove=F) %>%
#   mutate(
#     date = as.Date(date, format="%d/%b/%Y"),
#     temp = temp/10
#   ) %>%
#   select(date, temp)

giv_temp <- read.csv("giv_temp.csv") %>%
  mutate(
    date = as.Date(date, format="%d/%m/%Y")
  )

ncases <- read.csv("ncases.csv") %>%
  mutate(
    date = as.Date(date, format="%d/%m/%Y")
  ) %>%
  select(date, cases = newCasesBySpecimenDate)



t_temp <- merge(giv_temp, ncases, by="date") %>%
  arrange(date) %>%
  mutate(
    temp = ra(temp),
    cases = ra(cases)
  ) %>%
  drop_na() %>%
  mutate(
    p_temp = (temp - lag(temp))/lag(temp),
    p_cases = (cases - lag(cases))/lag(cases)
  ) %>%
  drop_na() %>%
  mutate(
    l_temp = lag(p_temp, 5)
  ) %>%
  pivot_longer(c(p_cases, l_temp), names_to="type", values_to="perc") %>%
  subset(date > as.Date("2021-03-01")) %>%
  mutate(
    type = ifelse(type == "p_cases", "% increase in average cases (5 days later)", "% increase in average temperature")
  )

ggplot(t_temp, aes(x=date, y = perc, group=type, colour=type)) +
  geom_line() +
  theme_inews_basic() +
  scale_y_continuous(labels=scales::percent_format(accuracy = 5L)) +
  scale_colour_inews() +
  labs(title="Cases as daily temperature rises", caption = "source: Nasa Giovanni, Covid-19 dashboard") +
  guides(colour = guide_legend(nrow = 2))
  
save_inews("t_temp.png")

bands <- merge(giv_temp, ncases, by="date")
bands <- bands %>%
  arrange(date) %>%
  mutate(
    cases = ra(cases)
  ) %>%
  arrange(date) %>%
  mutate(
    p_inc = (cases-lag(cases))/lag(cases),
    l_case = lag(p_inc, 4),
    l_case_s = lag(cases, 4)
  ) %>%
  subset(date > as.Date("2021-04-01")) %>%
  mutate(
    min = min(l_case, na.rm =T),
    max = max(l_case, na.rm = T)
  )
# midpoint = mid, low = "#08519c",
# mid="#fee0d2",
# high = "#ef3b2c"
#"#650912", "#a41d21",
mid = mean(bands$temp)
colours = rev(c( "#c92026", "#ed3c2f", "#f2694c", "#f69173", "#fabaa1", "#fde0d0",
            "#dfedf9", "#c7ddf1", "#9dcbe3", "#68aed8", "#3d92c7", "#1572b7"))
#"#0a519d", "#203466"
#CCFC62 #238b45
ggplot(bands) +
  geom_rect(aes(fill = temp, xmin=date, xmax=date+1, ymin=min, ymax = max)) +
  geom_line(aes(y = l_case, x = date), colour="#238b45") +
  scale_fill_gradientn(colours = colours) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme_inews_map() +
  theme(
    plot.title = element_text(size = rel(1.4), colour = "#000000", family = "Bitter", face = "bold",  hjust = 0.5, margin=margin(t=5, b=10)),
    axis.ticks = element_line(size = rel(0.7), colour = "#878787"),
    axis.text= element_text(size = rel(1), margin=margin(0,0,0,b=5, unit="pt"))
  ) +
  coord_cartesian(expand = F) +
  labs(title = "Daily temperature vs % increase in cases 4 days later", caption="source: NASA Giovanni, Covid-19 dashboard")

save_inews("climateband.png", type="other")
mob_g <- read.csv("2021_GB_Region_Mobility_Report.csv", na.strings = "") %>%
  subset(country_region == "United Kingdom" & is.na(sub_region_1)) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  select(date, park = parks_percent_change_from_baseline)

park <- merge(giv_temp, mob_g, by="date") %>%
  arrange(date) %>%
  mutate(
    park = ra(park),
    temp = ra(temp)
  ) %>%
  drop_na() %>%
  mutate(
    p_temp = (temp - lag(temp))/lag(temp),
    p_park = (park - lag(park))/lag(park)
  ) %>%
  pivot_longer(c(p_temp, p_park), names_to="type", values_to="perc") %>%
  subset(date > as.Date("2021-04-01")) %>%
  mutate(
    type = ifelse(type == "p_park", "% increase in park activity", "% increase in temperature")
  )

ggplot(park, aes(x=date, y= perc, group = type, colour=type)) +
  geom_line() +
  theme_inews_basic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  labs(title="Park visits as temperature increases", caption="source: NASA Giovanni, Google Mobility Report")

save_inews("park.png")
ncaseslta <- read.csv("ncaseslta.csv") %>%
  mutate(
    date = as.Date(date, format="%d/%m/%Y"),
    week = cut.Date(date, breaks = "1 week", labels = FALSE)
  ) %>%
  group_by(week, areaName) %>%
  mutate(
    avg_case = mean(newCasesBySpecimenDate),
    date_r = paste(format(min(date), "%d-%b"), format(max(date), "%d-%b"), sep=" to ")
  ) %>%
  subset(week > 74) %>%
  select(date_r, avg_case, areaCode)
  
breaks <- unname(unlist(classInt::classIntervals(ncaseslta$avg_case, n =5, style = "pretty")["brks"]))

map <- st_read("Local_Authority_Districts_(December_2019)_Boundaries_UK_BUC.shp")

casemap <- merge(map,ncaseslta, by.y="areaCode", by.x = "lad19cd") 



casemap$date_r <- factor(casemap$date_r, levels=c("28-Jun to 04-Jul","05-Jul to 11-Jul","12-Jul to 18-Jul","19-Jul to 25-Jul"))

ggplot(casemap, aes(fill = avg_case)) +
  facet_wrap(~date_r) +
  geom_sf(size=0) +
  theme_inews_map() +
  scale_fill_fermenter(direction=1, palette="OrRd", breaks=breaks) +
  labs(title = "Weekly average cases", caption="source: Covid-19 dashboard") +
  theme(
    plot.title = element_text(size = rel(2), colour = "#000000", family = "Bitter", face = "bold",  hjust = 0.5, margin=margin(t=5, b=10)),
    text = element_text(size = rel(1.5), family = "Bitter", colour = "#898a8c"),
    plot.subtitle = element_text(size = rel(1.3), colour = "#525354", family = "Bitter", hjust = 0.5),
    plot.caption = element_text(family = "Bitter", colour = "#898a8c", size = rel(0.8), hjust = 0),
    legend.key.size = unit(3, "lines"),
    plot.margin = margin(t = 10,r = 10, b = 10,l =10, unit = "pt")
  )

save_inews("casemap.png", type="map")


## Gender Split

ages <- c("20_to_24", "25_to_29", "30_to_34")

gendersplit <- fromJSON("ncase_gender.json", flatten = TRUE)

female_case <- as.data.frame(gendersplit["data"]) %>%
  unnest(data.femaleCases) %>%
  select(!data.maleCases) %>%
  subset(age %in% ages) %>%
  mutate(
    date = as.Date(data.date)
  ) %>%
  group_by(date) %>%
  summarise(
    fem_case = sum(value)
  )

male_case <- as.data.frame(gendersplit["data"]) %>%
  unnest(data.maleCases) %>%
  select(!data.femaleCases) %>%
  subset(age %in% ages) %>%
  mutate(
    date = as.Date(data.date)
  ) %>%
  group_by(date) %>%
  summarise(
    male_case = sum(value)
  )

tcase <- merge(male_case, female_case, by="date") %>%
  pivot_longer(!date, names_to="type", values_to="cases") %>%
  subset(date > as.Date("2020-02-23")) %>%
  arrange(date) %>%
  group_by(type) %>%
  mutate(
    ncase = cases - lag(cases),
    lcase = lag(ncase, 4)
    # lcase_rib = lcase,
    # type_rib = type
  ) %>%
  subset(date > as.Date("2021-05-11")) %>%
  mutate(
    type = ifelse(type == "fem_case", "Female cases", "Male cases")
  )
  # pivot_wider(names_from="type", values_from="lcase") %>%
  # group_by(date) %>%
  #   summarise(
  #   fem_case = sum(fem_case, na.rm=T),
  #   male_case = sum(male_case, na.rm = T)
  #   )
  # 
  
  

ggplot(tcase, aes(x=date)) +
  geom_line(aes(y=lcase, group=type, colour=type)) +
  #geom_line(aes(y=fem_case), colour="#a6cee3") +
  theme_inews_basic() +
  scale_colour_inews() +
  #scale_x_date(date_breaks= "1 day") +
  #geom_ribbon(aes(ymin = fem_case, ymax=male_case), fill="#E33A11") +
  labs(title="Cases during the Euro period", subtitle="Cases 4 days later among men and women\n between the ages of 20 and 34", caption = "source: Covid-19 dashboard")
  #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

save_inews("gend_split.png")
  