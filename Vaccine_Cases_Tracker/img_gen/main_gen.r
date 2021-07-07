library(tidyverse)
library(InewsTheme)
library(sf)
library(logging)
library(grid)
library(extrafont)
loadfonts(device = "win", quiet = TRUE) 


#Set up logging for R
logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file="venv/out/output.log", level='DEBUG')
with(getLogger(), names(handlers))

#Load data in from gen
vac_world <- read.csv("venv/out/covid_data_world.csv")
vac_lta <- read.csv("venv/out/vaccine_uk_lta.csv")
vac_uk_nat <- read.csv("venv/out/vaccine_uk_nat.csv")
vac_uk_ov <- read.csv("venv/out/vaccine_uk_ov.csv")

###############################
## Total Vaccines used Graph ##
###############################
loginfo('RVIS: Generating Total Vaccine Graph')

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
  labs(caption = "source: Covid-19 dashboard") +
  theme(
    legend.position = "none",
    plot.caption = element_text(size = rel(0.5))
    ) +
  annotate("text", family = "Bitter", fontface="bold", size=20, x = as.Date("26/03/2021", format= "%d/%m/%Y"), y = 50000000, label = scales::comma(no_vac)) +
  annotate("text", family = "Bitter", fontface="bold", size=13, x = as.Date("26/03/2021", format= "%d/%m/%Y"), y = 33000000, label = "Vaccines given")
  
save_inews("venv/out/img/tot_vac.png")
loginfo('RVIS: Total vaccines completed')

###########################
## Vaccine Nation Graphs ##
###########################
loginfo('RVIS: Generating Vaccine competitiveness graphs')


#Nations graph
loginfo('RVIS: Generating nations graph')
 vac_nat_graph <- vac_uk_nat %>%
   select(areaName, cumVaccinationSecondDoseUptakeByPublishDatePercentage, cumVaccinationFirstDoseUptakeByPublishDatePercentage, date) %>%
   rename(
     location = areaName
   ) %>%
   mutate(
     date = as.Date(date, format="%Y-%m-%d"),
     cumVaccinationFirstDoseUptakeByPublishDatePercentage = cumVaccinationFirstDoseUptakeByPublishDatePercentage - cumVaccinationSecondDoseUptakeByPublishDatePercentage
   ) %>%
   pivot_longer(c(cumVaccinationFirstDoseUptakeByPublishDatePercentage, cumVaccinationSecondDoseUptakeByPublishDatePercentage), names_to="class_v", values_to="vacs") %>%
   mutate(
     vacs = vacs/100,
     class_v = ifelse(class_v == "cumVaccinationSecondDoseUptakeByPublishDatePercentage", "Two doses", ifelse(class_v == "cumVaccinationFirstDoseUptakeByPublishDatePercentage", "One dose", NA))
  )
 
vac_line <- vac_uk_ov %>%
  select(date, cumVaccinationFirstDoseUptakeByPublishDatePercentage, cumVaccinationSecondDoseUptakeByPublishDatePercentage) %>%
  rename(
    first_dose = cumVaccinationFirstDoseUptakeByPublishDatePercentage,
    second_dose = cumVaccinationSecondDoseUptakeByPublishDatePercentage
  ) %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d")
  ) %>%
  pivot_longer(c(first_dose, second_dose), values_to="vacs", names_to="type") %>%
  mutate(
    type = ifelse(type == "first_dose", "One dose", "Two doses"),
    vacs = vacs/100
  )

ggplot(data=vac_line ,aes(x=date, y=vacs, group = type, colour = type))+
 geom_line() + 
 scale_y_continuous(expand = expansion(mult = c(0, .1)), labels=scales::percent) +
 theme_inews_basic() +
 labs(title = "People fully vaccinated (UK)", subtitle="UK, by % of population with one or two vaccine doses", caption = "source: Covid-19 dashboard") +
 scale_colour_inews() +
 theme(
   plot.title = element_text(hjust = 0.5, margin = margin(b = 5, unit = "pt")),
   plot.subtitle = element_text(hjust = 0.5)
 )

save_inews("venv/out/img/uk_vac_line.png", width=20)
loginfo('RVIS: UK lines generated')

####Nat bar
vac_bar <- vac_nat_graph %>% subset(
  date == max(date)
)
ggplot(data=vac_bar ,aes(x=location, y=vacs, fill = class_v))+
  geom_bar(stat = "identity") + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), labels=scales::percent) +
  theme_inews_basic() +
  labs(title = "People fully vaccinated (UK)", subtitle="UK Nations, by % of population with one or two vaccine doses", caption = "source: Covid-19 dashboard") +
  scale_colour_inews() +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 5, unit = "pt")),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = rel(0.5))
  )

save_inews("venv/out/img/nations_vac_bar.png", width=20)
loginfo('RVIS: Nations graph generated')

## LTA Graph
loginfo('RVIS: Generating LTA Graph')
#Load in LTas
lad_2019 <- st_read("C:/Users/Tom/Google Drive/Inews/mapfiles/lad_2019/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC.shp", stringsAsFactors = FALSE) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

#Merge with LTA Covid Data
bottom10 <- vac_lta %>%
  arrange(cumVaccinationSecondDoseUptakeByPublishDatePercentage) %>%
  head(10) %>%
  pull(areaName)

vac_lta_bind <- vac_lta %>%
  select(areaCode,areaName,Long, Lat, cumVaccinationFirstDoseUptakeByPublishDatePercentage, cumVaccinationSecondDoseUptakeByPublishDatePercentage) %>%
  rename(
    first_dose = cumVaccinationFirstDoseUptakeByPublishDatePercentage,
    second_dose = cumVaccinationSecondDoseUptakeByPublishDatePercentage
  ) %>%
  mutate(
    second_dose = second_dose
  )

det_band <- function(x){
  if (x < 30){
    band = "20-30%"
  }
  if (x >= 30 & x < 40){
    band = "30-40%"
  }
  if (x < 50 & x >= 40){
    band = "40-50%"
  }
  if (x < 60 & x >= 50){
    band = "50-60%"
  }
  if (x < 70 & x >= 60){
    band = "60-70%"
  }
  if (x >= 70){
    band = "70-80%"
  }
  return(band)
}

vac_lad <- merge(lad_2019, vac_lta_bind, by.x = "lad19cd", by.y = "areaCode", all.x=TRUE) %>%
  drop_na(second_dose)

vac_lad$band = lapply(vac_lad$second_dose, det_band)

ggplot(vac_lad) +
  geom_sf(aes(fill = as.factor(unlist(band)), geometry = geometry), size = 0,  na.rm = FALSE) +
  scale_fill_manual(values = c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4")) + 
  theme_inews_map() +
  labs(title= "Vaccine coverage by lower authority", subtitle = "Second dose uptake by LTA, (% for All adults)", caption = "source: ONS/Covid-19 dashboard \nCopyright I")

ggsave("venv/out/img/LTA_uptake.png", dpi = 300, type = "cairo", width = 25, height = 25, units = "cm")
loginfo('RVIS: Generated LTA uptake Map')

##################
## World Graphs ##
##################
loginfo('RVIS: Starting World Maps')

vac_world <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d")
  )

# Ensure that populations below 1 mill aren't included:
pop <- read.csv("img_gen/pop.csv")
#img_gen/

vac_world_t <- merge(vac_world, pop, by.x = "iso_code", by.y = "code", all.x=TRUE) %>%
  drop_na(pop) %>%
  subset(
    pop > 1000000
  )

final_vac <- vac_world_t %>%
  group_by(location) %>%
  summarise(
    final_vac = ifelse(date == max(date), total_vaccinations_per_hundred, NA)
  ) %>% drop_na() %>%
  arrange(desc(final_vac)) %>%
  ungroup %>%
  slice_max(final_vac, n=10)

vac_world_graph <- vac_world %>%
  subset(
    location %in% final_vac$location
  ) %>%
  select(
    date, location, total_vaccinations_per_hundred
  ) %>%
  rename(
    vacs = total_vaccinations_per_hundred
  ) %>%
  mutate(
    vacs = vacs/100
  ) %>%
  drop_na(vacs)

ggplot(data=vac_world_graph, aes(x=date, y=vacs, colour = location, group=location))+
  geom_line() +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), labels=scales::percent) +
  theme_inews_basic() +
  labs(title = "Top 10 vaccinated countries", subtitle="Top countries, by number of vaccines per 100 residents*", caption = "source: Our World in Data \n*Excludes countries with population less than one million") +
  scale_colour_inews() + 
  theme(
    plot.caption = element_text(size = rel(0.5))
  )

save_inews("venv/out/img/topworld.png", width=20)
loginfo('RVIS: top world generated')

###Facet wrapped world graph
world_vac <- vac_world %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d")
  ) %>%
  group_by(iso_code) %>%
  mutate(
    final_label = ifelse(date == max(date), round(total_vaccinations_per_hundred), NA),
    count = n()
  ) %>% subset(
    count > 10
  ) %>%
  arrange(desc(final_label)) %>% ungroup%>% 
  mutate(
    total_vaccinations_per_hundred = total_vaccinations_per_hundred/100
  ) %>%
  rename(
    vacs = total_vaccinations_per_hundred
  ) %>% select(iso_code, date, vacs, location, final_label) %>% drop_na(vacs) %>% drop_na(location) %>% subset(location != "Kuwait")

world_vac <- merge(world_vac, pop, by.x = "iso_code", by.y = "code", all.x=TRUE) %>%
  drop_na(pop) %>%
  subset(
    pop > 1000000
  )



#Add income type for colour, also gets rid of non countries
inc_type <- read.csv("img_gen/inc_type.csv")

world_vac <- merge(world_vac, inc_type, by.x = "iso_code", by.y = "code", all.x=TRUE) %>%
  drop_na(type)

#Ensure that graphs are ordered by vaccine succes, not name
world_vac$location = factor(world_vac$location)

new_levels <- world_vac %>%
  drop_na(final_label) %>%
  arrange(desc(final_label)) %>%
  pull(location)

world_vac$location = factor(world_vac$location, levels=new_levels) 

world_vac <- world_vac %>% drop_na(location)


ggplot(data=world_vac, aes(x=date, y=vacs, colour = as.factor(type)))+
  theme_inews_basic() +
  theme(
    plot.caption = element_text(hjust=0.5, margin = margin(t=1, unit="cm")),
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
  ) + 
  scale_color_manual(breaks=c("High income", "Upper middle income", "Lower middle income", "Low income"), values=c("#7b3294", "#c2a5cf", "#a6dba0", "#008837")) +
  facet_wrap(vars(location), ncol=5, labeller = label_wrap_gen(width=20)) +
  geom_line() +
  geom_text(aes(label = final_label), nudge_x = -10, nudge_y = 0.8, na.rm = TRUE, size = 4) + 
  labs(title = "All countries", subtitle="Vaccines per 100 residents (number indicates most recent data)", caption = "source: Our World in Data \n(countries < 1 million population excluded) \nBy Tom Saunders")

ggsave("venv/out/img/all_world.png", dpi = 300, type = "cairo", width = 20, height = 150, units = "cm", limitsize = FALSE)
loginfo('RIVS: Whole world built')
#

#######################################
## Generate Age and gender breakdown ##
#######################################


loginfo('RIVS: building age breakdown')
age <- read.csv("img_gen/age.csv") %>%
  pivot_longer (!c(type, gender), names_to="age", values_to="coverage") 

ex_age <- function(x){
  #Particular cases
  if (x == "X80."){
    nstring = "80+"
  }
  else if (x == "Under.30"){
    nstring = "18-29"
  }
  else{
    nstring <- gsub(".", "-", x, fixed=TRUE)
    nstring <- gsub("X", "",nstring)
  }
  return(nstring)
}

age$age <- lapply(age$age, ex_age)
age$age <- unlist(age$age)
age$age <- factor(age$age, levels=c("18-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))


age_ons <- read.csv("img_gen/age_ons.csv")
age <- merge(age, age_ons, by=c("age", "gender", "coverage", "type"), all=TRUE)



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
  scale_y_continuous(labels = abs_l, limits = c(-1,1)*lims, breaks = c(-25000000, -1000000, 0, 1000000, 25000000)) +
  coord_flip() +
  theme_inews_basic() + 
  scale_fill_manual(labels =c("Female (2 doses)", "Male (2 doses)", "Female (1 dose)", "Male (1 dose)", "Female (No vaccine)", "Male (No vaccine)"), values=c("#7B4C99","#777CE6",  "#E664D7", "#8FA6D6", "#85D69C", "#85D69C")) + #"#efb6e8", "#8fd5d6"
  labs(title="Vaccine coverage", subtitle = "Vaccines given, by age and gender, as of June 10th, ", caption = "source: PHE Weekly statistics") + 
  theme(
    plot.caption = element_text(size = rel(0.5))
  )

save_inews("venv/out/img/vac_by_age.png")
loginfo('RIVS: ALL graphs generated')