library(sf)
library(InewsTheme)
library(tidyverse)
library(ggthemes)

gbelt <- st_read("gbelt/England_Green_Belt_2019-20_WGS84.shp") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

lad2020 <- st_read("lad2020/Local_Authority_Districts_(December_2020)_UK_BFC.shp") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

lad2019 <- st_read("lad_2019/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC.shp")%>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

lad2018 <- st_read("lad_2018/Local_Authority_Districts__December_2018__Boundaries_GB_BFC.shp")%>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

houseprices <- read.csv("hps.csv") %>%
  drop_na() %>%
  subset(price < 800000)

hmap <- merge(lad2020, houseprices, by.x = "LAD20CD", by.y = "code") %>%
  drop_na(price)


ggplot(hmap) +
  geom_sf(aes(fill = price, geometry = geometry), size = 0,  na.rm = FALSE) +
  geom_sf(data = gbelt, fill = "#0064CE", alpha =0.8, size=0) +
  scale_fill_distiller(palette = "RdYlGn", labels=scales::comma) +
  theme_inews_map() +
  labs(title="Greenbelt location + median house prices", subtitle="Median house prices by Local Authority*", caption="*Excluding Westminster, City of London + Chelsea") +
  theme(
    plot.title = element_text(family="sans"),
    plot.caption = element_text(family="sans"),
    plot.subtitle = element_text(family="sans"),
    legend.text = element_text(family="sans")
  )
  
save_inews("test.png", type="map")

##############################
## New Houses

nhs <- read.csv("Housebuilds_2019.csv") %>% #New developments
  drop_na(All) %>%
  select(
    code = Code,
    location = Location,
    all = All
  )

hproj <- read.csv("HouseholdProjections.csv") %>% #Population projections
  drop_na() %>%
  select(
    old_proj = X2019,
    proj = X2029,
    code = Code,
    location = Location
  ) # Pick today, 10 years in future

aff_ratio <- read.csv("AffordabilityRatio.csv") %>%  # Affordability ratios
  drop_na() %>%
  select(
    ratio = X2019,
    code = Code,
    location = Name
  )
   
### Update Household Projections to new codes
changes <- read.csv("Changes.csv") %>%
  subset(
    YEAR == 2019 | YEAR == 2020
  )

hproj <- merge(changes, hproj, by.x = "GEOGCD_P", by.y="code", all.y = TRUE) %>%
  mutate(
    code = ifelse(!is.na(GEOGCD), GEOGCD, GEOGCD_P)
  ) %>%
  group_by(code) %>%
  summarise(
    old_proj = sum(old_proj),
    proj = sum(proj)
  )
nhs <- merge(changes, nhs, by.x = "GEOGCD_P", by.y="code", all.y = TRUE) %>%
  mutate(
    code = ifelse(!is.na(GEOGCD), GEOGCD, GEOGCD_P)
  ) %>%
  group_by(code) %>%
  summarise(
    all = sum(all)
  )


gov_form <- function(x){ #Function to determine housing increases
  if(x > 4){
    adj = ((x-4)/4)*0.25 + 1
    return(adj)
  }else{
    return(1)
  }
}

setdiff(hproj$code, lad2019$lad19cd)

com_proj <- merge(hproj, aff_ratio, by = "code", all.y = TRUE) %>%
  mutate(
    ratio = as.numeric(ratio),
    proj = as.numeric(proj)*1000,
    old_proj = as.numeric(old_proj)*1000,
    inc = (proj-old_proj)/10
  ) 



com_proj$adj_pred <- sapply(com_proj$ratio, gov_form)
urban <- c("Birmingham", "Bradford", "Brighton and Hove", "Bristol, City of", "Coventry", "Derby", "Kingston upon Hull, City of", "Leeds", "Leicester", "Liverpool", "Manchester", "Newcastle upon Tyne", "Nottingham", 
           "Plymouth", "Reading", "Sheffield", "Southampton", "Stoke-on-Trent", "Wolverhampton", "Westminster", "Wandsworth", "Waltham Forest", "Tower Hamlets", "Sutton", "Southwark", "Richmond upon Thames", "Redbridge", "Newham", "Merton", "Lewisham", "Lambeth", 
           "Kingston upon Thames", "Kensington and Chelsea","Islington", "Hounslow", "Hillingdon", "Havering", "Harrow", "Haringey", "Hammersmith and Fulham", "Hackney", "Greenwich", "Enfield", "Ealing", "Croydon", "Camden", "Bromley", "Brent", "Bexley", "Barnet", "Barking and Dagenham")
com_proj <- com_proj %>%
  mutate(
    finalp = ifelse(location %in% urban, 1.35*((adj_pred * inc)*0.4), (adj_pred * inc)*0.4)
  )

rf <- merge(com_proj, nhs, by = "code") %>%
  mutate(
    res = all/finalp,
    res = ifelse(res > 4, 4, res),
    res = ifelse(res < 0, 0, res)
  )

setdiff(com_proj$code, nhs$code)

hp_map <- merge(lad2020, rf, by.x = "LAD20CD", by.y = "code", all.x=TRUE) %>%
  drop_na(res)

setdiff(lad2019$lad19cd, rf$code)


ggplot(hp_map) +
  geom_sf(aes(fill = res, geometry = geometry),  size=0, na.rm = FALSE) +
  scale_fill_gradient2(low = "#d73027", mid="#ffffbf", high="#006837", midpoint=1, labels=scales::percent) +
  theme_inews_map() +
  labs(title="Ratio of New dwellings vs projected demand", subtitle="Above 100% implies surpassed projection, below indicates failed to meet projection*", caption="Capped at 400% \n source: ONS/Ministroy for Housing") +
  theme(
    plot.title = element_text(family="sans"),
    plot.caption = element_text(family="sans"),
    plot.subtitle = element_text(family="sans"),
    legend.text = element_text(family="sans")
  )

  

save_inews("hps.png", type="map")

lad2020 %>% subset(
  LAD20CD == "E06000060"
)