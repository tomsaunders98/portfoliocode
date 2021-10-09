library(sf)
library(tidyverse)
library(InewsTheme)

t_area <- read.csv("Gbelt_train_area_pure.csv") %>%
  select(lad19cd, t_area=Area)
l_area <- read.csv("La_area.csv") %>%
  select(lad19cd, lad19nm, l_area = Area)

area <- merge(t_area, l_area, by="lad19cd") %>%
  mutate(
    p_area = t_area/l_area
  ) %>% arrange(desc(p_area)) %>%
  head(10)

ggplot(area, aes(x=reorder(lad19nm, p_area), y=p_area, fill=lad19nm)) +
  geom_col() +
  coord_flip() +
  theme_inews_basic() +
  theme(legend.position = "none") +
  scale_fill_inews() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Local authorities with largest % of usable green belt", caption = "source: MHCLG, DEFRA, Ordinance Survey") 

save_inews("gbelt_area_bar.png")
save_inews("gbelt_area_bar.svg", device="svg")

write.csv(area, "area.csv", row.names = F)