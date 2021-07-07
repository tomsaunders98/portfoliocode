library(tidyverse)
library(patchwork)
library(tidybayes)
library(lubridate)
library(StanHeaders)
library(rstan)
library(brms)
library(here)
library(InewsTheme)



polls_long <- read.csv("polls.csv") %>%
  tibble() %>%
  mutate(
    StartDate = as.Date(StartDate, format = "%d/%m/%Y"),
    EndDate = as.Date(EndDate, format = "%d/%m/%Y"),
    date_long = EndDate - (1 + as.numeric(EndDate-StartDate)) %/% 2, # Midpoint as date val (field dates are so close this is almost pointless)
    date_index = 1 + as.numeric(date_long) - min(as.numeric(date_long)), #bind date index to earliest poll
    Area = as.factor(Area)
  ) 

pdate <- polls_long %>%
  select(date_long, date = date_index)

polls <- polls_long %>%
  select(
    con = Con,
    lab = Lab,
    lib = Lib.Dem,
    snp = SNP,
    grn = Green,
    area = Area,
    date = date_index,
    pollster = Pollster
  ) %>%
  mutate(
    oth = 1- (con+lab+lib+snp+grn),
  ) %>%
  subset(oth > 0)

npolls <- polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("con", "lab", "lib","snp", "grn", "oth")])
  )



## Fit Model

m1 <-
  brm(formula = bf(outcome ~ 1 + area + s(date, k = 10) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "oth"),
      prior =
        prior(normal(0, 1.5), class = "Intercept", dpar = "mucon") +
        prior(normal(0, 0.5), class = "b", dpar = "mucon") +
        prior(exponential(2), class = "sd", dpar = "mucon") +
        prior(exponential(2), class = "sds", dpar = "mucon") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "mugrn") +
        prior(normal(0, 0.5), class = "b", dpar = "mugrn") +
        prior(exponential(2), class = "sd", dpar = "mugrn") +
        prior(exponential(2), class = "sds", dpar = "mugrn") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "mulab") +
        prior(normal(0, 0.5), class = "b", dpar = "mulab") +
        prior(exponential(2), class = "sd", dpar = "mulab") +
        prior(exponential(2), class = "sds", dpar = "mulab") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "mulib") +
        prior(normal(0, 0.5), class = "b", dpar = "mulib") +
        prior(exponential(2), class = "sd", dpar = "mulib") +
        prior(exponential(2), class = "sds", dpar = "mulib") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "musnp") +
        prior(normal(0, 0.5), class = "b", dpar = "musnp") +
        prior(exponential(2), class = "sd", dpar = "musnp") +
        prior(exponential(2), class = "sds", dpar = "musnp") +
        prior(gamma(1, 0.01), class = "phi"),
      backend = "rstan",
      data = npolls,
      seed = 666,
      iter = 2e3,
      chains = 4,
      cores = 4,
      refresh = 5,
      control =
        list(
          adapt_delta = .95,
          max_treedepth = 15
        ),
      file = here("_output", paste0("model", "-", Sys.Date()))
  )
max <- 1+as.numeric(max(polls_long$date_long))-min(as.numeric(polls_long$date_long))
pred_dta <-
  tibble(
    date = c(1:max),
    #date = seq(min(polls_long$date_long), max(polls_long$date_long), by = "day"),
    area = "GB"
  )

pred_dta <-
  add_fitted_draws(
    model = m1,
    newdata = pred_dta,
    re_formula = NA
  ) %>%
  group_by(date, .category) %>%
  summarise(
    est = median(.value),
    lower = quantile(.value, probs = .05),
    upper = quantile(.value, probs = .95),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  rename(party = .category)

clean_polls <- polls_long %>%
  rename(
    con = Con,
    lab = Lab, 
    lib = Lib.Dem,
    snp = SNP,
    grn = Green
  ) %>%
  pivot_longer(c(con, lab, lib, snp, grn), names_to = "party", values_to="polls") %>%
  select(party, polls, date = date_index, date_long)
pred_dates <- merge(clean_polls, pred_dta, by = c("date", "party")) %>% select(!date) %>%
  group_by(party) %>%
  mutate(
    fvab = ifelse(date_long == max(date_long), est, NA)
  )

dvabs <- data.frame(
  date_long = rep(seq(max(pred_dates$date_long)+1, max(pred_dates$date_long)+7, by = "day"), 5)
) %>% group_by(date_long) %>%
  mutate(
    party = c("con", "lab", "lib", "snp", "grn"),
    polls = NA,
    est = NA,
    lower = NA,
    upper = NA,
    fvab = NA
  )

pred_dates <- rbind(dvabs, pred_dates) %>%
  group_by(party) %>%
  mutate(
    final_val = ifelse(date_long == max(date_long), max(fvab, na.rm=T), NA),
    final_label = ifelse(date_long == max(date_long), paste(round(max(fvab, na.rm=T), 2)*100, "%", sep=""), NA)
  )


ggplot(pred_dates, aes(x = date_long, group = party, colour = party, fill = party)) +
  geom_line(aes(y = est), na.rm =T) +
  geom_point(aes(y = polls), alpha = 0.5, size = 0.5, na.rm = T) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5, colour = NA, na.rm = T) +
  theme_inews_basic() +
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(label = final_label, y = final_val), na.rm = T, size = rel(3)) +
  scale_fill_manual(values = c("#483ae8", "#60d662", "#f23809", "#f2a809", "#f2e209"), labels=c("Conservative", "Greens", "Labour", "Liberal Democrats", "SNP")) +
  scale_colour_manual(values = c("#483ae8", "#60d662", "#f23809", "#f2a809", "#f2e209"), guide="none") +
  labs(title = "The I's Poll of Polls")

save_inews("pgraph.png", width_i = 20)
