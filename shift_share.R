
# load libraries
library(tidyverse)
library(readxl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### LOAD DATA
# see README for data sources
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# GVA data 
regional_gva <- read_excel("data/regional_gva.xlsx", 
                           sheet = "Table3c", skip = 1)

# WfH proportions by ITL3 region
whf_itl3 <- read_excel("data/whf_itl3.xlsx", 
                       skip = 3,
                       n_max = 176,
                       col_types = c("text", "numeric", "numeric", "numeric")) %>%
  mutate(itl3 = str_replace(`NUTS3 region`, "^UK", "TL"),
         wfh_change = `2020` - `2018`,
         wfh_growth = (`2020` - `2018`)/`2018`)

# Rural/Urban classification for ITL3 regions
Rural_Urban_NUTS3 <- read_csv("data/Rural_Urban_Classification_(2011)_of_NUTS_3_(2015)_in_England.csv") %>%
  mutate(itl3 = str_replace(NUTS315CD, "^UK", "TL"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### PROCESS DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# keep only England
# select years of interest
# select only major industry rows
regional_gva_prep <- regional_gva %>%
  rename(yr1 = `2019`,
         yr2 = `2021`) %>%
  filter(str_detect(`SIC07 code`, "^[A-Z][ ].*"),
         !str_detect(`ITL region code`, "^TL[LNM]")) %>% #just keep England
  select(all_of(c("ITL region code", 
                  "ITL region name",
                  "SIC07 code", 
                  "SIC07 description",
                  "yr1", 
                  "yr2"))) %>%
  mutate(regional_sector_growth = (yr2-yr1)/yr1,
         label_short = str_extract(`SIC07 code`, "^[A-Z]"),
         label = paste0(label_short, " - ", `SIC07 description`))

# find the national growth for each industry sector
sectoral_growth <- regional_gva_prep %>%
  summarise(yr1 = sum(yr1),
            yr2 = sum(yr2),
            .by = label) %>%
  mutate(national_sectoral_growth = (yr2-yr1)/yr1)

# find the overall national growth
national_growth <- regional_gva_prep %>%
  summarise(yr1 = sum(yr1),
            yr2 = sum(yr2)) %>%
  mutate(national_growth = (yr2-yr1)/yr1) %>%
  pull(national_growth)

# find the average regional growth
regional_growth <- regional_gva_prep %>%
  summarise(yr1 = sum(yr1),
            yr2 = sum(yr2),
            .by = `ITL region code`) %>%
  mutate(regional_growth = (yr2-yr1)/yr1)

# add the sub calculations back in to get table ready for summarising
regional_gva_prep2 <- regional_gva_prep %>%
  left_join(sectoral_growth %>% select(label, national_sectoral_growth),
            by = "label") %>%
  left_join(regional_growth %>% select(`ITL region code`, regional_growth),
            by = "ITL region code")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### CALCULATE SHIFT-SHARE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

regional_gva_shiftshare <- regional_gva_prep2 %>%
  summarise(NE = sum(yr1 * national_growth)/sum(yr1),
            SE = sum(yr1 * (national_sectoral_growth - national_growth))/sum(yr1),
            CE = sum(yr1 * (regional_sector_growth - national_sectoral_growth)/sum(yr1)),
            .by = `ITL region code`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### PLOT SHIFT-SHARE FOR A REGION
# as in figure 4.4 of Capello
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

regional_gva_prep2 %>%
  filter(`ITL region name` == "Enfield") %>%
  ggplot(aes(x = national_sectoral_growth, y = regional_sector_growth, label = label_short)) +
  geom_point() +
  geom_text(nudge_y = 0.02) +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = 2) + 
  geom_vline(xintercept = national_growth,
             linetype = 3) + 
  geom_hline(data = function(x) distinct(select(x, regional_growth)),
             aes(yintercept = regional_growth),
             linetype = 3) +
  geom_vline(xintercept = 0,
             linetype = 1) + 
  geom_hline(yintercept = 0,
             linetype = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Playing around with the results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# with shift-share
regional_gva_shiftshare %>%
  left_join(whf_itl3,
            by = c("ITL region code" = "itl3")) %>%
  left_join(Rural_Urban_NUTS3,
            by = c("ITL region code" = "itl3")) %>%
  filter(!is.na(wfh_growth)) %>%
  ggplot()+
  geom_point(aes(x = wfh_change, y = CE, color = Broad_RUC11))



# not shift-share

retail_wfh <- regional_gva_prep2 %>%
  #filter(str_detect(`ITL region code`, "TLI.*")) %>%
  filter(label_short == "R") %>%
  left_join(whf_itl3,
            by = c("ITL region code" = "itl3")) %>%
  left_join(Rural_Urban_NUTS3,
            by = c("ITL region code" = "itl3")) %>%
  filter(!is.na(wfh_growth))

retail_wfh %>%
  ggplot() +
  geom_point(aes(x = wfh_growth, y = regional_sector_growth, color = Broad_RUC11)) +
  facet_wrap(~Broad_RUC11)

lm_wfh <- lm(regional_sector_growth ~  wfh_growth + Broad_RUC11,
             retail_wfh)

summary(lm_wfh)         
