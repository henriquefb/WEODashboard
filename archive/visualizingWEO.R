library(tidyverse)
setwd("/Users/henrique/Desktop/weoapp/")

oct2019 <- read.csv("Data/WEOOct2019all.csv")
apr2020 <- read.csv("Data/WEOApr2020all.csv")
oct2020 <- read.csv("Data/WEOOct2020all.csv")
apr2021 <- read.csv("Data/WEOApr2021all.csv")

weo <- oct2019

reshapingWEO <- function(weo, yr) {
  weo$WEO.Subject.Code <- paste0(weo$WEO.Subject.Code,"_", yr)
  code_book<- weo %>% select(WEO.Subject.Code, Subject.Descriptor, Subject.Notes, Units, Scale) %>% distinct()
  weo <- weo %>% select(-c(WEO.Country.Code ,Subject.Descriptor, Subject.Notes, Units, Scale, Country.Series.specific.Notes, Estimates.Start.After))
  weo <- weo %>% mutate_at(vars(starts_with("X")), as.character)
  weo <- weo %>% mutate_at(vars(starts_with("X")), as.numeric)
  weo <- weo %>% 
    pivot_longer(cols = starts_with("X"),
                 names_to = "year", 
                 names_prefix = "X",
                 values_to = "value", 
                 values_drop_na = TRUE)
  

  weo <- weo %>%
    filter(WEO.Subject.Code!="")
  
  weo <- weo %>% 
    pivot_wider(names_from = WEO.Subject.Code, values_from = value, names_repair="check_unique")
  
  return(weo)
}

oct2019_r <- reshapingWEO(oct2019, "oct2019")
apr2020_r <- reshapingWEO(apr2020, "apr2020")
oct2020_r <- reshapingWEO(oct2020, "oct2020")
apr2021_r <- reshapingWEO(apr2021, "apr2021")


full_weo <- oct2019_r %>% full_join(apr2020_r)
full_weo <- full_weo %>% full_join(oct2020_r)
full_weo <- full_weo %>% full_join(apr2021_r)

subcountries <- c("Denmark", "Finland", "Norway", "Sweden")

full_weo %>%
  filter(Country %in% subcountries & year>2015) %>%
  ggplot(aes(x=year, y=LUR_apr2021, group=Country, color = Country)) + 
  geom_line()

