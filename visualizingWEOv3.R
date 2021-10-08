library(tidyverse)

oct2019 <- read.csv("Data/WEOOct2019all.csv", stringsAsFactors=FALSE)
apr2020 <- read.csv("Data/WEOApr2020all.csv", stringsAsFactors=FALSE)
oct2020 <- read.csv("Data/WEOOct2020all.csv", stringsAsFactors=FALSE)
apr2021 <- read.csv("Data/WEOApr2021all.csv", stringsAsFactors=FALSE)

reshapingWEO <- function(weo) {
  weo$Version <- as.character(deparse(substitute(weo)))
  code_book<- weo %>% select(WEO.Subject.Code, Subject.Descriptor, Subject.Notes, Units, Scale, Version) %>% distinct()
  weo <- weo %>% select(-c(WEO.Country.Code ,Subject.Descriptor, Subject.Notes, Units, Scale, Country.Series.specific.Notes, Estimates.Start.After))
  weo <- weo %>% mutate_if(is.character, str_replace_all, pattern=",", replacement="")
  weo <- weo %>% mutate_at(vars(starts_with("X")), as.numeric)
  weo <- weo %>% mutate_at(vars(starts_with("X")), round, digits=1)
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

oct2019_r <- reshapingWEO(oct2019)
apr2020_r <- reshapingWEO(apr2020)
oct2020_r <- reshapingWEO(oct2020)
apr2021_r <- reshapingWEO(apr2021)


full_weo <- rbind(oct2019_r, oct2020_r)
full_weo <- rbind(full_weo, apr2021_r)

write.csv(full_weo, file="Data/WEOpanelApr2021.csv")

save(full_weo, file="Data/WEOpanelApr2021.Rdata")

code_book <- apr2021 %>% select(WEO.Subject.Code, Subject.Descriptor, Subject.Notes, Units, Scale) %>% distinct()
save(code_book, file="Data/WEOcodebook.Rdata")


