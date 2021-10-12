library(tidyverse)

oct2019 <- read.csv("Data/WEOOct2019all.csv", stringsAsFactors=FALSE)
apr2020 <- read.csv("Data/WEOApr2020all.csv", stringsAsFactors=FALSE)
oct2020 <- read.csv("Data/WEOOct2020all.csv", stringsAsFactors=FALSE)
apr2021 <- read.csv("Data/WEOApr2021all.csv", stringsAsFactors=FALSE)

weo <- oct2020
weo$Version <- "oct2020"

reshapingWEO <- function(weo, yr) {
  weo$Version <- as.character(deparse(substitute(weo)))
  code_book<- weo %>% select(WEO.Subject.Code, Subject.Descriptor, Subject.Notes, Units, Scale, Version) %>% distinct()
  weo <- weo %>% select(-c(WEO.Country.Code ,Subject.Descriptor, Subject.Notes, Units, Scale, Country.Series.specific.Notes, Estimates.Start.After))
  weo <- weo %>% mutate_if(is.character, str_replace_all, pattern=",", replacement="")
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


full_weo <- rbind(oct2019_r, oct2020_r)



full_weo %>% filter(Country == "Norway" & year>2018 & year < 2024) %>% 
  ggplot(aes(y=LUR, x=year, fill=Version)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim=c(2.5, 5)) + 
  geom_text(aes(label=LUR), vjust=1.6, color="white",
                                              position = position_dodge(0.9), size=3)

#subcountries <- c("Denmark", "Finland", "Norway", "Sweden")
#subcountries <- c("Denmark")
# # 
# full_weo %>%
#   filter(Country %in% subcountries & year>2018 & year < 2024) %>%
#   ggplot(aes(x=year, y=NGDP, group=Version)) +
#   geom_line()
# # 
# full_weo %>%
#   filter(Country %in% subcountries & year>2018) %>%
#   ggplot(aes(y=NGDP_RPCH, x=year, fill=Version)) +
#   geom_bar(position="dodge", stat="identity")

