library(tidyverse)
library(ggplot2)
library(ggthemes)

genGlobalTemperatures <- function(){
  
  globalTemperatures <- read.csv("globalTemperatures.csv")
  
  # Séparer les dates en année/mois/jour pour ne sélectionner que les années et les mois
  globalTemperatures$dt <- as.character(globalTemperatures$dt)
  globalTemperatures <- globalTemperatures %>%
    separate("dt", c("year", "month", "day"), sep = "-", remove = TRUE) %>%
    select(-day)
  
  globalTemperatures$year <- as.numeric(globalTemperatures$year)
  
  # Faire la moyenne par an
  globalTemperaturesYear <- globalTemperatures %>%
    group_by(year) %>%
    summarise(LandAverageTemperature = mean(LandAverageTemperature))
  
  # Ajouter une colonne avec la moyenne étalée sur dix ans
  globalTemperaturesYear$TenYearAvg <- NA
  
  for (i in c(10:length(globalTemperaturesYear$TenYearAvg))) {
    globalTemperaturesYear$TenYearAvg[i] <-
      mean(globalTemperaturesYear$TenYearAvg[(i-9):i])
  }
  
  return(globalTemperaturesYear)
}


genCountryTemperatures <- function(country = "World", year = 1900) {
  tempsByCountry <- read.csv("GlobalLandTemperaturesByCountry.csv")
  
  # Mettre les dates en forme
  tempsByCountry$dt <- as.character(tempsByCountry$dt)
  tempsByCountry <- tempsByCountry %>%
    separate("dt", c("Year", "Month", "Day"), sep = "-", remove = TRUE) %>%
    select(-Day)
  tempsByCountry$Year <- as.numeric(tempsByCountry$Year)
  
  # Filtrer les années à partir de l'année sélectionnée
  tempsByCountry <- tempsByCountry %>% filter(Year >= year - 9)
  
  # Faire la moyenne par an
  tempsByCountry <- tempsByCountry %>%
    group_by(Year, Country) %>%
    summarise(AverageTemperature = mean(AverageTemperature)) %>%
    arrange(Country)
  
  # Sélectionner un pays
  if (country %in% tempsByCountry$Country) {
    
    tempsByCountry = tempsByCountry %>% filter(Country == country)
    tempsByCountry$TenYearAvg = NA
    for (i in c(10:length(tempsByCountry$TenYearAvg))) {
      tempsByCountry$TenYearAvg[i] <- mean(tempsByCountry$AverageTemperature[(i-9):i])
    }
    return(tempsByCountry)
    
  }
  else if(country == "World") {

    tempsByCountry$TenYearAvg = NA
    for (i in c(10:length(tempsByCountry$TenYearAvg))) {
      if(tempsByCountry$Country[i] == tempsByCountry$Country[i-9]) {
        tempsByCountry$TenYearAvg[i] <- mean(tempsByCountry$AverageTemperature[(i-9):i])
      }
    }
    return(tempsByCountry)
    
  }
  else {
    print("Invalid argument.")
    return(NULL)
  }
  
}

tempsByCountry <- genCountryTemperatures(country = "Spain", year = 1980)
View(tempsByCountry)
