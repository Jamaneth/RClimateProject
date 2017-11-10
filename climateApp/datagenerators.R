library(tidyverse)
library(ggplot2)
library(ggthemes)

genGlobalTemperatures <- function(){
# Fonction pour générer la base de données de températures annuelles au niveau global à partir
# de la base de données de Berkeley Earth
  globalTemperatures <- read.csv("globalTemperatures.csv")
  
  # Séparer les dates en année/mois/jour pour ne sélectionner que les années et les mois
  globalTemperatures$dt <- as.character(globalTemperatures$dt)
  globalTemperatures <- globalTemperatures %>%
    separate("dt", c("Year", "month", "day"), sep = "-", remove = TRUE) %>%
    select(-day)
  
  globalTemperatures$Year <- as.numeric(globalTemperatures$Year)
  
  # Faire la moyenne par an
  globalTemperaturesYear <- globalTemperatures %>%
    group_by(Year) %>%
    summarise(LandAverageTemperature = mean(LandAverageTemperature))
  
  # Ajouter une colonne avec la moyenne étalée sur dix ans
  globalTemperaturesYear$TenYearAvg <- NA
  
  for (i in c(10:length(globalTemperaturesYear$TenYearAvg))) {
    globalTemperaturesYear$TenYearAvg[i] <-
      mean(globalTemperaturesYear$LandAverageTemperature[(i-9):i])
  }
  
  return(globalTemperaturesYear)
}

genCountryTemperatures <- function(country = "World", year = 1900) {
  # Fonction pour générer les données de températures annuelles par pays d'après la base
  # de données de Berkeley Earth
  tempsByCountry <- read.csv("GlobalLandTemperaturesByCountry.csv")
  
  # Mettre les dates en forme
  tempsByCountry$dt <- as.character(tempsByCountry$dt)
  tempsByCountry <- tempsByCountry %>%
    separate("dt", c("Year", "Month", "Day"), sep = "-", remove = TRUE) %>%
    select(-Day)
  tempsByCountry$Year <- as.numeric(tempsByCountry$Year)
  tempsByCountry$Country <- as.character(tempsByCountry$Country)
  
  # Filtrer les années à partir de l'année sélectionnée
  tempsByCountry <- tempsByCountry %>% filter(Year >= year - 9)

  # Nettoyage : remplacer Burma par Myanmar pour être reconnu par la carte sur Shiny
  tempsByCountry$Country = gsub("Burma", "Myanmar", tempsByCountry$Country)  
    
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

    AvgTempByCountry <- tempsByCountry %>%
      group_by(Country) %>%
      summarise(AverageTemperature = mean(AverageTemperature, na.rm = TRUE)) %>%
      spread(key = Country, value = AverageTemperature)
    
    tempsByCountry$TenYearAvg = NA
    tempsByCountry$TempDiff = NA
    
    for (i in c(10:length(tempsByCountry$TenYearAvg))) {
      if(tempsByCountry$Country[i] == tempsByCountry$Country[i-9]) {
        tempsByCountry$TenYearAvg[i] <-
          mean(tempsByCountry$AverageTemperature[(i-9):i])
        tempsByCountry$TempDiff[i] <- tempsByCountry$TenYearAvg[i] - AvgTempByCountry[tempsByCountry$Country[i]]

      }
    }
    
    tempsByCountry$TempDiff <- as.numeric(tempsByCountry$TempDiff)
    return(tempsByCountry)
    
  }
  else {
    print("Invalid argument.")
    return(NULL)
  }
  
}

genCarbonDioxide <- function(){
  # Fonction pour nettoyer la base de données de la concentration atmosphérique en CO2
  carbonDioxide <- read.csv("GlobalCarbonDioxide.csv")  
  
  carbonDioxide <- carbonDioxide %>% filter(year >= 1750) %>%
    select(year, data_mean_global)
  colnames(carbonDioxide) <- c("Year", "CO2")
  return(carbonDioxide)
}

genOverview <- function(){
  # Fonction pour lier les températures moyennes annuelles au niveau du monde et
  # la concentration atmosphérique en C02 
  carbonDioxide <- genCarbonDioxide()
  globalTemps <- genGlobalTemperatures()
  overview = globalTemps %>% full_join(carbonDioxide, by = "Year")
  return(overview)
}