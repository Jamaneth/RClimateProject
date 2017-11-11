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
  
  # Nettoyage : notre base de données n'a pas d'informations pour le Sud Soudan, donc on les
  # calque sur celles du Soudan par défaut
  tempsSouthSudan <- tempsByCountry %>% filter(Country == "Sudan")
  tempsSouthSudan$Country <- gsub("Sudan", "South Sudan", tempsSouthSudan$Country)
  tempsByCountry <- rbind(tempsByCountry, tempsSouthSudan)
    
  # Faire la moyenne par an
  tempsByCountry <- tempsByCountry %>%
    group_by(Year, Country) %>%
    summarise(AverageTemperature = mean(AverageTemperature)) %>%
    arrange(Country)
  
  # Sélectionner un pays
  if (country %in% tempsByCountry$Country) {
    
    # Calcul de la moyenne glissante sur 10 ans
    tempsByCountry = tempsByCountry %>% filter(Country == country)
    tempsByCountry$TenYearAvg = NA
    for (i in c(10:length(tempsByCountry$TenYearAvg))) {
      tempsByCountry$TenYearAvg[i] <- mean(tempsByCountry$AverageTemperature[(i-9):i])
    }
    return(tempsByCountry)
  }
  else if(country == "World") {
  # Si aucun pays n'est sélectionné, alors la table est créée pour tous les pays du monde
    AvgTempByCountry <- tempsByCountry %>%
      group_by(Country) %>%
      summarise(AverageTemperature = mean(AverageTemperature, na.rm = TRUE)) %>%
      spread(key = Country, value = AverageTemperature)
    
    tempsByCountry$TenYearAvg = NA
    tempsByCountry$TempDiff = NA
    # Calcul de la moyenne glissante sur 10 ans    
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
  # Fonction pour nettoyer la base de données pour la concentration atmosphérique en CO2
  carbonDioxide <- read.csv("GlobalCarbonDioxide.csv")  
  
  carbonDioxide <- carbonDioxide %>% filter(year >= 1750) %>%
    select(year, data_mean_global)
  colnames(carbonDioxide) <- c("Year", "CO2")
  return(carbonDioxide)
}

genOverview <- function(){
  # Fonction pour lier les températures moyennes annuelles au niveau du monde et
  # la concentration atmosphérique en CO2 
  carbonDioxide <- genCarbonDioxide()
  globalTemps <- genGlobalTemperatures()
  overview = globalTemps %>% full_join(carbonDioxide, by = "Year")
  return(overview)
}

genOverviewPrediction <- function(){
  # On établit le modèle de régression linéaire sur la base de données à partir de 1970.
  # En effet, un étude préliminaire a montré que, sur la période 1970-2014 :
  # -> La concentration de CO2 suit une progression quasi-linéaire,
  # -> La corrélation entre la concentration de CO2 et la température globale est égale à 0.99,
  #    ce qui justifie l'établissement d'un modèle linéaire.
  overviewPrediction <- genOverview() %>% filter(Year >= 1970)
  modelCO2 = lm(formula = overviewPrediction$CO2 ~ overviewPrediction$Year)
  modelTemp = lm(formula = overviewPrediction$TenYearAvg ~ overviewPrediction$CO2)
  
  overviewPrediction = overviewPrediction %>%
    filter(Year == 2014)
  
  calcVect = c(overviewPrediction$Year[1], NA, overviewPrediction$TenYearAvg[1],
               overviewPrediction$CO2[1])
  # Pour une année donnée, on calcule les données de l'année d'après de la façon suivante :
  # -> Température moyenne : on suit le modèle linéaire que l'on vient de calculer automatiquement,
  # -> CO2 : on prend la concentration de CO2 de l'année dernière, et on ajoute le coefficient
  #    de proportionnalité obtenu dans le deuxième modèle.
  for(calcYear in c(2015:2030)){
   calcVect = c(calcVect[1] + 1,
                NA,
                modelTemp$coefficients[1] + modelTemp$coefficients[2] * calcVect[4],
                calcVect[4] + modelCO2$coefficients[2])
   overviewPrediction = rbind(overviewPrediction, calcVect)
  }
  # On arrête le calcul dès 2030, puisqu'il s'agit d'un modèle linéaire simple.
  
  return(overviewPrediction)

}

View(genOverviewPrediction())