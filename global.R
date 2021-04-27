httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))


library(shiny)
#library(leaflet)
#library(inlmisc)
library(tidyverse)
library(sf)
library(DT)
library(config)
library(pins)

addUnits_envDataDF <- function(envData){
  return(rename(envData, `pH (unitless)`= pH, `DO (mg/L)` = DO, `TN (mg/L)` = TN, 
                `TP (mg/L)` = TP, `Total Habitat (unitless)`= TotalHabitat,
                `LRBS (unitless)`= LRBS, `MetalsCCU (unitless)`= MetalsCCU,
                `SpCond (uS/cm)` = SpCond,  `TDS (mg/L)` = TDS,  
                `DSulfate (mg/L)` = DSulfate, `DChloride (mg/L)` = DChloride, 
                `DPotassium (mg/L)` = DPotassium, `DSodium (mg/L)` = DSodium,
                `Temperature (C)` = Temp))
}

removeUnits_envDataDF <- function(envData){
  return(rename(envData,  pH = "pH..unitless.", DO = "DO..mg.L." , TN =  "TN..mg.L.", 
                TP = "TP..mg.L.", TotalHabitat = "Total.Habitat..unitless.",
                LRBS =  "LRBS..unitless." , MetalsCCU = "MetalsCCU..unitless.",
                SpCond = "SpCond..uS.cm.",  TDS = "TDS..mg.L." ,  
                DSulfate = "DSulfate..mg.L.", DChloride= "DChloride..mg.L.", 
                DPotassium = "DPotassium..mg.L.", DSodium = "DSodium..mg.L.",
                Temp = "Temperature..C."))
}