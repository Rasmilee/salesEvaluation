my_packages = c("randomForest", "data.table","shiny","shinydashboard","tidyverse", "lubridate","dplyr","tidyr","shinyjs","sodium","TTR", "forecast","party")
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(shinyjs)
library(sodium)
library(data.table)
library(TTR)
library(forecast)
library(party)
library(randomForest)
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
