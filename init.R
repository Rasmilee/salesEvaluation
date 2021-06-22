my_packages = c("randomForest", "data.table","shiny","shinydashboard","tidyverse", "lubridate","dplyr","tidyr","shinyjs","sodium","TTR", "forecast","party")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
