library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(vcd)
library(cluster)
library(pROC)
library(caret)
library(tidyr)
library(class)

source('kmeans++.R')
source('diy.R')
source('ui.R')
source('server.R')


shinyApp(ui,server)


