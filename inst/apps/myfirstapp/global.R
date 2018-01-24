# Packages

library(shiny)
library(shinythemes)
library(choroplethr)
library(choroplethrMaps)

# Load the dataset from the choroplethrMaps package

data('df_state_demographics')
map_data <- df_state_demographics