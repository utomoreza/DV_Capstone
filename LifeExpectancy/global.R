library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(plotly)
library(scales)
library(DT)

lifeExp <- as.data.frame(read_csv("life-expectancy.csv"))
MenVSWomen <- as.data.frame(read_csv("life-expectancy-of-women-vs-life-expectancy-of-women.csv"))

