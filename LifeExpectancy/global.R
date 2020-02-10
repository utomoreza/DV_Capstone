library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggthemes)
library(plotly)
library(scales)
library(DT)


# define a function to add 'years' at the end of a string
add_year <- function(x) {return(paste(x, 'years'))}

# define a function to convert factor to numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

countries1 <- as.data.frame(read_tsv("countries.tsv", na = ""))

countries2 <- as.data.frame(read_tsv("countriesGoogle.tsv", na = "")) %>% 
     select(country, latitude, longitude)

countries <- left_join(countries1, countries2, by = c("Country Code" = "country"))

for (i in 1:length(countries$latitude)) {
     if (is.na(countries$latitude[i])) {
          countries$latitude[i] <- replace_na(countries$latitude[i], 
                                              countries$`Capital Latitude`[i])
          countries$longitude[i] <- replace_na(countries$longitude[i], 
                                               countries$`Capital Longitude`[i])
     }        
}
countries_ <- countries %>% 
     rename(Country = `Country Name`,
            Capital = `Capital Name`,
            Continent = `Continent Name`,
            Latitude = latitude,
            Longitude = longitude) %>% 
     select(Country, Capital, Continent, Latitude, Longitude) %>% 
     mutate(Continent = as.factor(Continent))
countries_$Country[52] <- "Cote d'Ivoire"
countries_$Capital[125] <- "Macau"
countries_$Country[174] <- "Democratic Republic of Congo"
countries_$Country[217] <- "Gambia"

lifeExp <- as.data.frame(read_csv("life-expectancy.csv"))
lifeExp_countries <- lifeExp %>% 
     filter((!is.na(Code) | Entity == "Saint Barthlemy") & Entity != "World") %>% 
     rename(Country = Entity, LE_age = `Life expectancy (years)`) %>% 
     mutate(Country = as.factor(Country), Code = as.factor(Code))
lifeExp_regions <- lifeExp %>% 
     filter((is.na(Code) | Entity == "World") & Entity != "Saint Barthlemy") %>% 
     select(Entity, Year, `Life expectancy (years)`) %>% 
     rename(Region = Entity, LE_age = `Life expectancy (years)`) %>% 
     mutate(Region = as.factor(Region))

# Select only 5 countries in maximum
country1 <- "Indonesia"
country2 <- "Malaysia"
country3 <- "Singapore"
country4 <- "Myanmar"
country5 <- "Thailand"

# subset by countries chosen
lifeExp_countries_chosen <- lifeExp_countries %>% 
     filter(Country == country1 | 
                 Country == country2 | 
                 Country == country3 | 
                 Country == country4 | 
                 Country == country5)

MenVSWomen <- as.data.frame(read_csv("life-expectancy-of-women-vs-life-expectancy-of-women.csv"))

MenVSWomen_countries <- MenVSWomen %>% 
   filter((!is.na(Code) | Entity == "Saint Barthlemy") & Entity != "World") %>% 
   rename(Country = Entity, 
          LE_men_age = `Life expectancy of men (years)`,
          LE_women_age = `Life expectancy of women (years)`,
          Total_pop = `Total population (Gapminder)`) %>% 
   mutate(Country = as.factor(Country),
          Code = as.factor(Code))
MenVSWomen_regions <- MenVSWomen %>% 
   filter((is.na(Code) | Entity == "World") & Entity != "Saint Barthlemy") %>%
   select(Entity, 
          Year, 
          `Life expectancy of men (years)`, 
          `Life expectancy of women (years)`, 
          `Total population (Gapminder)`) %>% 
   rename(Region = Entity, 
          LE_men_age = `Life expectancy of men (years)`,
          LE_women_age = `Life expectancy of women (years)`,
          Total_pop = `Total population (Gapminder)`) %>% 
   mutate(Region = as.factor(Region))