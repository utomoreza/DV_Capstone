library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggthemes)
library(plotly)
library(scales)
library(DT)
library(shinycssloaders)
library(leaflet)
options(shiny.maxRequestSize=200*1024^2)

# set loading spinner color
options(spinner.color = "blue")

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

MenVSWomen_countries_long <- MenVSWomen_countries %>% 
   pivot_longer(cols = c("LE_men_age","LE_women_age"),
                names_to = "Gender", values_to = "LE_age") %>% 
   as.data.frame() %>% 
   filter(!is.na(LE_age))

   # drop_na()
   # filter(Country != "Wallis and Futuna")

MenVSWomen_countries_long$Gender <- as.factor(sapply(as.character(MenVSWomen_countries_long$Gender), switch,
                                                     "LE_men_age" = "Men",
                                                     "LE_women_age" = "Women"))

MenVSWomen_regions_long <- MenVSWomen_regions %>% 
   pivot_longer(cols = c("LE_men_age","LE_women_age"),
                names_to = "Gender", values_to = "LE_age") %>% 
   as.data.frame()

MenVSWomen_regions_long$Gender <- as.factor(sapply(
   as.character(MenVSWomen_regions_long$Gender), switch,
   "LE_men_age" = "Men",
   "LE_women_age" = "Women"))

df_pop <- MenVSWomen_regions %>% 
   filter(Region != "World" & Region != "Saint Barthlemy") %>% 
   select(Region, Year, Total_pop) %>% 
   drop_na() %>% 
   mutate(Year = as.factor(Year))

Total <- df_pop %>% 
   group_by(Year) %>% 
   summarise(sum_Year = sum(Total_pop)) %>% 
   ungroup()

dfpopPercent <- left_join(df_pop, Total, by = c("Year" = "Year")) %>% 
   mutate(Ratio = Total_pop/sum_Year*100,
          Year = as.numeric.factor(Year)) %>% 
   select(Region, Year, Total_pop, Ratio)

LEvsGDP <- as.data.frame(read_csv("life-expectancy-vs-gdp-per-capita.csv"))

LEvsGDP_countries <- LEvsGDP %>% 
   filter(!is.na(Code) & Entity != "World") %>% 
   rename(Country = Entity, 
          LE_age = `Life expectancy at birth (years)`,
          GDP = `GDP per capita ($)`,
          Total_pop = `Population by country`) %>% 
   mutate(Country = as.factor(Country),
          Code = as.factor(Code))
LEvsGDP_regions <- LEvsGDP %>% 
   filter(is.na(Code) | Entity == "World") %>% 
   select(Entity, 
          Year, 
          `Life expectancy at birth (years)`,
          `GDP per capita ($)`,
          `Population by country`) %>% 
   rename(Region = Entity, 
          LE_age = `Life expectancy at birth (years)`,
          GDP = `GDP per capita ($)`,
          Total_pop = `Population by country`) %>% 
   mutate(Region = as.factor(Region))

LEvsGDP_countries_ <- LEvsGDP_countries %>% 
   mutate(Country = as.character.factor(Country))
LEvsGDP_countries_$Country <- str_replace_all(LEvsGDP_countries_$Country, 
                                              "Brunei", 
                                              "Brunei Darussalam")
LEvsGDP_countries_$Country <- str_replace_all(LEvsGDP_countries_$Country, 
                                              "Macao", 
                                              "Macau")
LEvsGDP_countries_$Country <- str_replace_all(LEvsGDP_countries_$Country, 
                                              "^Congo$", 
                                              "Democratic Republic of Congo")
LEvsGDP_countries_$Country <- str_replace_all(LEvsGDP_countries_$Country, 
                                              "^Micronesia \\(country\\)$", 
                                              "Federated States of Micronesia")
LEvsGDP_countries_$Country <- str_replace_all(LEvsGDP_countries_$Country, 
                                              "Timor", 
                                              "Timor-Leste")
LEvsGDP_countries_$Country <- str_replace_all(LEvsGDP_countries_$Country, 
                                              "^Polynesia$", 
                                              "French Polynesia")

LEvsGDP_countrFull <- left_join(LEvsGDP_countries_,
                                countries_,
                                by = c("Country" = "Country")) %>% 
   mutate(Country = as.factor(Country)) %>% 
   filter(Country != "Channel Islands" & 
             Country != "French Guiana" &
             Country != "Macacu" &
             Country != "Melanesia" &
             Country != "Reunion" &
             Country != "Curacao" &
             Country != "Martinique" &
             Country != "Guadeloupe" &
             Country != "Mayotte" &
             Country != "United States Virgin Islands")

LEvsGDP_countrFull[9980:10073,"Capital"] <- "Prague"
LEvsGDP_countrFull[9980:10073,"Continent"] <- "Europe"
LEvsGDP_countrFull[9980:10073,"Latitude"] <- 49.81749
LEvsGDP_countrFull[9980:10073,"Longitude"] <- 15.47296

LEvsGDP_countrFull[44622:44841,"Capital"] <- "Vatican"
LEvsGDP_countrFull[44622:44841,"Continent"] <- "Europe"
LEvsGDP_countrFull[44622:44841,"Latitude"] <- 41.87194
LEvsGDP_countrFull[44622:44841,"Longitude"] <- 12.56738

LEvsGDP_countrFull <- LEvsGDP_countrFull %>% 
   filter(!is.na(LE_age) & !is.na(GDP))

LEvsHealth <- as.data.frame(read_csv("life-expectancy-vs-healthcare-expenditure.csv", na = ""))
LEvsHealth <- LEvsHealth %>% 
   rename(Country = Entity,
          LE_age = `Life expectancy at birth (years)`,
          HealthExp = `Healthcare Expenditure per capita (int.-$) (constant 2011 international $)`,
          Total_pop = X6) %>% 
   filter(Year >= 1995 & !is.na(Code)) %>% 
   filter(Country != "Channel Islands" & 
             Country != "French Guiana" &
             Country != "Macacu" &
             Country != "Melanesia" &
             Country != "Reunion" &
             Country != "Curacao" &
             Country != "Martinique" &
             Country != "Guadeloupe" &
             Country != "Mayotte" &
             Country != "United States Virgin Islands")

LEvsHealth$Country <- str_replace_all(LEvsHealth$Country, 
                                      "Brunei", 
                                      "Brunei Darussalam")
LEvsHealth$Country <- str_replace_all(LEvsHealth$Country, 
                                      "Macao", 
                                      "Macau")
LEvsHealth$Country <- str_replace_all(LEvsHealth$Country, 
                                      "^Congo$", 
                                      "Democratic Republic of Congo")
LEvsHealth$Country <- str_replace_all(LEvsHealth$Country, 
                                      "^Micronesia \\(country\\)$", 
                                      "Federated States of Micronesia")
LEvsHealth$Country <- str_replace_all(LEvsHealth$Country, 
                                      "Timor", 
                                      "Timor-Leste")
LEvsHealth$Country <- str_replace_all(LEvsHealth$Country, 
                                      "^Polynesia$", 
                                      "French Polynesia")

LEvsHealth_countrFull <- left_join(LEvsHealth,
                                   countries_,
                                   by = c("Country" = "Country")) %>% 
   filter(!is.na(Continent) | Country == "World") %>% 
   mutate(Continent = as.character.factor(Continent))

idx <- as.numeric(rownames(LEvsHealth_countrFull[LEvsHealth_countrFull$Country == "World",]))
LEvsHealth_countrFull[idx,"Continent"] <- "World"

LEvsHealth_countrFull$Continent <- as.factor(LEvsHealth_countrFull$Continent)

LEvsHealth_countrFull <- LEvsHealth_countrFull %>% 
   filter(!is.na(LE_age) & !is.na(HealthExp))
