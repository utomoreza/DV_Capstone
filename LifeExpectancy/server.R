#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {
   output$vBox1 <- renderValueBox({
      valueBox(
         format(
            max(
               (
                  MenVSWomen_regions[MenVSWomen_regions$Region == "World",]
               )
               $Total_pop),
            big.mark = ","), 
         "Today's world population", 
         icon = icon("globe-asia"), color = "blue")
   })
   
   output$vBox2 <- renderValueBox({
      
      subset_ <- MenVSWomen_regions %>% 
         filter(Region == "World" & Year == 2015)
      
      valueBox(round(subset_[1,3],2), 
         "Today's men's life expectancy", 
         icon = icon("male"), color = "green")
   })
   
   output$vBox3 <- renderValueBox({
      
      subset_ <- MenVSWomen_regions %>% 
         filter(Region == "World" & Year == 2015)
      
      valueBox(round(subset_[1,4],2), 
               "Today's women's life expectancy", 
               icon = icon("female"), color = "red")
   })
   
      output$plot1a <- renderPlotly({
         
         # Select only 5 countries in maximum
         country1 <- input$country1
         country2 <- input$country2
         country3 <- input$country3
         country4 <- input$country4
         country5 <- input$country5

         lifeExp_countries_chosen <- lifeExp_countries %>%
            filter(Country == country1 |
                      Country == country2 |
                      Country == country3 |
                      Country == country4 |
                      Country == country5)

         p1a <- ggplot(lifeExp_countries_chosen, aes(x = Year,
                                                     y = LE_age,
                                                     text = paste0(Country, "<br>",
                                                                   "Year: ", Year, "<br>",
                                                                   "Life expectancy: ", round(LE_age, 1)),
                                                     group = 1)) +
            geom_line(aes(color = Country)) +
            labs(title = "Life expectancy",
                 caption = "Source: ourworldindata.org") +
            scale_y_continuous(breaks = seq(from = 0, to = 90, by = 10),
                               labels = add_year) +
            theme_minimal() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.title = element_text(face = "bold", size = 18),
                  plot.caption = element_text(size = 7),
                  legend.title = element_blank())

         ggplotly(p1a, tooltip = "text")
         
      })
   
      output$plot1b <- renderPlotly({
         region1 <- ifelse(input$region1 == TRUE, yes = sort(as.character(unique(lifeExp_regions$Region)))[1], no = NA)
         region2 <- ifelse(input$region2 == TRUE, yes = sort(as.character(unique(lifeExp_regions$Region)))[2], no = NA)
         region3 <- ifelse(input$region3 == TRUE, yes = sort(as.character(unique(lifeExp_regions$Region)))[3], no = NA)
         region4 <- ifelse(input$region4 == TRUE, yes = sort(as.character(unique(lifeExp_regions$Region)))[4], no = NA)
         region5 <- ifelse(input$region5 == TRUE, yes = sort(as.character(unique(lifeExp_regions$Region)))[5], no = NA)
         region6 <- ifelse(input$region6 == TRUE, yes = sort(as.character(unique(lifeExp_regions$Region)))[6], no = NA)
         region7 <- ifelse(input$region7 == TRUE, yes = sort(as.character(unique(lifeExp_regions$Region)))[7], no = NA)
         region8 <- ifelse(input$region8 == TRUE, yes = sort(as.character(unique(lifeExp_regions$Region)))[8], no = NA)

         if (is.na(region1) &
             is.na(region2) &
             is.na(region3) &
             is.na(region4) &
             is.na(region5) &
             is.na(region6) &
             is.na(region7) &
             is.na(region8)) {
            lifeExp_regions_chosen <- lifeExp_regions %>% 
               filter(Region == "World")
         } else {
            lifeExp_regions_chosen <- lifeExp_regions %>% 
               filter(Region == region1 |
                         Region == region2 |
                         Region == region3 |
                         Region == region4 |
                         Region == region5 |
                         Region == region6 |
                         Region == region7 |
                         Region == region8)
         }
         
         p1b <- ggplot(lifeExp_regions_chosen, aes(x = Year,
                                         y = LE_age,
                                         text = paste0(Region, "<br>",
                                                       "Year: ", Year, "<br>",
                                                       "Life expectancy: ", round(LE_age, 1)),
                                         group = 1)) +
         geom_line(aes(color = Region)) +
         labs(title = "Life expectancy",
              caption = "Source: ourworldindata.org") +
         scale_y_continuous(breaks = seq(from = 0, to = 90, by = 10), 
                            labels = add_year) +
         theme_minimal() +
         theme(axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               plot.title = element_text(face = "bold", size = 18),
               plot.caption = element_text(size = 7),
               legend.title = element_blank(),
               legend.position = "top")
      
      ggplotly(p1b, tooltip = "text")
      
      })
      
   output$plot2a <- renderPlotly({
      # Choose year to plot
      sliceYear <- input$slideMenWomen
      
      MenVSWomen_countries_yearChosen <- MenVSWomen_countries %>% 
         filter(Year == sliceYear[2])
      
      p2a <- ggplot(MenVSWomen_countries_yearChosen, aes(x = LE_men_age,
                                                         y = LE_women_age,
                                                         size = Total_pop,
                                                         text = paste0(Country, "<br>",
                                                                       "Men: ", round(LE_men_age,2), "<br>",
                                                                       "Women: ", round(LE_women_age,2), "<br>",
                                                                       "Pop: ", format(Total_pop, big.mark = ",")))) +
         geom_jitter(aes(color = Country), alpha = 0.8) +
         labs(title = paste0("Men's vs women's life expectancy in ", sliceYear[2]),
              x = "Life expectancy of men",
              y = "Life expectancy of women") +
         theme_bw() +
         theme(legend.position = "none",
               plot.title = element_text(face = "bold", size = 13))
      
      ggplotly(p2a, tooltip = "text")
   })
   
   output$plot2b <- renderPlotly({
      sliceYear <- input$slideMenWomen
      
      MenVSWomen_regions_yearChosen <- MenVSWomen_regions %>% 
         filter(Year == sliceYear[2])
      
      p2b <- ggplot(MenVSWomen_regions_yearChosen, aes(x = LE_men_age,
                                                       y = LE_women_age,
                                                       size = Total_pop,
                                                       text = paste0(Region, "<br>",
                                                                     "Men: ", round(LE_men_age,2), "<br>",
                                                                     "Women: ", round(LE_women_age,2), "<br>",
                                                                     "Pop: ", format(Total_pop, big.mark = ",")))) +
         geom_jitter(aes(color = Region), alpha = 0.8) +
         scale_size_continuous(labels = comma) +
         labs(title = paste0("Men's vs women's life expectancy in ", sliceYear[2]),
              x = "Life expectancy of men",
              y = "Life expectancy of women") +
         theme_bw() +
         theme(legend.position = "none",
               plot.title = element_text(face = "bold", size = 13))
      
      ggplotly(p2b, tooltip = "text")
   })
   
   output$plot3a <- renderPlotly({
      
      MenVSWomen_countries_long <- MenVSWomen_countries %>% 
         pivot_longer(cols = c("LE_men_age","LE_women_age"),
                      names_to = "Gender", values_to = "LE_age") %>% 
         as.data.frame() %>% 
         filter(!is.na(LE_age))
      
      # filter(Country != "Wallis and Futuna")
      
      MenVSWomen_countries_long$Gender <- as.factor(sapply(as.character(MenVSWomen_countries_long$Gender), switch,
                                                           "LE_men_age" = "Men",
                                                           "LE_women_age" = "Women"))
      
      # Choose a country
      countryChosen <- input$selACountry
      
      dfLong_counChosen <- MenVSWomen_countries_long %>% 
         filter(Country == countryChosen) %>% 
         drop_na() %>% 
         as.data.frame() 
      
      p3a <- ggplot(dfLong_counChosen, aes(x = Year,
                                           y = ifelse(Gender == "Men",
                                                      LE_age, 
                                                      -LE_age),
                                           fill = Gender,
                                           text = paste0(Year, "<br>",
                                                         "Life exp: ", round(LE_age,2),
                                                         " years"))) +
         geom_col() +
         coord_flip() + 
         scale_fill_brewer(palette = "Set1") +
         scale_y_continuous(labels = abs,
                            limits = max(dfLong_counChosen$LE_age)*c(-1,1)) +
         labs(title = paste0("Life expectancy by gender in ", countryChosen),
              y = "Life expectancy",
              x = NULL) +
         theme_calc() +
         theme(plot.title = element_text(face = "bold", size = 9))
      
      ggplotly(p3a, tooltip = "text")  
   })
   
   output$plot3b <- renderPlotly({
      
      MenVSWomen_regions_long <- MenVSWomen_regions %>% 
         pivot_longer(cols = c("LE_men_age","LE_women_age"),
                      names_to = "Gender", values_to = "LE_age") %>% 
         as.data.frame()
      
      MenVSWomen_regions_long$Gender <- as.factor(sapply(
         as.character(MenVSWomen_regions_long$Gender), switch,
         "LE_men_age" = "Men",
         "LE_women_age" = "Women"))
      
      # Choose a region
      regionChosen <- input$selARegion
      
      dfLong_regChosen <- MenVSWomen_regions_long %>% 
         filter(Region == regionChosen) %>% 
         drop_na() %>% 
         as.data.frame()
      
      p3b <- ggplot(dfLong_regChosen, aes(x = Year,
                                          y = ifelse(Gender == "Men",
                                                     LE_age, 
                                                     -LE_age),
                                          fill = Gender,
                                          text = paste0(Year, "<br>",
                                                        "Life exp: ", round(LE_age,2),
                                                        " years"))) +
         geom_col() +
         coord_flip() + 
         scale_fill_brewer(palette = "Set1") +
         scale_y_continuous(labels = abs,
                            limits = max(dfLong_regChosen$LE_age)*c(-1,1)) +
         labs(title = paste0("Life expectancy by gender in ", regionChosen),
              y = "Life expectancy",
              x = NULL) +
         theme_calc() +
         theme(plot.title = element_text(face = "bold", size = 9))
      
      ggplotly(p3b, tooltip = "text") 
   })
   
   output$plot4 <- renderPlotly({
      options(scipen = 999)
      p4 <- ggplot(dfpopPercent, aes(x = Year,
                                     y = Total_pop,
                                     text = paste0(Region, "<br>",
                                                   "Year: ", Year, "<br>",
                                                   "Pops: ", format(Total_pop, big.mark = ",")),
                                     group = Region)) +
         geom_area(aes(fill = Region),
                   position = "fill",
                   color = "white",
                   size = 0.2) +
         labs(title = "World Population - Normalized",
              x = "Year",
              y = NULL) +
         scale_x_continuous(labels = comma) +
         scale_fill_brewer(palette = "Dark2") +
         scale_y_continuous(labels = percent) +
         theme_calc() +
         theme(plot.title = element_text(face = "bold", size = 15))
      
      ggplotly(p4, tooltip = "text")
   })
   
   output$plot5 <- renderPlotly({
      # Choose year to plot
      sliceYear <- input$slideCompare1
      
      DF_LEvsGDPCountries <- LEvsGDP_countrFull %>% 
         filter(Year == sliceYear[2])
      
      p5 <- ggplot(DF_LEvsGDPCountries, aes(x = LE_age,
                                            y = GDP,
                                            size = Total_pop,
                                            text = paste0(Country, "<br>",
                                                          "Life exp: ", round(LE_age,2), "<br>",
                                                          "GDP: $", format(GDP, big.mark = ","), "<br>",
                                                          "Pops: ", format(Total_pop, big.mark = ",")))) +
         geom_jitter(aes(color = Continent), alpha = 0.6) +
         labs(title = paste0("Life expectancy vs GDP per capita in ", sliceYear[2]),
              x = NULL,
              y = NULL) +
         scale_y_continuous(labels = dollar) +
         scale_x_continuous(labels = add_year) +
         scale_color_brewer(palette = "Set1") +
         theme_bw() +
         theme(legend.position = "none",
               plot.title = element_text(face = "bold", size = 13))
      
      ggplotly(p5, tooltip = "text")
   })
   
   output$plot6 <- renderPlotly({
      # Choose year to plot
      sliceYear <- input$slideCompare2
      
      DF_LEvsHealthCountries <- LEvsHealth_countrFull %>% 
         filter(Year == sliceYear[2])
      
      p6 <- ggplot(DF_LEvsHealthCountries, aes(x = LE_age,
                                               y = HealthExp,
                                               size = Total_pop,
                                               text = paste0(Country, "<br>",
                                                             "Life exp: ", round(LE_age,2), 
                                                             "<br>",
                                                             "Healthcare Exp: $", 
                                                             format(
                                                                round(
                                                                   HealthExp,2), 
                                                                big.mark = ","), 
                                                             "<br>",
                                                             "Pops: ", format(Total_pop, big.mark = ",")))) +
         geom_jitter(aes(color = Continent), alpha = 0.6) +
         labs(title = paste0("Life expectancy vs healthcare expenditure in ", sliceYear[2]),
              x = NULL,
              y = NULL) +
         scale_y_continuous(labels = dollar) +
         scale_x_continuous(labels = add_year) +
         scale_color_brewer(palette = "Set1") +
         theme_bw() +
         theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 13))
      
      ggplotly(p6, tooltip = "text")
   })
   
    output$dataRaw1 <- renderDataTable(
        datatable(lifeExp, options = list(scrollX = TRUE)))
    output$dataRaw2 <- renderDataTable(
        datatable(MenVSWomen, options = list(scrollX = TRUE)))
    output$dataRaw3 <- renderDataTable(
       datatable(LEvsGDP, options = list(scrollX = TRUE)))
    output$dataRaw4 <- renderDataTable(
       datatable(LEvsHealth, options = list(scrollX = TRUE)))
    
    output$toGithub <- renderUI({
       a("test", href="http://google.com", target="_blank")
    })
})
