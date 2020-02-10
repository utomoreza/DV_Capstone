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
   
    output$dataRaw1 <- renderDataTable(
        datatable(lifeExp, options = list(scrollX = TRUE)))
    output$dataRaw2 <- renderDataTable(
        datatable(MenVSWomen, options = list(scrollX = TRUE)))
    
    output$toGithub <- renderUI({
       a("test", href="http://google.com", target="_blank")
    })
})
