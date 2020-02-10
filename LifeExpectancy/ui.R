


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)


dashboardPage(
   skin = "black",
   dashboardHeader(title = "Life Expectancy"),
   dashboardSidebar(
      sidebarMenu(
         menuItem(text = "Life Expectancy",
                  tabName = "LE",
                  icon = icon("line-chart")),
         menuItem(text = "Comparisons",
                  tabName = "comp",
                  icon = icon("bar-chart")),
         menuItem(text = "Maps",
                  tabName = "map",
                  icon = icon("globe-asia")),
         menuItem(text = "Data",
                  tabName = "dat",
                  icon = icon("table")),
         menuItem(text = "More Info",
                  tabName = "info",
                  icon = icon("info"),
                  a("Data source", href = "https://ourworldindata.org/life-expectancy", target = "_blank"),
                  br(),
                  a("Source codes", href = "https://github.com/utomoreza/DV_Capstone", target = "_blank"),
                  br(),
                  a("About me", href = "https://id.linkedin.com/utomoreza", target = "_blank"))
      )
   ),
   dashboardBody(
      tabItems(
         tabItem(
            tabName = "LE",
            h2("Life Expectancy"),
            fluidPage(sidebarLayout(
               sidebarPanel(
                  tabsetPanel(
                     tabPanel("Select by country",
                              selectizeInput(
                                 'country1', 'Select first country', 
                                 choices = unique(lifeExp_countries$Country)
                              ),
                              selectizeInput(
                                 'country2', 'Select second country', 
                                 choices = unique(lifeExp_countries$Country)
                              ),
                              selectizeInput(
                                 'country3', 'Select third country', 
                                 choices = unique(lifeExp_countries$Country)
                              ),
                              selectizeInput(
                                 'country4', 'Select fourth country', 
                                 choices = unique(lifeExp_countries$Country)
                              ),
                              selectizeInput(
                                 'country5', 'Select fifth country', 
                                 choices = unique(lifeExp_countries$Country)
                              )
                     ),
                     tabPanel("Select by region",
                              checkboxInput("region1", sort(unique(lifeExp_regions$Region))[1], FALSE),
                              checkboxInput("region2", sort(unique(lifeExp_regions$Region))[2], FALSE),
                              checkboxInput("region3", sort(unique(lifeExp_regions$Region))[3], FALSE),
                              checkboxInput("region4", sort(unique(lifeExp_regions$Region))[4], FALSE),
                              checkboxInput("region5", sort(unique(lifeExp_regions$Region))[5], FALSE),
                              checkboxInput("region6", sort(unique(lifeExp_regions$Region))[6], FALSE),
                              checkboxInput("region7", sort(unique(lifeExp_regions$Region))[7], FALSE),
                              checkboxInput("region8", sort(unique(lifeExp_regions$Region))[8], FALSE)
                     )
                  )
               ),
               
               mainPanel(
                         fluidPage(
                            tabsetPanel(tabPanel("Life expectancy by country",
                                                 plotlyOutput("plot1a")), 
                                        tabPanel("Life expectancy by region", 
                                                 plotlyOutput("plot1b")))
                         )
               )
               
            )
            ),
            fluidPage(
               sidebarLayout(
                  sidebarPanel(width = 3, "Slide to choose year",
                               sliderTextInput(
                                  inputId = "slideMenWomen", 
                                  label = h4(tags$b("Year:")), 
                                  choices = sort(unique(MenVSWomen$Year))[189:254], 
                                  selected = c(1950,2015),
                                  from_fixed = T)
                  ),
                  mainPanel(width = 9,
                            fluidPage(
                               tabsetPanel(tabPanel("Life expectancy of men vs that of women by country",
                                                    plotlyOutput("plot2a")),
                                           tabPanel("Life expectancy of men vs that of women by region",
                                                    plotlyOutput("plot2b"))
                                           
                               )
                            )
                  )
               )
            )
         ),
         tabItem(
            tabName = "comp",
            h2("Comparisons")
         ),
         tabItem(
            tabName = "dat",
            fluidPage(
               tabsetPanel(
                  tabPanel("tab 1",
                           dataTableOutput(outputId = "dataRaw1")), 
                  tabPanel("tab 2", 
                           dataTableOutput(outputId = "dataRaw2"))
               )
            )
         )
      )
   )
)