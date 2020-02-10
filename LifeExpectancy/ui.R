


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
         # menuItem(text = "Maps",
         #          tabName = "map",
         #          icon = icon("globe-asia")),
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
                  a("About me", href = "https://www.linkedin.com/in/utomorezadwi/", target = "_blank"))
      )
   ),
   dashboardBody(
      tabItems(
         tabItem(
            tabName = "LE",
            h2("Life Expectancy"),
            fluidRow(
               valueBoxOutput("vBox1") %>% withSpinner(),
               valueBoxOutput("vBox2") %>% withSpinner(),
               valueBoxOutput("vBox3") %>% withSpinner()
            ),
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
                                          plotlyOutput("plot1a")  %>% withSpinner()), 
                                 tabPanel("Life expectancy by region", 
                                          plotlyOutput("plot1b") %>% withSpinner()))
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
                                  from_fixed = T),
                               fluidPage(
                                  p("In every country the life expectancy of women is higher than
                                  the life expectancy of men as next to chart shows. Why this is the 
                                    case is answered by Esteban Ortiz-Ospina in his text:",
                                     align = "justify"),
                                  a(href="https://ourworldindata.org/why-do-women-live-longer-than-men", 
                                    "Why do women live longer than men?", target = "_blank")
                               )
                  ),
                  mainPanel(width = 9,
                            fluidPage(
                               tabsetPanel(tabPanel("Life expectancy of men vs that of women by country",
                                                    plotlyOutput("plot2a") %>% withSpinner()),
                                           tabPanel("Life expectancy of men vs that of women by region",
                                                    plotlyOutput("plot2b") %>% withSpinner())
                                           
                               )
                            )
                  )
               )
            ),
            fluidPage(
               fluidRow(column(width = 3,
                               selectInput("selACountry", "Select a country",
                                           choices = unique(MenVSWomen_countries_long$Country))),
                        column(width = 3, offset = 3,
                               selectInput("selARegion", "Select a region",
                                           choices = c("Africa", "Asia", "Europe", "Oceania", "World")
                                           )
                               )
               ),
               
               fluidRow(column(width = 12,
                               splitLayout(box(color = "black", width = 12,
                                               plotlyOutput("plot3a") %>% withSpinner()
                               ),
                               box(color = "black", width = 12,
                                   plotlyOutput("plot3b") %>% withSpinner()
                               )
                               )
               )
               )
            )
            
         ),
         tabItem(
            tabName = "comp",
            h2("Comparisons"),
               fluidPage(
                  fluidRow(
                     h3("Life Expectancy VS GDP and Healthcare Expenditure",
                        align = "center"),
                     p("Life expectancy will be compared to GDP per capita and healthcare expenditure. 
                       Each figure below shows the relationship between each of them.", 
                       align = "justify")
                  ),
                  fluidRow(
                     box("Input for life expectancy vs GDP", solidHeader = T,
                         width = 4, status = "primary",
                         sliderTextInput(
                            inputId = "slideCompare1", 
                            label = "Slide to choose year", 
                            choices = sort(unique(LEvsGDP_countrFull$Year)), 
                            selected = c(1703,2015),
                            from_fixed = T),
                         p("(ourworldindata.org) This graph displays the correlation between life expectancy and gross
                           domestic product (GDP) per capita. It shows that In general, countries
                           with higher GDP tend to have a higher life expectancy. It is a
                           logarithmic relationship: the difference in life expectancy per
                           difference in GDP per capita is higher for poorer than for richer
                           countries.", align = "justify"),
                         a("Read more ...", href = "https://ourworldindata.org/life-expectancy#life-expectancy-and-gdp",
                           target = "_blank")
                     ),
                     box(title = "How strong is the link between GDP per capita and life expectancy?", solidHeader = T, 
                         width = 8, status = "info",
                         plotlyOutput("plot5") %>% withSpinner()
                     )
                  ),
                  fluidRow(
                     box("Input for life expectancy vs healthcare", solidHeader = T,
                         width = 4, status = "primary",
                         sliderTextInput(
                            inputId = "slideCompare2", 
                            label = "Slide to choose year", 
                            choices = sort(unique(LEvsHealth_countrFull$Year)), 
                            selected = c(1995,2014),
                            from_fixed = T),
                         p("(ourworldindata.org) One of the most important inputs to health is healthcare. Here we
                         study cross-country evidence of the link between aggregate healthcare
                         consumption and production, and health outcomes. One common way of
                           measuring national healthcare consumption and production is to 
                           estimate aggregate expenditure on healthcare (typically expressed as
                           a share of national income).", align = "justify"),
                         a("Read more ...", href = "https://ourworldindata.org/life-expectancy#how-strong-is-the-link-between-healthcare-expenditure-and-life-expectancy",
                           target = "_blank")
                     ),
                     box(title = "How strong is the link between healthcare expenditure and life expectancy?", 
                         solidHeader = T, width = 8, status = "info",
                         plotlyOutput("plot6") %>% withSpinner()
                     )
                  )
               )
         ),
         tabItem(
            tabName = "dat",
            fluidPage(
               tabsetPanel(
                  tabPanel("Life expectancy",
                           dataTableOutput(outputId = "dataRaw1") %>% withSpinner()), 
                  tabPanel("Men vs women", 
                           dataTableOutput(outputId = "dataRaw2") %>% withSpinner()),
                  tabPanel("VS GDP",
                           dataTableOutput(outputId = "dataRaw3") %>% withSpinner()),
                  tabPanel("VS healthcare",
                           dataTableOutput(outputId = "dataRaw4") %>% withSpinner())
               )
            )
         )
      )
   )
)