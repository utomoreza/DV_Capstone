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
            menuItem(text = "Get Codes",
                     tabName = "code",
                     icon = icon("code"),
                     href = )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "LE",
                h2("Life Expectancy")
            ),
            tabItem(
                tabName = "comp",
                h2("Comparisons")
            ),
            tabItem(
                tabName = "dat",
                fluidPage(tabsetPanel(tabPanel("tab 1",
                                               dataTableOutput(outputId = "dataRaw1")), 
                                       tabPanel("tab 2", 
                                                dataTableOutput(outputId = "dataRaw2"))
                                      )
                          )
            )
        )
    )
)