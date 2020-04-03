library(shiny)
library(shinydashboard)


## Data wrangling
library(tidyverse)
library(lubridate)
ts_conf <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = F))
#Renaming the "Province/State" and "Country.Region" columns to State and Country respectively
colnames(ts_conf)[1] <- "State"
colnames(ts_conf)[2] <- "Country"
#gather the time series data
ts_conf_gathered <- gather(ts_conf, key = "date", value = "cases", -c("State", "Country", "Lat", "Long"))
#fix date
ts_conf_gathered$date <- mdy(ts_conf_gathered$date)
#Remove overseas territories
ts_conf_gathered <- ts_conf_gathered[ts_conf_gathered$State == "",]
#Make list of countries that have more than 8000 cases
high <- unique(ts_conf_gathered$Country[ts_conf_gathered$cases>8000])







ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("Confirmed Cases", tabName = "confirmed", icon = icon("chart-line")),
        menuItem("Map", tabName = "map", icon = icon("map-marked"))
    )),
    dashboardBody(
        tabItems(
            ### First tab content
            tabItem(tabName = "confirmed", #the tab name needs to be one word
                    fluidRow(
                        box(width = 9, plotOutput("plot")),
                        box(width = 3, title = "Controls",
                            checkboxGroupInput(inputId = "country", label = "Select Country", choices = high),
                            selectInput(inputId = "scale", label = "Scale", choices = c("linear", "log"))
                            )
                        )
                    ),
            ### Second Tab content
            tabItem(tabName = "map",
                    h2("Map of Cases"))
            )
        )
    )

server <- function(input, output) {
    output$plot <- renderPlot({
        ## Plot displaying cumulative confirmed COVID cases by country
        mygg <-  ggplot() +
            geom_line(data=filter(ts_conf_gathered, Country %in% input$country), aes(x=date, y=cases, group=Country, colour=Country)) +
            labs(title = "Confirmed COVID19 cases by country")
        #add log scale if log is selected in the Scale input
        ifelse(input$scale == "log", return(mygg + scale_y_log10()), return(mygg))
    })
}

shinyApp(ui, server)