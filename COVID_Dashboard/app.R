library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(leaflet)
library(shinyWidgets)
library(sf)


theme_set(theme_bw())


##############################
# Data Import
##############################

ts_conf <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = F))
ts_deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", stringsAsFactors = F))



##############################
# Data wrangling
##############################

#Renaming the "Province/State" and "Country/Region" columns to State and Country respectively
colnames(ts_conf)[1] <- "State"
colnames(ts_conf)[2] <- "Country"
colnames(ts_deaths)[1] <- "State"
colnames(ts_deaths)[2] <- "Country"
#gather the time series data confirmed
ts_conf_gathered <- gather(ts_conf, key = "date", value = "cases", -c("State", "Country", "Lat", "Long"))
#gather time series data deaths
ts_deaths_gathered <- gather(ts_deaths, key = "date", value = "deaths", -c("State", "Country", "Lat", "Long"))
#join cases and deaths
data <- left_join(ts_conf_gathered, ts_deaths_gathered)


#fix date
data$date <- mdy(data$date)
#Remove overseas territories
data <- data[data$State == "" | data$Country=="China" & data$State=="Hubei",]
#Make list of countries that have more than 8000 cases
high <- unique(data$Country[data$cases>5000])

###Make new column with daily cases
data <- data %>% 
    group_by(Country) %>% 
    mutate(daily_cases = cases - lag(cases)) %>% 
    ungroup()
#if daily cases is 0 on any given day where the next day is over 1000 take the next day and distribute over two days
data <- data %>% 
    group_by(Country) %>% 
    mutate(daily_cases = 
               case_when(daily_cases == 0 & lead(daily_cases)>1000 ~ lead(daily_cases)/2,
                         daily_cases > 1000 & lag(daily_cases) == 0 ~ daily_cases/2,
                         TRUE ~ as.numeric(daily_cases))) %>%
    ungroup()
    
    
###Make new column with daily deaths
data <- data %>% 
    group_by(Country) %>% 
    mutate(daily_deaths = deaths - lag(deaths)) %>% 
    ungroup()




##############################
#UI
##############################


ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("Confirmed Cases", tabName = "confirmed", icon = icon("chart-line")),
        menuItem("Interactive Map", tabName = "map", icon = icon("map-marked"))
    )),
    dashboardBody(
        tabItems(
            
            
            
            
            
            ### First tab content (Plot)
            tabItem(tabName = "confirmed", #the tab name needs to be one word
                    fluidRow(
                        box(width = 9, plotOutput("plot1")),
                        box(width = 3, title = "Controls",
                            dropdownButton(label = "Select countries", circle = FALSE,
                                           checkboxGroupInput(inputId = "country", 
                                                              label = "Select Country", 
                                                              choices = high,
                                                              selected = "Austria")),
                            br(),
                            selectInput(inputId = "scale", label = "Scale", 
                                        choices = c("linear", "log")),
                            selectInput(inputId = "cumul_daily", label = "Cumulative/Daily", 
                                        choices = c("cumulative", "daily")),
                            selectInput(inputId = "cases_deaths", label = "Cases/Deaths", 
                                        choices = c("cases", "deaths")),
                            sliderInput(inputId = "startdate", label = "Select Dates:",
                                        min = as.Date("2020-01-22","%Y-%m-%d"),
                                        max = Sys.Date() - 1,
                                        value=c(as.Date("2020-01-22"), as.Date(Sys.Date() - 1)),
                                        timeFormat="%Y-%m-%d")
                            )
                        )
                    ),
            
            
            
            
            
            ### Second Tab content (Interactive Map)
            tabItem(tabName = "map",
                    fluidRow(
                        leafletOutput("map")
                        ),
                    fluidRow(
                        sliderInput(inputId = "leafletdate", label = "Select Date:", width = "100%",
                                    min = as.Date("2020-01-22","%Y-%m-%d"),
                                    max = Sys.Date() - 1,
                                    value=as.Date("2020-01-22"),
                                    timeFormat="%Y-%m-%d",
                                    animate = TRUE), style = "padding: 1px 20px 1px 20px")) #top, right, bottom, left
            )
        )
    )



##############################
#Server
##############################

server <- function(input, output) {
### Make a reactive value which is a string that changes depending on inputs. This can be used with ggplot and aes_string().
    data1 <- reactive({
        if(input$cases_deaths == "deaths" & input$cumul_daily == "daily"){"daily_deaths"}
        else if(input$cases_deaths == "cases" & input$cumul_daily == "cumulative"){"cases"}
        else if(input$cases_deaths == "cases" & input$cumul_daily == "daily"){"daily_cases"}
        else if(input$cases_deaths == "deaths" & input$cumul_daily == "cumulative"){"deaths"}
        else {"cases"}
    })
    
    
    
    
    ### Plot displaying cumulative confirmed COVID cases by country
    output$plot1 <- renderPlot({ 
        plot1 <-  ggplot() +
            geom_line(data=filter(data, Country %in% input$country), 
                      aes_string(x="date", y=data1(), group="Country", colour="Country")) +
            labs(title = str_c("COVID19 ", input$cases_deaths, " by country")) +
            scale_x_date(limits = c(input$startdate)) +
            theme(legend.position="bottom")
        #add log scale if log is selected in the Scale input
        ifelse(input$scale == "log", return(plot1 + scale_y_log10()), return(plot1))
    })
    
    
    
    
    ###Leaflet Map displaying COVID cases
    output$map <- renderLeaflet({ 
        leaflet(filter(data, date==max(data$date))) %>% 
            addTiles() %>% 
            setView(0,0, zoom = 1.5)
    })
    
    observeEvent(input$leafletdate, {
        leafletProxy("map", data = filter(data, date==max(data$date))) %>%
            clearMarkers() %>%
            addCircleMarkers(radius = filter(data, date==input$leafletdate)$cases/5000,
                             stroke = FALSE,
                             color = "red")
            
    })
}

shinyApp(ui, server)













##############################
#unused code
##############################
#
#ggplot map (was to slow to render)
#########################################
#library(rnaturalearth)
#library(rnaturalearthdata)
#library(ggforce)
#
#
#
# #Server Code:
# ### ggplot map
#output$staticmap <- renderPlot({ 
#    #make world map
#    world <- ne_countries(scale = "medium", returnclass = "sf")
#    #plot map
#    ggplot() +
#        geom_sf(data=world) +
#        coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
#        geom_circle(data = filter(data, date==input$mapdate), 
#                    aes(x0 = Long, y0 = Lat, r = 6*(log(cases)/log(max(cases)))), color = "red", alpha = 0.5, show#.legend = FALSE) +
#        coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
#        scale_radius(limits = c(0,log(max(data$cases))))
#})
#
#
#
# # UI Code:
#            ### Third Tab content (Static Map)
#tabItem(tabName = "staticmap",
#        fluidRow(
#            plotOutput("staticmap")),
#        fluidRow(
#            sliderInput(inputId = "mapdate", label = "Select Date:", width = "100%",
#                        min = as.Date("2020-01-22","%Y-%m-%d"),
#                        max = Sys.Date() - 1,
#                        value=as.Date("2020-01-22"),
#                        timeFormat="%Y-%m-%d")))
#
