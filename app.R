# THIS APPLICATION WARNS THE USER ABOUT DRASTIC CHANGES IN NOT JUST TEMPERATURE, BUT ALSO WIND SPEED, HUMIDITY AND PRESSURE AND THE
# USER CAN LOOK AT VARIOUS VISUALIZATIONS TO BETTER UNDERSTAND THE CALM BEFORE THE STORM, OR MAYBE JUST THE CALM OR JUST STORM,
# DEPENDING ON THE WEATHER

# THIS APPLICATION HAS BEEN DEVELOPED BY HYGIEIA, A PLATFORM FOR PREVENTING INFECTIONS AT THE WORKPLACE

# Initializing all packages

library(shinydashboard)
library(dplyr)
library(shiny)
library(shinyalert)
library(owmr)
library(ggplot2)
library(lubridate)
library(ggdark)
library(plotly)
library(tidyr)
library(shinyjs)

# UI side design

ui <- dashboardPage(skin = "blue", # Giving a blue header
                    
                    dashboardHeader(title = "Weather Forecast" # Dashboard title
                                    
                    ),
                    
                    dashboardSidebar(), # Initializing a sidebar
                    
                    dashboardBody( useShinyjs(), useShinyalert(), # Shiny alert for cool popups
                                   
                                   tags$head(tags$style(HTML('                       
                                                .main-header .logo {
                                                font-family: "Georgia", Times,
                                                "Times New Roman",
                                                font-weight: bold;
                                                font-size: 24px;
                                                font-style: italic;
                                                }
                                                '))), # A CSS template for the font size and font
                                   
                                   fluidRow(
                                       
                                       tabBox(height = "900px", width = "1000px",
                                              
                                              tabPanel(title = tagList(icon("project-diagram", class = "fas fa-project-diagram") # Creating the tabset panels
                                                                       
                                                                       , "FORECAST FOR THE NEXT 5 DAYS"),
                                                       
                                                       box(plotOutput("TempPlot"), status = "primary", solidHeader = TRUE, # Temperature plot
                                                           
                                                           title = "Temp VS Feels Like", width = 8, height = 512, collapsible = TRUE),
                                                       
                                                       box(tableOutput("CurrTemp"), status = "primary", # Small boxes to show current weather attributes
                                                           
                                                           title = "Current Temperature (C)", width = 3, height = 85, collapsible = TRUE),
                                                       
                                                       box(tableOutput("CurrFeelsLike"), status = "primary", # Small boxes to show current weather attributes
                                                           
                                                           title = "Feels Like Currently (C)", width = 3, height = 85,
                                                           
                                                           collapsible = TRUE),
                                                       
                                                       box(tableOutput("CurrHumidity"), status = "primary", # Small boxes to show current weather attributes
                                                           
                                                           title = "Current Humidity (%)", width = 3, height = 85, collapsible = TRUE),
                                                       
                                                       box(tableOutput("CurrWind"), status = "primary", # Small boxes to show current weather attributes
                                                           
                                                           title = "Current Wind Speed (km/h)", width = 3, height = 85, collapsible = TRUE),
                                                       
                                                       box(tableOutput("CurrWeatherDesc"), status = "primary", # Small boxes to show current weather attributes
                                                           
                                                           title = "Current Weather Description", width = 3, height = 85, collapsible = TRUE),
                                                       
                                                       box(tableOutput("CurrPressure"), status = "primary", # Small boxes to show current weather attributes
                                                           
                                                           title = "Current Pressure (Pascal)", width = 3, height = 85, collapsible = TRUE)
                                              ),
                                              
                                              
                                              tabPanel(title = tagList(icon("th-list", lib = "glyphicon"), "3-h 5-day Forecast"),
                                                       
                                                       DT::dataTableOutput("ForecastTable") # Data table for the 3-h 5-day forecast
                                              ),
                                              
                                              tabPanel(title = tagList(icon("Wind", class = "fas fa-wind"), "Wind Speed Forecast"), # Another tabset panel for wind
                                                       
                                                       box(plotOutput("Wind"), status = "primary", solidHeader = TRUE,
                                                           
                                                           title = "WIND", width = 8, height = 512, collapsible = TRUE)
                                              ),
                                              
                                              tabPanel(title = tagList(icon("Wind", class = "fas fa-wind"), "Humidity Forecast"), # Another tabset panel for humidity
                                                       
                                                       box(plotOutput("Humidity"), status = "primary", solidHeader = TRUE,
                                                           
                                                           title = "HUMIDITY", width = 8, height = 512, collapsible = TRUE)
                                              ),
                                              
                                              tabPanel(title = tagList(icon("Wind", class = "fas fa-wind"), "Pressure Forecast"), # Another tabset panel for pressure
                                                       
                                                       box(plotOutput("Pressure"), status = "primary", solidHeader = TRUE,
                                                           
                                                           title = "PRESSURE", width = 8, height = 512, collapsible = TRUE)
                                              )
                                              
                                       )
                                   )
                    )) 

# Server side design

server <- function(input, output) {
    
    addClass(selector = "body", class = "sidebar-collapse") # Adding a JS class to automatically collapse the sidebar
    
    shinyalert(title = "WELCOME TO OUR WEATHER API!", type = "info", showConfirmButton = TRUE, confirmButtonText = "GOT IT!", 
               text = "You will now be able to receive warnings about 
             major temperature fluctuations and see live changes in wind speeds, humidity and pressure!") # A pop-up alert to let the user know what the application is about
    
    owmr_settings("f9c36c6269d09b203c108d7c370fda10") # Setting API Access token
    
    Sys.setenv(OWM_API_KEY = "f9c36c6269d09b203c108d7c370fda10") # Storing Access token in R environment
    
    (weather <- get_current(2158177, units = "metric") %>%
            owmr_as_tibble()) %>% names() # Fetching current weather data
    
    melbs <- get_forecast(2158177, units = "metric") %>% owmr_as_tibble() # Fetching forecasted weather data fro the next 5 days 
    
    melbs_forecast <- melbs %>% mutate(date = lubridate::date(dt_txt)) %>% group_by(date) %>% 
        summarise(temp_avg = mean(temp), feels_like_avg = mean(feels_like), humidity_avg = mean(humidity), pressure_avg = mean(pressure),
                  wind_speed_avg = mean(wind_speed)) # Creating a dataframe with average temperatures, humidity, pressure etc.
    
    
    output$TempPlot <- renderPlot({
        
        melbs_forecast_tidy <- melbs_forecast %>% select(date, temp_avg, feels_like_avg) %>% 
            gather(key = "Temperature", value = "value", -date)
        
        melbs_forecast_tidy_plot <- melbs_forecast_tidy %>% ggplot(aes(x = date, y = value, label = round(value, 2))) + 
            geom_line(aes(color = Temperature, linetype = Temperature)) + 
            scale_color_manual(values = c("blue", "red")) + 
            dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
            geom_point(col = "#E4F00A") + 
            geom_text(hjust = 0.3, vjust = 1.3) +
            ggtitle("Avg. Temperature vs Avg. Feels Like over the next 5 days") +
            xlab("Date") + ylab("Temperature in Degree Celsius") # side-by-side line chart for the forecasted temperature vs the forecasted feels like over the next 5 days
        
        melbs_forecast_tidy_plot # Plotting the ggplot object
        
    })
    
    output$CurrHumidity <- renderText({
        
        weather$humidity # Returning current humidity
        
    })
    
    
    output$ForecastTable <- DT::renderDataTable({
        
        melbs_selected <- melbs %>% select(c("dt_txt", "temp", "pressure", "humidity", "weather_description", "wind_speed", "feels_like"))
        
        names(melbs_selected) <- c("Datetime", "Temperature", "Pressure", "Humidity", "Weather Description", "Wind Speed", "Feels Like") # Changing default dataframe columns names
        
        melbs_selected # Returning the dataframe
        
    })
    
    output$CurrTemp <- renderText({
        
        weather$temp # Returning current temperature
        
    })
    
    output$CurrPressure <- renderText({
        
        weather$pressure # Returning current pressure
        
    })
    
    output$CurrWind <- renderText({
        
        weather$wind_speed # Returning current wind speed
        
    })
    
    output$CurrFeelsLike <- renderText({
        
        weather$feels_like # Returning current feels like temperature
        
    })
    
    output$CurrWeatherDesc <- renderText({
        
        toupper(weather$weather_description) # Returning current weather description in upper case
        
    })
    
    output$Wind <- renderPlot({
        
        melbs_forecast_tidy <- melbs_forecast %>% select(date, wind_speed_avg) %>% 
            gather(key = "Wind Speed", value = "value", -date) # Transforming wind speed data to plot as a line chart
        
        melbs_forecast_tidy %>% ggplot(aes(x = date, y = value, label = round(value, 2))) + 
            geom_line(aes(color = 'Wind Speed')) + 
            scale_color_manual(values = c("orange")) + 
            dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
            geom_point(col = "#E4F00A") + 
            geom_text(hjust = 0.3, vjust = 1.3) +
            ggtitle("Avg. Wind Speed over the next 5 days") +
            xlab("Date") + ylab("Wind Speed in km/h") # ggplot implementation for the line chart with a dark theme, line and points
        
    })
    
    output$Humidity <- renderPlot({
        
        melbs_forecast_tidy <- melbs_forecast %>% select(date, humidity_avg) %>% 
            gather(key = "Humidity", value = "value", -date) # Transforming humidity data to plot as a line chart
        
        melbs_forecast_tidy %>% ggplot(aes(x = date, y = value, label = round(value, 2))) + 
            geom_line(aes(color = 'Humidity')) + 
            scale_color_manual(values = c("red")) + 
            dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
            geom_point(col = "#E4F00A") + 
            geom_text(hjust = 0.3, vjust = 1.3) +
            ggtitle("Avg. Humidity over the next 5 days") +
            xlab("Date") + ylab("Humidity in %") # ggplot implementation for the line chart with a dark theme, line and points
        
    })
    
    output$Pressure <- renderPlot({
        
        melbs_forecast_tidy <- melbs_forecast %>% select(date, pressure_avg) %>% 
            gather(key = "Pressure", value = "value", -date) # Transforming pressure data to plot as a line chart
        
        melbs_forecast_tidy %>% ggplot(aes(x = date, y = value, label = round(value, 2))) + 
            geom_line(aes(color = 'Pressure')) + 
            scale_color_manual(values = c("blue")) + 
            dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
            geom_point(col = "#E4F00A") + 
            geom_text(hjust = 0.3, vjust = 1.3) +
            ggtitle("Avg. Pressure over the next 5 days") +
            xlab("Date") + ylab("Pressure in Pascals") # ggplot implementation for the line chart with a dark theme, line and points
        
    })
    
    for(i in 1:nrow(melbs_forecast)) { # Running a loop to get the emperature difference on consecutive days
        
        melbs_forecast$temp_diff_avg[i] <- melbs_forecast$temp_avg[i] - melbs_forecast$temp_avg[i+1] # Storing the data as a new column
        
    }
    
    melbs_forecast$temp_diff_avg[is.na(melbs_forecast$temp_diff_avg)] <- 0 # Converting NA values to 0 for simplicity
    
    for(i in 1:6) { # Running a loop to check if there are any major temperature fluctuations
        
        if(melbs_forecast$temp_diff_avg[i] > 3 | melbs_forecast$temp_diff_avg[i] < -3) { # Checking if there is a difference of more than 3 degrees, both ways
            
            Sys.sleep(7) # Sleeping time to give the first Shinyalert popup 7 seconds so it can be viewed by the user
            
            textTemp <- paste("There will be a temperature fluctuation of", paste(melbs_forecast$temp_diff_avg[i], 
                                                                                  paste("degrees", paste("on", melbs_forecast$date[i+1])))) # Making a custom modal message for temperature fluctuations
            
            shinyalert(title = "Watch out!", text = textTemp, type = "warning") # Enabling Shinyalert to do what it does best, popup!
        }
    }
    
} # Closing the server side design

shinyApp(ui = ui, server = server)