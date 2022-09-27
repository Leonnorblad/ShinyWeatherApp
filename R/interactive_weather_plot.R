#' Weather forecast application
#' 
#' A Shiny application based on get_weather_forecast() from the Weather4cat package
#' 
#' @import ggplot2 lubridate stringr shiny Weather4cats
#'
#' @return A shiny application
#' 
#' @export

interactive_weather_plot <- function(){
  
  # Function to render plot
  render_plot <- function(city, date, W_variable){
    # Creates global variables
    time <- Time <- Temperature <- Humidity <- Precipitation <- Cloudcover <- Wind_speed <- Wind_direction <- City <- NULL
    # API data
    data <- get_weather_forecast(city = city, date=date)
    
    # Transform date object
    date <- as.POSIXlt(gsub("T", " ", data$Time))
    
    # Time in plot = hour
    data["time"] <- strftime(date, format="%H")
    
    # Get the date of the day
    date_of_day <- strftime(date, format="%Y-%m-%d")
    
    # Construct the title 
    title <- paste0(W_variable, ", ", data$City[1])
    
    # construct the subtitle
    subtitle <- paste0(day(date_of_day)," ", str_to_title(month(date_of_day, label = TRUE, abbr=FALSE), ","), " ", year(date_of_day))[1]
    
    # Line plots
    if (any(c("Temperature"==W_variable,
              "Humidity"==W_variable,
              "Cloudcover"==W_variable))){
      plot <- ggplot(data=data, aes_string(x="time", y=W_variable, group=1)) + geom_line() + geom_point()
    }
    
    # Bar plot
    if(W_variable=="Precipitation"){
      plot <- ggplot(data=data, aes_string(x="time", y=W_variable, group = 1)) +
        geom_bar(stat='identity') +
        scale_y_continuous(expand = c(0, 0)) +
        labs(y="Precipitation (mm)")
    }
    # Vector plot
    if(W_variable=="Wind speed"){
      plot <- ggplot(data=data, aes(x=time, y=Wind_speed)) +
        geom_text(aes(angle=-Wind_direction+90), label="-->", size=6) +
        geom_point(size=2) + labs(y="Wind speed (m/s)")
    }
    
    # Change y-labs
    if(W_variable=="Temperature"){
      plot <- plot + labs(y="Temperature (\u00b0 C)")
    }
    
    if(W_variable=="Humidity"){
      plot <- plot + labs(y="Humidity (%)")
    }
    
    if(W_variable=="Cloudcover"){
      plot <- plot + labs(y="Cloudcover (%)")
    }
    
    # Add theme (to all plots)
    plot <- plot +
      labs(title = title,
           x = "Hour",
           subtitle = subtitle) + theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5, size = 16),
            plot.subtitle = element_text(hjust=0.5),
            axis.title = element_text(size=14),
            axis.text = element_text(size=12))
    
    # Change the scale for the variables in % from 0 to 100
    if(any("Humidity"==W_variable, "Cloudcover"==W_variable)){
      plot <- plot + scale_y_continuous(limits=c(0,100))
    }
    return(plot)
  }
  # The app ####
  ui <- fluidPage(
    selectInput("area", label = "Area", choices = c("Link\u00f6ping", "Stockholm", "Uppsala", "Malm\u00f6", "G\u00f6teborg")),
    selectInput("var", label = "Variable", choices = c("Temperature", "Humidity", "Cloudcover", "Precipitation", "Wind speed")),
    dateInput("date", label="Date", value=Sys.Date(), min=Sys.Date(), max=Sys.Date()+7),
    plotOutput("plot")
  )
  
  server <- function(input, output, session) {
    
    output$plot <- renderPlot({render_plot(city = input$area, date = input$date, W_variable = input$var)
    })
  }
  
  shinyApp(ui, server)
}
