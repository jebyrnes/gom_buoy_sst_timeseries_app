#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readr)
library(rerddap)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gghighlight)
library(ggeasy)
library(rnaturalearth)
library(sf)
library(ggrepel)
library(plotly)
library(glue)
source("buoy_functions.R")


thisyear <- 2024

binfo <- read_csv("data/buoy_info.csv",
                  show_col_types = FALSE) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

coast <- ne_countries(country = "United States of America", 
                   scale = 50,
                   returnclass = "sf") |>
    st_crop(st_bbox(binfo) + c(-1,0,0.5,2.3))

buoy_map <- ggplot() + 
    geom_sf(data = coast, fill = "brown") +
    geom_sf(data = binfo) +
    ggrepel::geom_label_repel(
        data = binfo,
        aes(label = station, geometry = geometry),
        stat = "sf_coordinates",
        min.segment.length = 0
    ) +
    labs(x="", y="") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "lightblue"))
  
buoys <- list.files("data", pattern="_\\d") |>
    stringr::str_remove("buoy_history_") |>
    stringr::str_remove("\\.csv")


    
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Temperature Records from Gulf of Maine Buoys with Historical Comparisons"),
    
    # row layout
    fluidRow(
        column(4,
               selectInput("buoy_id",
                           label = "Which Buoy?",
                           choices = binfo$station,
                           selected = 44013)
        ),
        column(8,
               selectInput("c_or_f",
                           label = "C of F?",
                           choices = c("C", "F"),
                           selected = "C")
        )
    ),
    
    fluidRow(
        column(3,
               plotOutput("buoy_map_highlighted")
               ),
        column(9,
               plotlyOutput("timeseries")
        ),
        
    ),
    
    fluidRow(
        column(12,
               HTML("Data are taken from <a href='https://coastwatch.noaa.gov/cwn/index.html'> NOAA
                    Coastwatch's</a> <a href=https://coastwatch.pfeg.noaa.gov/erddap/index.html>ERDDAP
                    server</a> using the <a href=https://cran.r-project.org/web/packages/rerddap/index.html>
                    rerddap</a> library in R. For more, see the <a href=https://www.ndbc.noaa.gov/>National 
                    Buoy Data Center</a> or an interpolated map from the <a href=https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.graph?analysed_sst%5B(2023-07-10T09:00:00Z)%5D%5B(40.645):(47.175)%5D%5B(-71.655):(-65.125)%5D&.draw=surface&.vars=longitude%7Clatitude%7Canalysed_sst&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff>
                    MURSST product centered on the GOM - just remember to change the time to the current day</a>. Code for this Shiny app available 
                    <a href=https://github.com/jebyrnes/gom_buoy_sst_timeseries_app>here</a>."))
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    bdata <- reactive({
        read_csv(glue("data/buoy_history_{input$buoy_id}.csv"),
                 show_col_types = FALSE)
    })
    
    current_bdata <- reactive({
        get_buoydata_erddap(thisyear, thisyear, input$buoy_id) |>
            summarize_bdata()
    })
    
    summary_bdata <- reactive({
        current_bdata() |>
            group_by(day, month) |>
            summarize(across(wind_dir:water_level, 
                             list(mean = ~ mean(.x, na.rm = TRUE),
                                  lower = ~ quantile(.x, prob = 0.1, na.rm = TRUE),
                                  upper = ~ quantile(.x, prob = 0.9, na.rm = TRUE))),
                      .groups = "drop") |>
            mutate(plot_time = ymd(paste("2000", month, day, sep = "-"))) #arbitrary year
    })
    
    # for plotting
    all_bdata <- reactive({
        ret <- bind_rows(bdata(), current_bdata())|>
            mutate(plot_time = ymd(paste("2000", month, day, sep = "-")),#arbitrary year
                   date = paste(year, month, day, sep = "-")) 
        
        if(input$c_or_f =="F"){
            ret <- ret |>
             mutate(sea_surface_temperature = sea_surface_temperature*(9/5)+32)
        }
        
        ret
        
    })
    
    output$buoy_map_highlighted <- renderPlot({
        binfo_onebuoy <- binfo |>
            dplyr::filter(station==input$buoy_id)
        
        buoy_map +
            geom_sf(data = binfo_onebuoy, color = "red") 
            
        
    })
    
    output$timeseries <- renderPlotly({
        
        yrs_before <- length(unique(all_bdata()$year))-2
        
        
        tseries <- ggplot(all_bdata(),
                          aes(x = plot_time, y = sea_surface_temperature, 
                              color = as.character(year),
                              label = date)) +
            geom_line() +
            theme_bw() +
            scale_x_date(date_labels = "%B", date_breaks = "1 month") +
            easy_rotate_x_labels(angle = 45, side = "right") +
            easy_legend_at("bottom") +
            scale_color_manual(values = c(rep("lightgrey", yrs_before), "orange", "black")) +
            labs(y = "SST", x = "", color = "year", title = glue("Buoy {input$buoy_id}"))
        
        plotly::ggplotly(p = tseries,
                         tooltip = c("y", "label"))
        
        
    })

   
}

# Run the application 
shinyApp(ui = ui, server = server)
