#------------------------------------
#' Use site info to harvest buoy
#' temperature data from NOAA
#------------------------------------
library(tidyverse)
library(readr)
library(rnoaa) #make sure it is from github
library(lubridate)
library(glue)
library(rerddap)

#-------
# get data and process function
#--------

get_buoydata_erddap <- function(year_start, year_end, station){
    
    station <- toupper(station)

    tabledap(x ="cwwcNDBCMet",
             glue('station="{station}"'),
             glue('time>={year_start}-01-01'), 
             glue('time<={year_end}-12-31'),
             fields = c("time", "wd", "wtmp", "wvht", "tide")) |>
    as_tibble() |>
    rename(wind_dir = wd,
           water_level = tide,
           sea_surface_temperature = wtmp) |>
    mutate(across(wind_dir:water_level, as.numeric)) |>
    mutate(time = ymd_hms(time)) |>
    group_by(year = year(time), month = month(time), day = day(time)) |>
    summarize(across(wind_dir:water_level, ~ mean(.x, na.rm = TRUE)),
              .groups = "drop") |>
    mutate(time = ymd(paste(year, month, day, sep = "-")))

}

#-------
# summarize data function
#--------

summarize_bdata <- function(bdata){
    bdata |>
       # mutate(time = ymd_hms(time)) |>
        group_by(year, month, day) |>
        summarize(across(wind_dir:water_level, ~ mean(.x, na.rm = TRUE)),
                  .groups = "drop") |>
        mutate(time = ymd(paste(year, month, day, sep = "-")))
}

#-------
# Get Buoy Info
#--------

buoys <- c(44013, 44018, 44090, 44030, 44005, 44029, 44032, 44037, 44034, 44027, 44011#, 
           #44150, #oh, canada!
           #44258, 
           #44488, 
           #44403
           )

binfo <- buoy_stations() |>
    filter(station %in% buoys) |>
    select(station, lat, lon)

write_csv(binfo, "data/buoy_info.csv")


#-------
# Go through Buoys and use functions above to get environmental records
#--------

walk(buoys, ~ get_buoydata_erddap(1984, 2023, .x) |>
         summarize_bdata() |>
         write_csv(file=glue("data/buoy_history_{.x}.csv")),
     .progress=TRUE)

#check
# b <- map(buoys, ~ get_buoydata_erddap(1984, 2022, .x) |>
#               summarize_bdata())
# 
# ggplot(b[[1]] |> mutate(plot_time = ymd(paste(year, month, day, sep = "-"))),
#        aes(x = plot_time, y = sea_surface_temperature)) +
#     geom_line()
