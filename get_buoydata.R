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
    
#44029
#bdata <- map_df(c(2004:2008,2011:2022), ~buoy("stdmet", buoyid = 44029, year = .x)$data)
#bdata <- 
# get_buoydata_erddap(2001, 2022, 44029) |>
#     summarize_bdata() |>
#     write_csv(file="data/buoy_history_44029.csv")




#44013
#bdata <- map_df(1984:2022, ~buoy("stdmet", buoyid = 44013, year = .x)$data)
# get_buoydata_erddap(1984, 2022, 44013) |>
#     summarize_bdata() |>
#     write_csv(file="data/buoy_history_44013.csv")
# 
# 
# get_buoydata_erddap(1984, 2022, 44098) |>
#     summarize_bdata() |>
#     write_csv(file="data/buoy_history_44098.csv")
# 
# get_buoydata_erddap(1984, 2022, 44018) |>
#     summarize_bdata() |>
#     write_csv(file="data/buoy_history_44018.csv")
# 
# get_buoydata_erddap(1984, 2022, 44090) |>
#     summarize_bdata() |>
#     write_csv(file="data/buoy_history_44090.csv")
# 
# get_buoydata_erddap(1984, 2022, 44030) |>
#     summarize_bdata() |>
#     write_csv(file="data/buoy_history_44090.csv")
# 
# get_buoydata_erddap(1984, 2022, 44005) |>
#     summarize_bdata() |>
#     write_csv(file="data/buoy_history_44005.csv")

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

walk(buoys, ~ get_buoydata_erddap(1984, 2022, .x) |>
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

# bdata <- bdata |>
#     mutate(time = ymd_hms(time)) |>
#     group_by(year = year(time), month = month(time), day = day(time)) |>
#     summarize(across(wind_dir:water_level, ~ mean(.x, na.rm = TRUE)),
#               .groups = "drop") |>
#     mutate(time = ymd(paste(year, month, day, sep = "-")))
# 
# #-------
# # and write out results
# #--------
# write_csv(bdata, file="data/buoy_history_44029.csv")

