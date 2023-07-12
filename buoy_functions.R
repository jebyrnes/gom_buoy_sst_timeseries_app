
#-------
# get data and process function
#--------

get_buoydata_erddap <- function(year_start, year_end, station){
    
    station <- toupper(station)
    
    tabledap(x ="cwwcNDBCMet",
             url = "https://upwell.pfeg.noaa.gov/erddap/",
             #url = "https://polarwatch.noaa.gov/erddap/",
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