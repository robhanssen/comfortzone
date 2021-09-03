library(tidyverse)
library(lubridate)

generate_filelist <- function(pattern) {
    lst <- c(list.files(path = "./raw", pattern = pattern, full.names = TRUE),
             list.files(path = "./sources", pattern = pattern, full.names = TRUE)
             )
    return(lst)
}

cleanupdata <- function(filelist) {
    tbl <-
        filelist %>%
        map_df(~read_csv(.)) %>%
        janitor::clean_names() %>%
        rename(timestamp = "timestamp_for_sample_frequency_every_15_min",
            temp_f = "temperature_fahrenheit",
            humidity = "relative_humidity") %>%
        mutate(datetime = floor_date(timestamp, unit = "hour")) %>%
        group_by(datetime) %>%
        summarize(temp_f = mean(temp_f),
                humidity = mean(humidity)) %>%
        arrange(datetime)
    return(tbl)
}


data <- generate_filelist("Alex*")
room <- cleanupdata(data)
write_csv(room, "processed/AlexRoom.csv")

data <- generate_filelist("Nova*")
room <- cleanupdata(data)
write_csv(room, "processed/NovaRoom.csv")

data <- generate_filelist("Outside*")
room <- cleanupdata(data)
write_csv(room, "processed/Outside.csv")

data <- generate_filelist("Lab*")
room <- cleanupdata(data)
write_csv(room, "processed/Lab42.csv")

data <- generate_filelist("Living*")
room <- cleanupdata(data)
write_csv(room, "processed/LivingRoom.csv")