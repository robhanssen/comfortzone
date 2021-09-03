library(tidyverse)
library(lubridate)
source("config.r")


# need to replace this later with automated import
data_list <- fs::dir_ls("./processed", regexp = ".csv$")

comfort_init <- map_dfr(data_list, read_csv, .id ="source") %>%
        mutate(source = stringr::str_replace(source, "./processed/", ""),
                source = stringr::str_replace(source, ".csv", ""))

# generate extra data
comfort <-
        comfort_init %>% mutate(
            tempC = 5/9 * (temp_f - 32.0),
            tempF = temp_f,
            year = year(datetime),
            month = month(datetime),
            monthname = month(datetime, label = TRUE),
            day = day(datetime),
            dayofweek = wday(datetime),
            hour = hour(datetime),
            minute = minute(datetime),
            moisturecontent = humidcontent(tempC, humidity) * 1000
                    )

daytime = 8:16
workweek = 2:6

comfort %>%
        filter(source == "Outside", month %in% 6:9, year == 2021, hour >= 8 & hour <= 21) %>%
        ggplot() +
                aes(x = tempF, y = moisturecontent, color = hour) + 
                scale_color_gradient2(low = "green", high = "yellow", mid = "red", midpoint = 15) +
                geom_point(alpha = .99) + 
                labs(x = tempFlabel,
                     y = "Moisture content (mg water/kg air)",
                     color = "Time of day",
                     title = "Daytime conditions in Spartanburg, SC during Summer 2021",
                     subtitle = "Conditions measured between 8:00 and 21:00"
                    ) +
                scale_x_continuous(limits = c(60, 100), sec.axis=sec_axis(name = tempClabel, ~ 5 / 9 * (. -32.0))) + 
                scale_y_continuous(limits = c(0, 30)) +
                facet_wrap(. ~ monthname) +
                geom_line(data = stdcurvedata %>% filter(relativehumidity == 20), color = "grey") + 
                geom_line(data = stdcurvedata %>% filter(relativehumidity == 40), color = "grey") +
                geom_line(data = stdcurvedata %>% filter(relativehumidity == 60), color = "grey") +
                geom_line(data = stdcurvedata %>% filter(relativehumidity == 80), color = "grey") +
                geom_line(data = stdcurvedata %>% filter(relativehumidity == 100), color = "grey") + 
                geom_polygon(data = boundingbox, fill = "green", color = NA, alpha = 0.2) +
                annotate("text", x = 88, y = 6, label = "20%") +
                annotate("text", x = 87, y = 11, label = "40%") +
                annotate("text", x = 84, y = 15, label = "60%") +
                annotate("text", x = 79, y = 17, label = "80%") +
                annotate("text", x = 73, y = 18, label = "100%") +
                annotate("text", x = 75, y = 1, label = "Comfort\nzone")

#ggsave("./graphs/Lab42.png")
ggsave("./graphs/SCoutside.pdf", width=11, height=8)