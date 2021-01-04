# 
# 
# 
# 
# 
# 
# 
# 
library(tidyverse)
library(lubridate)
source("config.r")


# need to replace this later with automated import
dataname = "Lab42_2020"

datafile <- paste0("sources/", dataname, ".csv")
comfort <- read_csv(datafile, 
                    col_types = cols(`Timestamp for sample frequency every 15 min` = col_datetime(format = "%Y-%m-%d %H:%M:%S"))
                    )
colnames(comfort) = c("datetime","tempF", "humidity")

# data size reduction by factor
reductionfactor = 1
comfort <- comfort[seq(1,nrow(comfort),reductionfactor),]

# generate extra data
comfort %>% mutate(
            tempC = 5/9 * (tempF - 32.0),
            year = year(datetime),
            month = month(datetime),
            day = day(datetime),
            hour = hour(datetime),
            minute = minute(datetime),
            moisturecontent = humidcontent(tempC, humidity) * 1000
                    ) -> comfort


comfort %>%     ggplot() + aes(tempF,moisturecontent) + geom_point(alpha=.5) + 
                xlab(tempFlabel) + ylab("Moisture content (mg water/kg air)") +
                scale_x_continuous(limits=c(60,90), sec.axis=sec_axis(name=tempClabel,~ 5/9*(. -32))) + 
                scale_y_continuous(limits=c(0,20)) + 
                facet_wrap(.~month) +  
                geom_line(data=stdcurvedata %>% filter(relativehumidity==20), color="grey") + 
                geom_line(data=stdcurvedata %>% filter(relativehumidity==40), color="grey") +
                geom_line(data=stdcurvedata %>% filter(relativehumidity==60), color="grey") +
                geom_line(data=stdcurvedata %>% filter(relativehumidity==80), color="grey") +
                geom_line(data=stdcurvedata %>% filter(relativehumidity==100), color="grey") + 
                geom_polygon(data=boundingbox, fill="green", alpha=0.2) +
                annotate("text", x = 88, y = 6, label = "20%") +
                annotate("text", x = 87, y = 11, label = "40%") + 
                annotate("text", x = 84, y = 15, label = "60%") +
                annotate("text", x = 79, y = 17, label = "80%") +
                annotate("text", x = 73, y = 18, label = "100%") +
                annotate("text", x = 75, y = 1, label = "Comfort Zone") 

graphname = paste0("graphs/", dataname, ".pdf")
dataoutput = paste0("data/", dataname, ".csv")

ggsave(graphname, height=8, width=11)
write_csv(comfort, dataoutput)