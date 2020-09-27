humidcontent <- function(Tc, hum)
{
    Tk = Tc + 273.15
    satvap = 6.112 * exp( (17.62 * Tc) / (Tc + 243.12) )
    presfunction = (1.0016 + 0.00000315 * 1000) - (0.074 * 1) / 1000
    moistairsatvap = satvap * presfunction * 100
 	satcon = (moistairsatvap * hum) / 100
 	massvolratio = satcon / (461.5 * Tk)
 	massmassratio = massvolratio / 1.225
 	return( massmassratio)
}


# generate standard curves @ 0-100% RH, temperatures in F
Tstart = 50
Tend = 100
stepsize = 2
ndatapoint = (Tend - Tstart)/stepsize + 1

stdcurvedata = tibble(R20=seq(0,0,ndatapoint),
                      R40=seq(0,0,ndatapoint),
                      R60=seq(0,0,ndatapoint),
                      R80=seq(0,0,ndatapoint),
                      R100=seq(0,0,ndatapoint),
                      tempF=seq(Tstart,Tend,stepsize)
                      )

stdcurvedata %>% pivot_longer(cols=starts_with("R"),
                              names_prefix="R",
                              names_to="relativehumidity", 
                              values_to="moisturecontent") %>% 
                              mutate(relativehumidity = as.numeric(relativehumidity),
                                     tempC = 5/9*(tempF - 32.0),
                                     moisturecontent = 1000*humidcontent(tempC,relativehumidity)) %>%
                              arrange(relativehumidity)-> stdcurvedata

# Polygon( [69.5, 74.5, 80, 71], [0.012, 0.012, 0, 0] );
boundingbox = tibble(tempF = c(69.5, 74.5, 80, 71), moisturecontent=1000*c(0.012, 0.012, 0, 0))

tempClabel = paste("Temperature (\U00B0", "C)", sep="")
tempFlabel = paste("Temperature (\U00B0", "F)", sep="")


