## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(fixest)
data(airquality)

setFixest_notes(FALSE) # To avoid messages when NAs are removed

est_noFE = feols(Ozone ~ Solar.R + Wind + Temp, airquality)
est_1FE  = feols(Ozone ~ Solar.R + Wind + Temp | Day, airquality)
est_2FE  = feols(Ozone ~ Solar.R + Wind + Temp | Day + Month , airquality)
est_poly = feols(Ozone ~ Solar.R + Wind + poly(Temp, 3) | Day + Month, airquality)

## ---- eval = FALSE------------------------------------------------------------
#  # Dictionary => set only once per session
#  setFixest_dict(c(Ozone = "Ozone (ppb)", Solar.R = "Solar Radiation (Langleys)",
#                   Wind = "Wind Speed (mph)", Temp = "Temperature"))
#  
#  etable(est_noFE, est_1FE, est_2FE, est_poly, est_slopes, cluster = "Day", file = file,
#         group = list("Temperature (cubic)" = "poly"), notes = "Estimation of 5 models.")
#  

## -----------------------------------------------------------------------------
new_style = list(lines = "top:\\toprule; bottom:\\bottomrule",
                 var = "title:\\midrule", # cleans the title but keeps line
                 depvar = "", # cleans the title
                 model = "format:$Model_{I}$",  # cleans the titles + changes model format
                 fixef = "title: ; suffix: fixed effects; where:stats",
                 # fixef: cleans the titles + adds a suffix + places after the stats
                 slopes = "format:__var__ $\\times $ __slope__",
                 stats = "title: ") # cleans the title but adds an empty line

setFixest_etable(fitstat = ~r2, signifCode = NA, yesNo = "$\\checkmark$",
                 tablefoot = FALSE, style = new_style)

## ---- eval = FALSE------------------------------------------------------------
#  etable(est_noFE, est_1FE, est_2FE, est_poly, est_slopes, cluster = "Day", file = file,
#         group = list("Temperature (cubic)" = "poly"), notes = "Estimation of 5 models.")

