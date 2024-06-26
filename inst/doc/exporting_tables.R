## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#>")

is_pander = requireNamespace("pander", quietly = TRUE)
if(is_pander) library(pander)

library(fixest)
setFixest_notes(FALSE)
setFixest_etable(digits = 3)

## ----eval = TRUE, results = "hide"--------------------------------------------
library(fixest)
data(airquality)

# Setting a dictionary 
setFixest_dict(c(Ozone = "Ozone (ppb)", Solar.R = "Solar Radiation (Langleys)",
                 Wind = "Wind Speed (mph)", Temp = "Temperature"))

## -----------------------------------------------------------------------------
# On multiple estimations: see the dedicated vignette
est = feols(Ozone ~ Solar.R + sw0(Wind + Temp) | csw(Month, Day), 
            airquality, cluster = ~Day)

## -----------------------------------------------------------------------------
etable(est)

## -----------------------------------------------------------------------------
etable(est, style.df = style.df(depvar.title = "", fixef.title = "", 
                                fixef.suffix = " fixed effect", yesNo = "yes"))

## ----eval = !is_pander, include = !is_pander----------------------------------
#  # NOTE:
#  # The evaluation of the code of this section requires the
#  #   package 'pander' which is not installed.
#  # The code output is not reported.

## ----eval = is_pander---------------------------------------------------------
library(pander)

etable(est, postprocess.df = pandoc.table.return, style = "rmarkdown")

## ----eval = is_pander---------------------------------------------------------
my_style = style.df(depvar.title = "", fixef.title = "", 
                    fixef.suffix = " fixed effect", yesNo = "yes")
setFixest_etable(style.df = my_style, postprocess.df = pandoc.table.return)

## ----eval = is_pand, eval = is_pander-----------------------------------------
etable(est[rhs = 2], style = "rmarkdown", caption = "New default values")

## -----------------------------------------------------------------------------
est_slopes = feols(Ozone ~ Solar.R + Wind | Day + Month[Temp], airquality)

## ----results = 'hide'---------------------------------------------------------
etable(est, est_slopes, tex = TRUE)

## ----include = FALSE----------------------------------------------------------
# etable(est, est_slopes, file = "../_VIGNETTES/vignette_etable.tex", replace = TRUE)
# etable(est, est_slopes, file = "../_VIGNETTES/vignette_etable.tex", style.tex = style.tex("aer"), fitstat = ~ r2 + n, signif.code = NA)

## ----results = 'hide'---------------------------------------------------------
etable(est, est_slopes, style.tex = style.tex("aer"), 
       signif.code = NA, fitstat = ~ r2 + n, tex = TRUE)

## -----------------------------------------------------------------------------
set_rules = function(x, heavy, light){
  # x: the character vector returned by etable
  
  tex2add = ""
  if(!missing(heavy)){
    tex2add = paste0("\\setlength\\heavyrulewidth{", heavy, "}\n")
  }
  if(!missing(light)){
    tex2add = paste0(tex2add, "\\setlength\\lightrulewidth{", light, "}\n")
  }
  
  if(nchar(tex2add) > 0){
    x[x == "%start:tab\n"] = tex2add
  }
  
  x
}

## ----results = 'hide'---------------------------------------------------------
etable(est, est_slopes, postprocess.tex = set_rules, heavy = "0.14em", tex = TRUE)

## -----------------------------------------------------------------------------
setFixest_etable(style.tex = style.tex("aer", signif.code = NA), postprocess.tex = set_rules, 
                 fitstat = ~ r2 + n)

## -----------------------------------------------------------------------------
etable(est, heavy = "0.14em", tex = TRUE)

## -----------------------------------------------------------------------------
fitstat_register(type = "p_s", alias = "pvalue (standard)",
                 fun = function(x) pvalue(x, vcov = "iid")["Solar.R"])

fitstat_register(type = "p_h", alias = "pvalue (Heterosk.)",
                 fun = function(x) pvalue(x, vcov = "hetero")["Solar.R"])

fitstat_register(type = "p_day", alias = "pvalue (Day)",
                 fun = function(x) pvalue(x, vcov = ~Day)["Solar.R"])

fitstat_register(type = "p_month", alias = "pvalue (Month)",
                 fun = function(x) pvalue(x, vcov = ~Month)["Solar.R"])

# We first reset the default values set in the previous sections
setFixest_etable(reset = TRUE)
# Now we display the results with the new fit statistics
etable(est, fitstat = ~ . + p_s + p_h + p_day + p_month)

## ----eval = FALSE-------------------------------------------------------------
#  summary(.l(est, est_slopes), cluster = ~ Month)

