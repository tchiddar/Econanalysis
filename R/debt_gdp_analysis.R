#' Combines the GDP and Public Dataset from FRED and plots both on a single graph
#'
#' @param debt The Public Debt dataset from FRED
#' @param gdp The GDP dataset from FRED
#'
#' @return A plot of Public Debt (in thousands) and GDP (in billions) on the same graph
#'
#' @import tidyverse
#' @import lubridate
#' @import ggplot2
#'
#'
#' @export
#'


debt_gdp_analysis <- function(debt, gdp){

  comb <- full_join(debt, gdp, by = "DATE")

  comb$DATE <- ymd(comb$DATE)
  comb$GFDEBTN <- comb$GFDEBTN / 1000

  comb %>%
    ggplot() +
    geom_line(aes(x = DATE, y = GDP)) +
    geom_line(aes(x = DATE, y = GFDEBTN))+
    scale_y_continuous(
      "GDP (in billions)",
      sec.axis = sec_axis(~ . * 1 , name = "Public Debt (in thousands) ")) +
    labs(title = "Public Debt and GDP over the years", x = "Year")

}





