#' Conduct Analysis of the relationship between inflation and unemployment: Philip's Curve
#'
#'
#' @param infl The inflation rate dataset from FRED
#' @param unemp The unemployment rate dataset from FRED
#'
#' @return A plot depicting the relationship between inflation rate and unemployment
#'
#' @import tidyverse
#' @import lubridate
#' @import ggplot2
#'
#' @export
#'

philips <- function(infl, unemp){

  colnames(infl)[2] <- "pi"
  infl$pi <- round(infl$pi , 4)
  u_i <- inner_join(infl, unemp, by = "DATE")

  plot <- u_i %>%
    ggplot(aes(x = UNRATE, y = pi))+
    geom_point() +
    geom_smooth() +
    labs(title = "Philip's Curve", x = "Unemployment Rate", y = "Inflation Rate")

  return(plot)

}










