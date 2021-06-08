#' Conduct Analysis of the relationship of percent change of GDP and Unemployment: Okun's Law
#'
#'
#' @param gdp The GDP dataset from FRED
#' @param unemployment The unemployment rate dataset from FRED
#'
#' @return Summary of linear regression of the relationship between percent change in GDP and unemployment as well as a plot depicting this relationship
#'
#' @import tidyverse
#' @import lubridate
#' @import ggplot2
#'
#' @export
#'

okuns_law <- function(gdp, unemployment){


  u_gdp <- inner_join(unemployment, gdp, by = "DATE")
  u_gdp$DATE <- ymd(u_gdp$DATE)

  u_gdp <- u_gdp %>%
    mutate(
      unemp_pct_change = (UNRATE/lag(UNRATE) - 1) * 100,
      GDP_pct_change = (GDP/lag(GDP) - 1) * 100
    )


  plot <- u_gdp %>%
    ggplot(aes(x = unemp_pct_change, y = GDP_pct_change)) +
    xlim(-5,5)+
    ylim(-3,5)+
    labs(title = "Okun's Law", x = "Change in Unemployment Rate", y = "Change in GDP") +
    geom_point() +
    stat_smooth(method = "lm")

  mod <- lm(GDP_pct_change ~ unemp_pct_change, data = u_gdp)
  sum <- summary(mod)

  return(list(sum, plot))
}








