#' Conducts linear regression and creates a linear plot for GDP data from FRED
#'
#' @param data The GDP dataset from FRED
#'
#' @return A summary of the linear regression as well as plot with GDP over time
#'
#' @import tidyverse
#' @import lubridate
#' @import ggplot2
#'
#'
#'
#' @export


gdp_analysis <- function(data){

  data$DATE <- ymd(data$DATE)

  mod <- lm(GDP ~ DATE, data = data)
  summary <- summary(mod)

  plot <- data %>%
    ggplot(aes(x = DATE, y = GDP)) +
    geom_point() +
    stat_smooth(method = "lm") +
    labs(title = "GDP over the Years, FRED", x = "Year", y = "GDP in billions")

  return(list(summary, plot))

}









