#' Conducts linear regression and creates a linear plot for Public Debt data from FRED
#'
#' @param data The Public Debt dataset from FRED
#'
#' @return A summary of the linear regression as well as plot with Public Debt over time
#'
#' @import tidyverse
#' @import lubridate
#' @import ggplot2
#'
#'
#' @export
#'


debt_analysis <- function(data){

  data$DATE <- ymd(data$DATE)

  mod <- lm(GFDEBTN ~ DATE, data = data)
  summary <- summary(mod)

  plot <- data %>%
    ggplot(aes(x = DATE, y = GFDEBTN)) +
    geom_point() +
    stat_smooth(method = "lm") +
    labs(title = "Public Debt over the Years, FRED", x = "Year", y = "Public Debt")

  return(list(summary, plot))

}












