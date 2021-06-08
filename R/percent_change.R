#' Makes a new column that has percent change from year to year
#'
#'
#' @param data The dataset to modify
#' @param var The variable that to be put in terms of percent change
#'
#' @return A new dataset with the percent change column
#'
#' @import tidyverse
#' @import lubridate
#' @import ggplot2
#'
#' @examples
#' # Data: unemp
#'
#' #Var: UNRATE
#' unemp <- percent_change(unemp, UNRATE)
#'
#' @export


percent_change <- function(data, var){

  var <- data %>% pull({{var}})

  data <- data %>%
    mutate(
      pct_change = (var/ lag(var) - 1) * 100
    )
}





