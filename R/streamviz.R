#' Manipulates users data to be used for visualization
#'
#' @param df The data frame converted from JSON file
#'
#' @examples
#' dataframe <- month_detect(userdf)
#'
#' @return A dataframe
#'
#' @import tidyverse
#'
#' @export

month_detect <- function(df){
    df %>%
      mutate(year = str_sub({endTime}, 1, 4),                  # separating month, day, and year into separate variables
             month = str_sub({endTime}, 6, 7),
             day = as.numeric(str_sub({endTime}, 9, 10)),
             minutes_played = {msPlayed} / 60000)
}


#' Creates a visualization for minutes played based on users input
#'
#' @param df Manipulated dataframe
#' @param dmonth string of month user would like to visualize, "01 - 12"
#'
#' @examples
#' df <- month_detect(userdf)
#' month_wrap(df, "05")
#'
#' @return graph of minutes played for wanted month
#'
#' @import tidyverse
#' @import ggplot2
#'
#' @export

month_wrap <- function(df, dmonth){
  df %>%
    filter({month} == dmonth) %>%
    ggplot(aes({day}, {minutes_played}, group = {month})) +         # df must be from spotify file no other manipulation needed
    geom_line() +
    scale_color_viridis_d() +
    labs(x = "Day of Month", y = "Minutes Played") +
    ggtitle("Minutes Played Throughout Month") +
    theme_classic()

}

#' Creates a visualization for minutes played for the entire year
#'
#' @param df Manipulated dataframe
#'
#' @examples
#' df <- month_detect(userdf)
#' year_wrap(df)
#'
#' @return graph of minutes played for entire year
#'
#' @import tidyverse
#' @import ggplot2
#' @import ggridges
#'
#' @export

year_wrap <- function(df){

  df %>%
    ggplot(aes(x = {day}, y = {month}, fill = {minutes_played})) +    # df must be from spotify file no other manipulation needed
    geom_density_ridges(scale = 4, rel_min_height = 0.01) +
    labs(title = 'Yearly Distribution of Minutes Played') +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("Day") +
    ylab("Month") +
    theme_classic()
}

