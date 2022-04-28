#' Converts JSON file to a dataframe
#'
#' @param filepath The path to the json file
#'
#' @return A dataframe


convert_to_df <- function(filepath) {

  dat <- fromJSON(file = filepath)
  dat <- purrr::map_dfr(dat, ~.x)
  dat<-dat%>%
    mutate(year = str_sub({endTime}, 1, 4),                  # separating month, day, and year into separate variables
           month = str_sub({endTime}, 6, 7),
           day = as.numeric(str_sub({endTime}, 9, 10)),
           minutes_played = {msPlayed} / 60000)
  return(dat)
}


#' Creates a visualization for minutes played based on users input
#'
#' @param df Manipulated dataframe
#' @param dmonth string of month user would like to visualize, "01 - 12"
#'
#' @examples
#'
#'
#'
#' @return graph of minutes played for wanted month
#'
#' @import rjson
#' @import purrr
#' @import dplyr
#'@import tidyverse
#' @import ggplot2
#'
#' @export

#

month_wrap <- function(filepath = NULL, dat=NULL, dmonth){
  if (is.null(filepath) && is.null(dat)){ #if neither a filepath nor df are supplied, stop

    stop("Needs data file input.")

  } else if (!is.null(filepath)) { #if there is a filepath, convert to a tbl

    dat <- convert_to_df(filepath)

  }
  dat %>%
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
#' @param filepath JSON file
#' @param dat a tidy data frame
#' @param n the number of artists to return
#'
#' @examples
#' df <- month_detect(userdf)
#' year_wrap(df)
#'
#' @return graph of minutes played for entire year
#'
#' @import rjson
#' @import purrr
#' @import dplyr
#'@import tidyverse
#' @import ggplot2
#' @import ggridges
#'
#' @export

year_wrap <- function(filepath = NULL, dat = NULL, n = 5) {

  if (is.null(filepath) && is.null(dat)){ #if neither a filepath nor df are supplied, stop

    stop("Needs data file input.")

  } else if (!is.null(filepath)) { #if there is a filepath, convert to a tbl

    dat <- convert_to_df(filepath)

  }


  dat %>%
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


#' Find your most listened-to artists by minutes listened
#'
#' @param filepath JSON file
#' @param dat a tidy data frame
#' @param n the number of artists to return
#'
#' @return graph of minutes played for entire year
#'
#' @import rjson
#' @import purrr
#' @import dplyr
#'@import tidyverse
#' @import ggplot2
#'
#' @export
#'
top_artists_time_graph <- function(filepath = NULL, dat = NULL, n = n) {

  if (is.null(filepath) && is.null(dat)){ #if neither a filepath nor df are supplied, stop

    stop("Needs data file input.")

  } else if (!is.null(filepath)) { #if there is a filepath, convert to a tbl

    dat <- convert_to_df(filepath)

  }

  dat %>%
    group_by(artistName) %>%
    summarise_at(vars(minutes_played),
                 list(sum = sum))%>%
    arrange(desc(sum))%>%
    top_n({n})%>%
    ggplot(., aes(x=reorder(artistName, -sum), y=sum))+
    geom_bar(stat='identity', fill="steelblue")+
    labs(title="Time Spent Listening To Your {{n}} Favorite Artists",
         x="Artist", y = "Minutes")+
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))

}
