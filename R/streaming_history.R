#' Finds Top Artist you listen to
#'
#' @param filepath JSON file
#'
#' @return A dataframe with top artists
#'
#' @import rjson
#' @import purrr
#' @import dplyr
#'
#' @export

top_artists <- function(filepath, n = 5) {

  dat <- convert_to_df(filepath)

  dat %>%
    group_by(artistName) %>%
    count(artistName) %>%
    rename(count = n) %>%
    arrange(desc(count)) %>%
    head(n = n)

}

#' Converts JSON file to a dataframe
#'
#' @param filepath The path to the json file
#'
#' @return A dataframe

convert_to_df <- function(filepath) {

  dat <- fromJSON(file = filepath)
  dat <- purrr::map_dfr(dat, ~.x)
  return(dat)

}
