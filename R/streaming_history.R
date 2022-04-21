#' Finds Top Artist you listen to
#'
#' @param filepath JSON file
#'
#' @return top playlists
#'
#'
#' @import rjson
#' @import purrr
#' @import tidyverse
#' @export


convert_to_df <- function(filepath) {

  dat <- fromJSON(file = filepath)
  dat <- purrr::map_dfr(dat, ~.x)
  return(dat)

}


top_artists <- function(filepath, n = 5) {

  dat <- counvert_to_df(filepath)

  dat %>%
    group_by(artistName) %>%
    count(artistName) %>%
    mutate(count = n) %>%
    select(-n) %>%
    arrange(desc(n)) %>%
    head(n = n)

}
