#' Find your most listened-to artists
#'
#' @param filepath JSON file
#' @param dat a tidy data frame
#' @param n the number of artists to return
#'
#' @return A dataframe with top artists
#'
#' @import rjson
#' @import purrr
#' @import dplyr
#'
#' @export

top_artists <- function(filepath = NULL, dat = NULL, n = 5) {

  if (is.null(filepath) && is.null(dat)){ #if neither a filepath nor df are supplied, stop

    stop("Needs data file input.")

  } else if (!is.null(filepath)) { #if there is a filepath, convert to a tbl

    dat <- convert_to_df(filepath)

  }

  dat %>%
    group_by(artistName) %>%
    count(artistName) %>%
    rename(count = n) %>%
    arrange(desc(count)) %>%
    head(n = n)

}

#' Find your most listened-to songs
#'
#' @param filepath JSON file
#' @param dat a tidy data frame
#' @param n the number of songs to return
#'
#' @return A dataframe with top songs
#'
#' @import rjson
#' @import purrr
#' @import dplyr
#'
#' @export

top_songs <- function(filepath = NULL, dat = NULL, n = 5) {

  if (is.null(filepath) && is.null(dat)){ #if neither a filepath nor df are supplied, stop

    stop("Needs data file input.")

  } else if (!is.null(filepath)) { #if there is a filepath, convert to a tbl

    dat <- convert_to_df(filepath)

  }

  dat %>%
    group_by(artistName, trackName) %>%
    count(trackName) %>%
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
