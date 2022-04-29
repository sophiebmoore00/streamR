#' Find your most listened-to artists by number of streams or minutes listened
#'
#' @param filepath path to a JSON file
#' @param dat a tidy data frame
#' @param method how to find top artists ("streams" or "minutes")
#' @param n the number of artists to return
#'
#' @return A dataframe with top artists
#'
#' @import rjson
#' @import purrr
#' @import dplyr
#'
#' @export

top_artists <- function(filepath = NULL, dat = NULL, method = "streams", n = 5) {

  if (is.null(filepath) && is.null(dat)){ #if neither a filepath nor df are supplied, stop

    stop("Needs data file input.")

  } else if (!is.null(filepath)) { #if there is a filepath, convert to a tbl

    dat <- convert_to_df(filepath)

  }

  if (method == "streams") {

    dat %>%
      group_by(artistName) %>%
      count(artistName) %>%
      rename(count = n) %>%
      arrange(desc(count)) %>%
      head(n = n)

  } else if (method == "minutes") {

    dat %>%
      group_by(artistName) %>%
      summarise_at(vars(minutes_played),
                   list(sum = sum))%>%
      arrange(desc(sum)) %>%
      head(n = n)

  } else {

    stop("The only valid inputs to argument `method` are 'streams' and 'minutes'.")

  }
}

