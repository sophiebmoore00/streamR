#' Converts JSON file to a tidy dataframe
#'
#' @param filepath The path to the json file
#'
#' @return A tidy dataframe
#'
#' @import dplyr
#' @import rjson
#' @import stringr
#'
#' @export

convert_to_df <- function(filepath) {

  dat <- fromJSON(file = filepath)
  dat <- purrr::map_dfr(dat, ~.x)
  dat <- dat%>%
    mutate(year = str_sub({endTime}, 1, 4),  # separating month, day, and year into separate variables
           month = str_sub({endTime}, 6, 7),
           day = as.numeric(str_sub({endTime}, 9, 10)),
           minutes_played = {msPlayed} / 60000)
  return(dat)
}
