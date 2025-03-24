

#' Calweek
#'
#' @param dates
#' @param week_start
#'
#' @return
#' @export
#'
#' @examples
calweek <- function(dates, week_start=7){

  # Jan 1st of year for each date in d
  jan1 <- lubridate::floor_date(dates, unit = "year")

  # Date of 1st day of week including jan1 for dates in d
  day1_week1 <- lubridate::floor_date(jan1, unit="week", week_start=week_start)

  # Calculate weeks since day1 of week including jan1
  as.numeric(dates - day1_week1) %/% 7 + 1
}


#' Download the Excel template
#'
#' Copies the Excel template file to a specified directory.
#'
#' @param path Character. The directory where the template should be saved. Defaults to the current working directory.
#' @param filename Character. The name of the saved file. Defaults to "template.xlsx".
#' @return The full path of the saved file.
#' @export
download_template <- function(path = ".", filename = "Inputs.xlsx") {
  template_path <- system.file("extdata", "Inputs.xlsx", package = "ECFchinook")

  if (template_path == "") {
    stop("Template file not found. Please reinstall the package.")
  }

  destination <- file.path(path, filename)
  file.copy(template_path, destination, overwrite = TRUE)

  message("Template saved to: ", destination)
  return(destination)
}
