#' Calculate Age of Sample from Collection Date
#'
#' Determines the age of a sample in years based on its collection date.
#' Handles multiple date formats and missing values.
#'
#' @param collection_date A date value, character string, or NA. Accepts various
#'   formats including M/D/YYYY, M/D/YY, and standard R date formats.
#' @param reference_date Date to calculate age from. Default is current system date.
#'
#' @return Numeric value representing sample age in years, or NA if date cannot
#'   be parsed or is missing.
#'
#' @details
#' The function attempts to parse dates in the following order:
#' \enumerate{
#'   \item M/D/YYYY format (e.g., "12/25/2020")
#'   \item M/D/YY format (e.g., "12/25/20")
#'   \item Standard R date format
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate age from a date string
#' calculate_age("01/15/2020")
#'
#' # Calculate age from an R Date object
#' calculate_age(as.Date("2020-01-15"))
#'
#' # Specify a reference date to calculate the age from
#' calculate_age("01/15/2020", reference_date = as.Date("2023-01-01"))
#' }
#'
#' @export
calculate_age <- function(collection_date, reference_date = Sys.Date()) {
  if (is.na(collection_date) || collection_date == "") return(NA)

  # convert string to date format
  if (is.character(collection_date)) {
    # try a couple of different date formats
    parsed_date <- tryCatch({
      # M/D/YYYY
      as.Date(collection_date, format = "%m/%d/%Y")
    }, error = function(e) {
      tryCatch({
        # M/D/YY
        as.Date(collection_date, format = "%m/%d/%y")
      }, error = function(e) {
        tryCatch({
          as.Date(collection_date)
        }, error = function(e) {
          return(NA)
        })
      })
    })

    collection_date <- parsed_date
  }

  if (is.na(collection_date)) return(NA)

  # get the age in years
  age_years <- as.numeric(difftime(reference_date, collection_date, units = "days")) / 365.25
  return(age_years)
}
