#' 900 monthly time series of length 48
#'
#' A dataset containing 900 synthetic example time series that can be used
#' to trial the `recur` package functionality.
#'
#' @format A data frame with 43200 rows and 3 variables:
#' \describe{
#'   \item{id}{the id of the time series in the format `ts-000`}
#'   \item{date}{the date of the monthly observation, formatted as `YYYY-MM-01`}
#'   \item{value}{the value of the time series (here i.i.d standard normal
#'   random values)}
#' }
"synthetic"