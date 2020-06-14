#' Measure the recurrence within time series
#' 
#' @param data A dataframe with columns "id", "date", "value". See [synthetic] 
#'     for more information.
#' @param absolute If TRUE (default), the difference between two observations is
#'     the absolute difference.
#' @param size Should half of the recurrence observations be dropped from each
#'     time series' matrix? If "triangle" (default), only the lower triangle
#'     of the recurrence matrix of each time series is returned. If "square",
#'     the full recurrence matrix is returned. This could for example be used
#'     with `absolute = FALSE`.
#' @param shape If "wide" (default), the recurrence matrices are returned as a
#'     wide dataframe which can be passed directly to, for example, 
#'     [umap::umap()] for further modeling. If "long", the recurrence matrices
#'     are returned as one long dataframe which makes use with [ggplot2::ggplot]
#'     simple.
#' @param start_date The first date considered when extending every time series.
#'     If `NA`, then set to `min(data$date)`.
#' @param end_date The last date considered when extending every time series.
#'     If `NA`, then set to `max(data$date)`.
#' @param na_replacement The value with which missing values in the time series
#'     are imputed. The missing values can also be generated due to the
#'     expansion implied by `start_date` and `end_date`. The default replacement
#'     is 0. If you require more complicated imputation, please extend each
#'     time series manually upfront to the range [`start_date`, `end_date`].
#'     
#' @references
#' Xixi Lia, Yanfei Kanga, Feng Li (2020). 
#' [*Forecasting with Time Series Imaging*](https://arxiv.org/abs/1904.08064), 
#' ArXiv e-prints 1802.03426.
#' 
#' J.-P. Eckmann, S. Oliffson Kamphorst, D. Ruelle (1987).
#' [*Recurrence Plots of Dynamical Systems*](http://iopscience.iop.org/0295-5075/4/9/004),
#' EPL (Europhysics Letters), Vol. 4, 9.
#' 
#' @examples 
#' times <- recur::synthetic
#' measured <- measure(data = times, size = "triangle", shape = "wide")
#' 
#' @export
measure <- function(data, 
                    absolute = TRUE,
                    size = c("triangle", "square"),
                    shape = c("wide", "long"),
                    start_date = NA, end_date = NA,
                    na_replacement = 0) {
  
  # https://arxiv.org/pdf/1904.08064.pdf
  # http://www.ihes.fr/~/ruelle/PUBLICATIONS/%5B92%5D.pdf
  
  if(!is.data.frame(data)) 
    stop("`data` should be a data.frame.")
  if(!all(c("id", "date", "value") %in% names(data)))
    stop("`data` must have names columns: id, date, value")
  
  if(!is.logical(absolute))
    stop("`absolute` should be either TRUE or FALSE")
  
  size <- size[1]
  if(!(size %in% c("triangle", "square")))
    stop("`size` has to be one of: triangle, square")
  
  shape <- shape[1]
  if(!(shape %in% c("wide", "long")))
    stop("`shape` has to be one of: wide, long")
  
  # define all months which define how many "pixels" we create for each id
  # TODO: Check correctness of date format
  if(is.na(start_date)) start_date <- min(data$date)
  if(is.na(end_date)) end_date <- max(data$date)
  if(end_date <= start_date) 
    stop("The implied end_date is not later than the implied start_date")
  all_dates <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  
  data$id <- as.character(data$id)
  all_ids <- unique(data$id)
  
  # TODO: Remove dependencies on dplyr and tidyr
  
  # create a combination of all dates with all dates (i.e., square images)
  date_size <- tidyr::expand_grid(date_x = all_dates, date_y = all_dates)
  
  if(size == "triangle") {
    # then drop the upper triangle of the square as its a duplicate
    date_size <- dplyr::filter(date_size, date_y > date_x)
  }
  
  # also create all IDs with all dates against which we join the date_date table
  id_date <- tidyr::expand_grid(id = all_ids, date_x = all_dates)
  
  # and finally create a table that has each combination of dates for each id
  id_date_size <- dplyr::inner_join(id_date, date_size, by = "date_x")
  
  # now join the actual time series observation for each id and dates to this
  # to be able to compute the recurrence distance
  id_recurrence <- id_date_size %>%
    dplyr::left_join(data, by = c(id = "id", date_x = "date")) %>%
    dplyr::rename(value_x = value) %>%
    dplyr::mutate(value_x = ifelse(is.na(value_x), na_replacement, value_x)) %>%
    dplyr::left_join(data, by = c(id = "id", date_y = "date")) %>%
    dplyr::rename(value_y = value) %>%
    dplyr::mutate(value_y = ifelse(is.na(value_y), na_replacement, value_y))
  
  if(absolute) {
    id_recurrence$recurrence <- abs(id_recurrence$value_y - 
                                      id_recurrence$value_x)
  } else {
    id_recurrence$recurrence <- (id_recurrence$value_y - id_recurrence$value_x)
  }
  
  # normalize the recurrence between 0 and 1 to make it comparable across series
  # then spread the table in order to have feature columns for UMAP unless
  # shape is set to long instead (which is good for ggplot)
  # TODO: Offer different kinds of standardization
  id_recurrence <- id_recurrence %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(recurrence = (recurrence - min(recurrence)) / 
                    (max(recurrence) - min(recurrence))) %>%
    dplyr::mutate(index = paste0("v_", 1:dplyr::n())) %>%
    dplyr::ungroup() 
  
  if(shape == "wide") {
    id_recurrence <- tidyr::spread(id_recurrence[c("id", "index", "recurrence")],
                                   index, recurrence)
  }
  
  # TODO: Make S3 Object
  return(list(recurrence_table = id_recurrence,
              absolute = absolute, size = size, shape = shape,
              start_date = start_date, end_date = end_date))
}
