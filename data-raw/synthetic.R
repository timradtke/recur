
# create a synthetic data set that represents a set of monthly time series
# with 4 years of observations each

synthetic <- data.frame(
  id = rep(paste0("ts-", 100:999), each = 48),
  date = rep(seq(as.Date("2016-01-01"), as.Date("2019-12-01"), by = "month"), 
             times = 999 - 100 + 1),
  value = rnorm(48 * (999-100+1)),
  stringsAsFactors = FALSE
)

usethis::use_data(synthetic, overwrite = TRUE)
