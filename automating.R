library(rmarkdown)
library(tidyverse)
weekdays <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
params <- lapply(weekdays, FUN = function(x){list(weekday = x)})
reports <- tibble(paste0(weekdays, ".md"), params)

apply(reports, MARGIN = 1, FUN = function(x){
  render(input = "README.Rmd", output_file = x[[1]], params = x[[2]])
})
