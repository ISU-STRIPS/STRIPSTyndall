library(dplyr)
library(tidyr)


# Read in crop revenue sheet which only contains revenue for the crop portion
# of the watershed
revenue <- readxl::read_xlsx(path = "Sept 22 Strips revenue update.xlsx",
                                 sheet = "New Revenue - Strips 2007-15",
                                 range = "A5:J16",
                                 col_names = c("watershed", 2007:2015)) %>%
  gather(year, revenue, -watershed) %>%
  mutate(year = as.numeric(year),
         watershed = factor(watershed))

devtools::use_data(revenue)
