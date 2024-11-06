library(rvest)
library(dplyr)

# Set the URL
url <- "https://enr.boenyc.gov/index.html"

# Read the HTML of the page
webpage <- read_html(url)

# Extract tables
tables <- webpage %>% html_nodes("table")


results_table <- tables %>% .[3] %>% html_table(fill = TRUE) %>% as.data.frame()

