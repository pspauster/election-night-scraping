library(rvest)
library(tidyverse)
library(purrr)

# Set the URL
url <- "https://enr.boenyc.gov/index.html"

# Read the HTML of the page
webpage <- read_html(url)

# Extract tables
tables <- webpage %>%
  html_nodes("table")

# Assign Results
results_table <- tables %>% .[3] %>% html_table(fill = F) %>% as.data.frame()

data <- tables[3] %>%
  html_elements("tr") %>%
  map(~ .x %>%
        html_elements("td, th") %>%
        map_chr(~ as.character(.x))
  )

extract_links <- function(row) {
  # Extract the 'title' and 'href' attributes for each cell with an <a> tag
  titles <- map_chr(row, ~ {
    cell <- read_html(.x) %>% html_element("a")
    if (!is.null(cell)) cell %>% html_attr("title") else NA_character_
  })
  
  hrefs <- map_chr(row, ~ {
    cell <- read_html(.x) %>% html_element("a")
    if (!is.null(cell)) cell %>% html_attr("href") else NA_character_
  })
  
  # Return a data frame with the extracted titles and hrefs for each cell
  data.frame(title = titles, href = hrefs, stringsAsFactors = FALSE)
}

# Example input: assume `data` is a list of rows with HTML content in each cell
# (Replace `data` below with your actual list of rows)
# data <- list(cells_row_1, cells_row_2, ...)

# Apply the function to each row
result <- map_dfr(data, extract_links) %>%
  filter(!is.na(href)) %>% 
  separate(title, sep = ": ", into = c("level", "race"), remove = T) %>% 
  mutate(link = paste0("https://enr.boenyc.gov/",href))

write_csv(result, "output/races_master_list.csv")


