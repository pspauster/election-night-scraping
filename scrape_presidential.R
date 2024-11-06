library(rvest)
library(tidyverse)

links <- read_csv("output/races_master_list.csv")

prez_links <- links %>% 
  filter(race == "President/Vice President")

prez_url <- prez_links %>% filter(level == "By AD") %>% pull(link)

read_page_results <- function(url) {
  webpage <- read_html(url)
  
  tables <- webpage %>% 
    html_nodes("table")
  
  process_results_table(tables)
}

process_results_table <- function(page_tables) {
  results_table <- page_tables %>% .[3] %>% html_table(fill = F) %>% as.data.frame()
  new_colnames <- paste(results_table[1, ], results_table[2, ], sep = "_")
  if("Queens" %in% results_table[[1]]) {
    new_colnames[1] <- "boro"
  }
  names(results_table) <- new_colnames
  results_table <- results_table[-c(1, 2), ] %>%
    select(-starts_with("NA_NA")) %>% 
    pivot_longer(cols = 3:ncol(.), 
                 names_to = "candidate_party", 
                 values_to = "votes") %>%
    # Separate candidate and party based on underscore "_"
    separate(candidate_party, into = c("candidate", "party"), sep = "_", extra = "merge") %>% 
    rename(reported = 2)
}

prez_results_boro <- read_page_results(prez_tables)

write_csv(prez_results_boro, "output/presidential/borough.csv")

prez_boro_links <- prez_tables[3] %>%
  html_elements("tr") %>%
  map(~ .x %>%
        html_elements("td, th") %>%
        map_chr(~ as.character(.x))
  )

prez_results_links <- map_dfr(prez_boro_links, extract_links) %>% 
  filter(!is.na(href)) %>% 
  mutate(link = paste0("https://enr.boenyc.gov/",href))
