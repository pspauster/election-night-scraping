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
}

prez_page <- read_page_results(prez_url)

process_results_table <- function(page_tables) {
  results_table <- page_tables %>% .[3] %>% html_table(fill = F) %>% as.data.frame()
  new_colnames <- paste(results_table[1, ], results_table[2, ], sep = "_")
  if("Queens" %in% results_table[[1]]) {
    new_colnames[1] <- "boro"
  } else if("AD" %in% str_sub(results_table[[1]],1,2)) {
    new_colnames[1] <- "AD_num"
  } else if("ED" %in% str_sub(results_table[[1]],1,2)) {
    new_colnames[1] <- "ED_num"
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

prez_results_boro <-process_results_table(prez_page)

write_csv(prez_results_boro, "output/presidential/borough.csv")

get_page_links <- function(page_tables) {
  links <- page_tables[3] %>%
  html_elements("tr") %>%
  map(~ .x %>%
        html_elements("td, th") %>%
        map_chr(~ as.character(.x))
  )
  
  map_dfr(links, extract_links) %>% 
    filter(!is.na(href)) %>% 
    mutate(link = paste0("https://enr.boenyc.gov/",href))
}

prez_results_links <- get_page_links(prez_page)

# GET AD RESULTS from totals page

prez_AD_link <- prez_results_links %>% filter(title == "Total") %>% pull(link)

prez_ad_page <- read_page_results(prez_AD_link)

prez_results_AD <- process_results_table(prez_ad_page)

write_csv(prez_results_AD, "output/presidential/ad.csv")

prez_AD_result_links <- get_page_links(prez_ad_page)

### List through all the EDs
#need to fix because there are duplicate ED numbers for every AD + total columns are not specified.
prez_results_ED <- map2_df(prez_AD_result_links$link, prez_AD_result_links$title, ~mutate(process_results_table(read_page_results(.x)), AD_num = .y))
write_csv(prez_results_ED, "output/presidential/ed.csv")



