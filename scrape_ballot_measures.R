library(rvest)
library(tidyverse)
library(purrr)

links <- read_csv("output/races_master_list.csv")

ballot_links <- links %>% 
  filter(str_detect(race, "Proposal Number"))

ballot_urls <- ballot_links %>% filter(level == "By AD") %>% pull(link)
ballot1_page <- read_page_results(ballot_urls[1])
ballot1_results <- process_results_table(ballot1_page) #modification for not party?
write_csv(ballot1_results, "output/ballot1/borough.csv")
ballot1_links <- get_page_links(ballot1_page)
ballot1_AD_link <- ballot1_links %>% filter(title == "Total") %>% pull(link)
ballot1_AD_page <- read_page_results(ballot1_AD_link)
ballot1_AD_results <- process_results_table(ballot1_AD_page)
write_csv(ballot1_AD_results, "output/ballot1/ad.csv")
ballot1_ED_links <- get_page_links(ballot1_AD_page)
ballot1_results_ED <- map2_df(ballot1_ED_links$link, ballot1_ED_links$title, ~mutate(process_results_table(read_page_results(.x)), AD_num = .y))
write_csv(ballot1_results_ED, "output/ballot1/ed.csv")

race_ads <- links %>% filter(level == "By AD")

# Create each folder if it doesn't exist using walk
walk(race_ads$race, ~ if (!dir.exists(paste0("output/", .x))) {
  dir.create(paste0("output/", .x))
  message(paste("Created folder:", .x))
} else {
  message(paste("Folder already exists:", .x))
})


write_race_data <- function(race_url, race, sub_race) {
  print(paste0("scraping data for ", race))
  
  time <- str_replace_all(now(), " ", "_")
  race_stub <- str_replace_all(race, " ", "_") %>% str_to_lower()
  page <- read_page_results(race_url)
  
  boro_results <- process_results_table(page) %>% 
    mutate(scrape_time = time)#modification for not party?
  if (sub_race != "") {
    write_csv(boro_results, paste0("output/", race, "/", sub_race, "/borough_", race_stub, ".csv"))
  } else {
    write_csv(boro_results, paste0("output/", race, "/borough_", race_stub, ".csv"))
    }
  print("Wrote out borough data")
  
  ad_links <- get_page_links(page)
  
  ad_link <- ad_links %>% filter(title == "Total") %>% pull(link)
  ad_page <- read_page_results(ad_link)
  ad_results <- process_results_table(ad_page) %>% 
    mutate(scrape_time = time)
  if (sub_race != "") {
    write_csv(ad_results, paste0("output/", race, "/", sub_race, "/ad_", race_stub, ".csv"))
  } else {
    write_csv(ad_results, paste0("output/", race, "/ad_", race_stub, ".csv"))
  }
  print("Wrote out AD data")
  
  ed_links <- get_page_links(ad_page)
  
  ed_results <- map2_df(ed_links$link, ed_links$title,
                                ~mutate(process_results_table(read_page_results(.x)),
                                        AD_num = .y,
                                        scrape_time = time))
  if (sub_race != "") {
    write_csv(ed_results, paste0("output/", race, "/", sub_race, "/ed_", race_stub, ".csv"))
  } else {
    write_csv(ed_results, paste0("output/", race, "/ed_", race_stub, ".csv"))
    }
  print("Wrote out ED data")
}



scrape_race <- function(race, race_url) {
  page <- read_page_results(race_url)
  sub_links <- get_page_links(page)
  
  if("Total" %in% sub_links$title) {
    write_race_data(race_url, race, sub_race = "")
  } else {

    subraces <- sub_links$title
    sublinks <- sub_links$link
    
    # Create each folder if it doesn't exist using walk
    walk(subraces, ~ if (!dir.exists(paste0("output/",race,"/", .x))) {
      dir.create(paste0("output/",race, "/", .x))
      message(paste("Created folder:", .x))
    } else {
      message(paste("Folder already exists:", .x))
    })
    
    walk2(.x = sublinks, .y = subraces, ~write_race_data(race_url = .x, race = race, sub_race = .y))
  }
}

walk2(race_ads$race, race_ads$link, ~scrape_race(.x,.y))

scrape_race("Representative in Congress", "https://enr.boenyc.gov/OF3AD0PY8.html")
