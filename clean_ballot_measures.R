library(tidyverse)

output_dir <- file.path("output")

proposal_folders <- list.dirs(output_dir, recursive = FALSE, full.names = TRUE) %>%
  tibble(path = .) %>%
  mutate(folder = basename(path)) %>%
  filter(startsWith(folder, "Proposal Number")) %>%
  pull(path)

read_data <- function(path) {
  number <- str_extract(path, "\\d+")
  print(number)
  data <- read_csv(paste0(path, "/borough_proposal_number_", number, ".csv"))
  print(data)
  data_clean <- data %>% 
    mutate(proposal_number = number) %>% 
    filter(boro == "Total")
  print(data_clean)
}

#read_data(proposal_folders[1])

clean_ballot_data <- map_df(proposal_folders, ~read_data(.x)) %>% 
  pivot_wider(names_from = "candidate", values_from = "votes") %>% 
  mutate(percentage = YES/(YES+NO)*100,
         passed = if_else(YES>NO,TRUE, FALSE))

write_csv(clean_ballot_data, "analysis/clean_ballot_data.csv")
