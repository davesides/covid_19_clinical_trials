# an R script showing how what could be done manually in
# Excel can be scripted for repeating by others
# 2020-05-16 D. Sides (david@sidesways.com)

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(lubridate, warn.conflicts = FALSE)
library(stringr)
library(readr)
library(writexl)
library(ggplot2)

source("covid_19_clinical_trials.R")

CLINICAL_TRIALS_SEARCH <- paste0(DATA,"COVID-19_Clinical_Trials_Search_Results.tsv")
XLSX_OUTPUT_FILE <- paste0(OUTPUT, "COVID-19_R_Script_Results.xlsx")

# the "month year" date format (e.g. "April 2020") is not handled well
# by lubridate (or anytime)
# this is a vectorized function that is called within a mutate()
# to handle these types of dates converting them to the first day
# of the month (e.g. "April 2020" becomes "2020-04-01")
# this is not robust since it uses the word count (== 2) and
# assumes this is a "month year", which works for this dataset (for now)
# note that UTC is returned by default and it's close enough
# for this analysis so UTC is stripped from the output

cleanDate <- function(char_date) {
  new_char_date <- if_else(str_count(char_date, "\\S+") == 2,
          paste0(word(char_date,2),"-",
                 match(word(char_date,1),month.name),"-01"),
          char_date)
  return(str_replace(parse_date_time(new_char_date, orders = c("m d, y", "y-m-d"), tz="UTC")," UTC",""))
}

# To generate the TSV file: https://clinicaltrials.gov/ct2/results?cond=COVID-19
trials_search_results <- read_tsv(CLINICAL_TRIALS_SEARCH,
                                  guess_max = 30000,
                                  quote = "",
                                  col_types = cols(),   # suppresses warnings on column guesses
                                  trim_ws = TRUE)

# there are more efficient ways to order this, but this shows some
# capabilities like removing columns and select everything()
trials_cleaned <- trials_search_results %>%
  select("Title",
         "Status",
         "Start Date",
         "Primary Completion Date",
         "Completion Date",
         "First Posted",
         "Last Update Posted",
         "Age",
         "Study Results",
         "Interventions",
         "Outcome Measures",
         "Locations",
         "Age") %>% 
  mutate(StartDate = ymd(cleanDate(`Start Date`)),
         PrimaryCompletionDate = ymd(cleanDate(`Primary Completion Date`)),
         CompletionDate = ymd(cleanDate(`Completion Date`)),
         FirstPosted = ymd(cleanDate(`First Posted`)),
         LastUpdatePosted = ymd(cleanDate(`Last Update Posted`)),
         PrimaryCompletionMonth = month(PrimaryCompletionDate),
         PrimaryCompletionYear = year(PrimaryCompletionDate)) %>% 
  select("PrimaryCompletionYear",
         "PrimaryCompletionMonth",
         everything(),
         -c("Start Date",
            "Primary Completion Date",
            "Completion Date",
            "First Posted",
            "Last Update Posted")
         ) %>% 
  filter(!(is.na(PrimaryCompletionYear) | is.na(PrimaryCompletionMonth))) %>% 
  filter((StartDate <= today() & ((PrimaryCompletionYear >= 2020) & 
                                    (PrimaryCompletionYear <= 2023)))) %>% 
  filter(Status != "Not yet recruiting") %>% 
  arrange(PrimaryCompletionYear, PrimaryCompletionMonth, StartDate)

trials_summary <- trials_cleaned %>% 
  group_by(Status) %>% 
  summarize(Completing_in_2020 = sum(if_else(PrimaryCompletionYear == 2020,1,0)),
            Completing_in_2021 = sum(if_else(PrimaryCompletionYear == 2021,1,0)),
            Completing_in_2022 = sum(if_else(PrimaryCompletionYear == 2022,1,0)),
            Completing_in_2023 = sum(if_else(PrimaryCompletionYear == 2023,1,0))
            ) %>% 
  replace_na(list(Completing_in_2020 = 0,
                  Completing_in_2021 = 0
                  # Completing_in_2022 = 0,
                  # Completing_in_2023 = 0
                  ))

# save multiple worksheets in Excel for ease of exploration
worksheets <- list("Trials Summary" = trials_summary,
                   "Trials Search Cleaned" = trials_cleaned,
                   "Trials Search Original" = trials_search_results)
tmp <- writexl::write_xlsx(worksheets, XLSX_OUTPUT_FILE)
