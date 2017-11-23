library(tidyverse)
library(randomNames)

rm(list = ls())

random_phones <- function(n) {
  sprintf("020-%04d-%04d", sample(0001:9999, n), sample(0001:9999, n))
}

random_postcodes <- function(n) {
  districts <- c("EC", "WC", "E", "N", "NW", "SE", "SW", "W")
  sprintf("%s%d %d%s%s",
          sample(districts, n, replace = TRUE),
          sample(11:25, n, replace = TRUE),
          sample(1:9, n, replace = TRUE),
          sample(LETTERS, n, replace = TRUE),
          sample(LETTERS, n, replace = TRUE))
}

generate_dataset <- function(n, duplicates = 0, seed = 0) {
  if (seed) {
    set.seed(seed)
  }

  dataset <- randomNames(n, return.complete.data = TRUE)

  if (duplicates) {
    duplicate_rows <- sample_n(dataset, duplicates)
    dataset <- dataset %>%
      anti_join(duplicate_rows, by = c("last_name", "first_name"))

    dataset <- bind_rows(dataset, duplicate_rows, duplicate_rows)
  }

  n <- nrow(dataset)

  dataset %>%
    mutate(Phone = random_phones(n)) %>%
    mutate(Postcode = random_postcodes(n)) %>%
    mutate(Middle = sample(LETTERS, n)) %>%
    rename(First = first_name, Last = last_name) %>%
    arrange(Last, First, Middle) %>%
    mutate(ID = 1000 + row_number())
}

people <- generate_dataset(6, 2, 444444)

phones <- people %>%
  select(ID, First, Middle, Last, Phone) %>%
  sample_n(nrow(people) -2) %>%
  arrange(ID)

phones %>%
  write_csv("./data/phones.csv")

postcodes <- people %>%
  select(GivenName = First, Middle, Surname = Last, Postcode) %>%
  sample_n(nrow(people) -3) %>%
  arrange(GivenName, Middle, Surname)

postcodes %>%
  write_csv("./data/postcodes.csv")

presidents <- read_csv("./data/USPresident-Wikipedia-URLs-Thmbs-HS.csv") %>%
  select(President, TookOffice = `Took office`, LeftOffice = `Left office`, HomeState = `Home State`) %>%
  extract(President, c("First", "Last"), regex = "(.*)\\s(.*)") %>%
  separate(First, c("First", "Middle"), extra = "merge")

home_states <- presidents %>%
  select(GivenName = First, Middle, Surname = Last, HomeState) %>%
  filter(row_number() %in% c(6,27,32,41,43)) %>%
  sample_n(nrow(.))

home_states

home_states %>%
  write_csv("data/presidents_home.csv")

presidents <- presidents %>%
  select(First, Middle, Last, TookOffice, LeftOffice) %>%
  filter(row_number() %in% c(32,34,35,41,43)) %>%
  sample_n(nrow(.))

presidents

presidents %>%
  write_csv("data/presidents.csv")
