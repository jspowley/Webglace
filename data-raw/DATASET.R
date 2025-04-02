## code to prepare `DATASET` dataset goes here

## Attempts at rentfaster scraping

card_scroll <- readRDS("data-raw/rent/card_scrollbar.rds")
usethis::use_data(card_scroll, overwrite = TRUE)

filter_button <- readRDS("data-raw/rent/filter_button.rds")
usethis::use_data(filter_button, overwrite = TRUE)

list_button <- readRDS("data-raw/rent/list_view.rds")
usethis::use_data(list_button, overwrite = TRUE)

# Sidebar filter classes

laundry <- readRDS("data-raw/rent/laundry.rds")
usethis::use_data(laundry, overwrite = TRUE)

apartment <- readRDS("data-raw/rent/apartment.rds")
usethis::use_data(apartment, overwrite = TRUE)

two_bed <- readRDS("data-raw/rent/two_bed.rds")
usethis::use_data(two_bed, overwrite = TRUE)

# Popup filter classes

laundry2 <- readRDS("data-raw/rent/laundry2.rds")
usethis::use_data(laundry2, overwrite = TRUE)

apartment2 <- readRDS("data-raw/rent/apartment2.rds")
usethis::use_data(apartment2, overwrite = TRUE)

two_bed2 <- readRDS("data-raw/rent/two_bed2.rds")
usethis::use_data(two_bed2, overwrite = TRUE)

search_button <- readRDS("data-raw/rent/search.rds")
usethis::use_data(search_button, overwrite = TRUE)
