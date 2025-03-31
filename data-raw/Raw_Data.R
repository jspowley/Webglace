## code to prepare `my_dataset` dataset goes here
logo <- OpenImageR::readImage("data-raw/hex-Webglace.png")

usethis::use_data(logo, overwrite = TRUE)

# RentFaster
address_input <- readRDS("data-raw/rentfaster/address_input.rds")
usethis::use_data(address_input, overwrite = TRUE)
apartment_button <- readRDS("data-raw/rentfaster/apartment_button.rds")
usethis::use_data(apartment_button, overwrite = TRUE)
close_filters <- readRDS("data-raw/rentfaster/close_filters.rds")
usethis::use_data(close_filters, overwrite = TRUE)
filter_button <- readRDS("data-raw/rentfaster/filter_button.rds")
usethis::use_data(filter_button, overwrite = TRUE)
laundry_in_suite <- readRDS("data-raw/rentfaster/laundry.rds")
usethis::use_data(laundry_in_suite, overwrite = TRUE)
two_bed <- readRDS("data-raw/rentfaster/two_bed.rds")
usethis::use_data(two_bed, overwrite = TRUE)

