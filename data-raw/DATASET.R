## code to prepare `DATASET` dataset goes here

## Attempts at rentfaster scraping

post <- readRDS("data-raw/demo1/post.rds")
usethis::use_data(post, overwrite = TRUE)

post_title <- readRDS("data-raw/demo1/post_title.rds")
usethis::use_data(post_title, overwrite = TRUE)
