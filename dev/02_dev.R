# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "initialize", with_test = TRUE)
golem::add_module(name = "welcome", with_test = TRUE)
golem::add_module(name = "browser", with_test = TRUE)
golem::add_module(name = "classification_1", with_test = TRUE)
golem::add_module(name = "testing_suite", with_test = TRUE)
# golem::add_module(name = "calgary_rentals", with_test = TRUE) # REMOVED
# golem::add_module(name = "guitar", with_test = TRUE) # REMOVED
golem::add_module(name = "demo1", with_test = TRUE)
golem::add_module(name = "lessons", with_test = TRUE)

# Name of the module
# golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

usethis::use_pipe()

usethis::use_package("bslib")
usethis::use_package("RSelenium")
usethis::use_package("base64enc")
usethis::use_package("bslib")
usethis::use_package("config")
usethis::use_package("DT")
usethis::use_package("fs")
usethis::use_package("golem")
usethis::use_package("htmltools")
usethis::use_package("magrittr")
usethis::use_package("OpenImageR")
usethis::use_package("purrr")
usethis::use_package("quarto")
usethis::use_package("R6")
usethis::use_package("RSelenium")
usethis::use_package("rvest")
usethis::use_package("shiny")
usethis::use_package("shinypanel")
usethis::use_package("stringr")
usethis::use_package("xml2")

## Add helper functions ----
## Creates fct_* and utils_*
# golem::add_fct("helpers", with_test = TRUE)
# golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file("script")
# golem::add_js_handler("handlers")
# golem::add_css_file("custom")
# golem::add_sass_file("custom")
# golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw(name = "Raw_Data", open = FALSE)

## Tests ----
## Add one line by test you want to create
# usethis::use_test("app")

# Documentation

## Vignette ----
# usethis::use_vignette("Webglace")
# devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
# usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
# usethis::use_github()

# GitHub Actions
# usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release()
# usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full()
# Add action for PR
# usethis::use_github_action_pr_commands()

# Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()

# Jenkins
# usethis::use_jenkins()

# GitLab CI
# usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
# rstudioapi::navigateToFile("dev/03_deploy.R")
