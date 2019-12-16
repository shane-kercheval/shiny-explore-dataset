library('testthat')
library(tidyverse)
library(rtools)
library(hms)

source('../helper_scripts/generic_helpers.R', chdir = TRUE)
source('../helper_scripts/definitions.R', chdir = TRUE)
source('../helper_scripts/logging_functions.R', chdir = TRUE)
source('../helper_scripts/variable_plots_helpers.R', chdir = TRUE)
source('../helper_scripts/dataset_loading_helpers.R', chdir = TRUE)
source('../helper_scripts/plot_helpers.R', chdir = TRUE)
Sys.setenv(TZ='UTC')

global__should_log_message <<- FALSE

# to run from command line, use:
# test_file("unit_tests.R")

test_that("filter", {
    context("generic_helpers::filter_data")

    dataset <- data.frame(diamonds)
    dataset[1:500, 'carat'] <- NA
    dataset[501:1000, 'cut'] <- NA
    dataset[1001:1500, 'color'] <- NA
    
    # build filter list
    # build filter selection list (to mimic shiny and also reuse list)
    global_filter_list <- list(
        carat = c(0.5, 2),
        cut = c('Ideal', 'Premium', 'Good'),
        color = NULL,
        price = c(326, 18823)
    )

    filter_selections <- c()
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expect_true(rt_are_dataframes_equal(dataset, filter_results[[1]]))
    expect_equal(length(filter_results[[2]]), 0)
    
    filter_selections <- NULL
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expect_true(rt_are_dataframes_equal(dataset, filter_results[[1]]))
    expect_equal(length(filter_results[[2]]), 0)
    
    ##########################################################################################################
    filter_selections <- c('carat')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2)
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    ##########################################################################################################
    filter_selections <- c('carat', 'cut')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2,
                                            cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    expect_true(all(filter_results[[1]]$cut %in% c('Ideal', 'Premium', 'Good')))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    # caret should have the same checks as above since it is the first filter
    # more than 19443 values will be removed from other filters, but not from the carat filter directly
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))

    ##########################################################################################################
    filter_selections <- c('carat', 'cut')
    filter_results <- filter_data(dataset=dataset %>% mutate(cut = as.character(cut)),  # change to character
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2,
                                            cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    expect_true(all(filter_results[[1]]$cut %in% c('Ideal', 'Premium', 'Good')))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    # caret should have the same checks as above since it is the first filter
    # more than 19443 values will be removed from other filters, but not from the carat filter directly
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    
    ##########################################################################################################
    filter_selections <- c('carat', 'cut', 'color')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    # same as above since color is NULL and therefore even though there are NULLs we are not filtering tem
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2,
                                            cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    expect_true(all(filter_results[[1]]$cut %in% c('Ideal', 'Premium', 'Good')))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    # caret should have the same checks as above since it is the first filter
    # more than 19443 values will be removed from other filters, but not from the carat filter directly
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    
    expect_equal(filter_results[[2]][[3]], "color: Not Filtering")

    ##########################################################################################################
    filter_selections <- c('carat', 'cut', 'color', 'price')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    # still the same as above since price doesn't have any NAs and the filter values include min/max i.e. all
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2,
                                            cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    expect_true(all(filter_results[[1]]$cut %in% c('Ideal', 'Premium', 'Good')))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    # caret should have the same checks as above since it is the first filter
    # more than 19443 values will be removed from other filters, but not from the carat filter directly
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    
    expect_equal(filter_results[[2]][[3]], "color: Not Filtering")

    expect_true(str_detect(filter_results[[2]][[4]], "price:"))
    expect_true(str_detect(filter_results[[2]][[4]], "326"))
    expect_true(str_detect(filter_results[[2]][[4]], "18,823"))
    expect_true(str_detect(filter_results[[2]][[4]], "Removing 0 rows"))
    
    ##########################################################################################################
    filter_selections <- c('carat', 'cut', 'price')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    # still the same as above since price doesn't have any NAs and the filter values include min/max i.e. all
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2,
                                            cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    expect_true(all(filter_results[[1]]$cut %in% c('Ideal', 'Premium', 'Good')))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    # caret should have the same checks as above since it is the first filter
    # more than 19443 values will be removed from other filters, but not from the carat filter directly
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    

    expect_true(str_detect(filter_results[[2]][[3]], "price:"))
    expect_true(str_detect(filter_results[[2]][[3]], "326"))
    expect_true(str_detect(filter_results[[2]][[3]], "18,823"))
    expect_true(str_detect(filter_results[[2]][[3]], "Removing 0 rows"))
    ##########################################################################################################
    filter_selections <- c('color', 'price')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    # not actually filtering anything out (color is NULL and price is min/max of column)
    expect_true(rt_are_dataframes_equal(dataset, filter_results[[1]]))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    expect_equal(filter_results[[2]][[1]], "color: Not Filtering")

    expect_true(str_detect(filter_results[[2]][[2]], "price:"))
    expect_true(str_detect(filter_results[[2]][[2]], "326"))
    expect_true(str_detect(filter_results[[2]][[2]], "18,823"))
    expect_true(str_detect(filter_results[[2]][[2]], "Removing 0 rows"))
})

test_that("filter: missing_values_options", {
    context("generic_helpers::filter_data w/ <Missing Values (NA)>")
    
    missing_value_string <- '<Missing Values (NA)>'
    
    dataset <- data.frame(diamonds)
    dataset[1:500, 'carat'] <- NA
    dataset[501:1000, 'cut'] <- NA
    dataset[1001:1500, 'color'] <- NA
    
    # build filter list
    # build filter selection list (to mimic shiny and also reuse list)
    global_filter_list <- list(
        carat = c(0.5, 2),
        cut = c(missing_value_string, 'Ideal', 'Premium', 'Good'),
        color = NULL,
        price = c(326, 18823)
    )
    
    filter_selections <- c()
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expect_true(rt_are_dataframes_equal(dataset, filter_results[[1]]))
    expect_equal(length(filter_results[[2]]), 0)
    
    filter_selections <- NULL
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expect_true(rt_are_dataframes_equal(dataset, filter_results[[1]]))
    expect_equal(length(filter_results[[2]]), 0)
    
    ##########################################################################################################
    filter_selections <- c('carat')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2)
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    ##########################################################################################################
    filter_selections <- c('cut')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    
    categories_kept <- global_filter_list$cut %>% rt_remove_val(missing_value_string)
    
    expected_filtered <- dataset %>% filter(is.na(cut) | cut %in% categories_kept)
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_equal(sum(is.na(filter_results[[1]]$cut)), 500)
    
    unique_cut_values <- unique(filter_results[[1]]$cut)
    expect_true(any(is.na(unique_cut_values)))
    expect_true(setequal(unique_cut_values %>% rt_remove_val(NA), categories_kept))

    num_filtered_out <- sum(!is.na(dataset$cut) & !(dataset$cut %in% categories_kept))
    
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[1]], "cut:"))
    expect_true(grepl(paste0(global_filter_list$cut, collapse = ', '), filter_results[[2]][[1]], fixed=TRUE))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))

    ##########################################################################################################
    filter_selections <- c('carat', 'cut')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2,
                                            is.na(cut) | cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    expect_true(all(is.na(filter_results[[1]]$cut) | filter_results[[1]]$cut %in% c('Ideal', 'Premium', 'Good')))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    # caret should have the same checks as above since it is the first filter
    # more than 19443 values will be removed from other filters, but not from the carat filter directly
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    
    categories_kept <- global_filter_list$cut %>% rt_remove_val(missing_value_string)
    unique_cut_values <- unique(filter_results[[1]]$cut)
    expect_true(any(is.na(unique_cut_values)))
    expect_true(setequal(unique_cut_values %>% rt_remove_val(NA), categories_kept))
    
    num_filtered_out <- sum(!is.na(t $cut) & !(t $cut %in% categories_kept))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(grepl(paste0(global_filter_list$cut, collapse = ', '), filter_results[[2]][[2]], fixed=TRUE))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    carat_cut_factor_results <- filter_results[[2]][[2]]
    ##########################################################################################################
    filter_selections <- c('carat', 'cut')
    filter_results <- filter_data(dataset=dataset %>% mutate(cut = as.character(cut)),  # change to character
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2,
                                            is.na(cut) | cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    expect_true(all(is.na(filter_results[[1]]$cut) | filter_results[[1]]$cut %in% c('Ideal', 'Premium', 'Good')))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    # caret should have the same checks as above since it is the first filter
    # more than 19443 values will be removed from other filters, but not from the carat filter directly
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    
    categories_kept <- global_filter_list$cut %>% rt_remove_val(missing_value_string)
    unique_cut_values <- unique(filter_results[[1]]$cut)
    expect_true(any(is.na(unique_cut_values)))
    expect_true(setequal(unique_cut_values %>% rt_remove_val(NA), categories_kept))
    
    num_filtered_out <- sum(!is.na(t $cut) & !(t $cut %in% categories_kept))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(grepl(paste0(global_filter_list$cut, collapse = ', '), filter_results[[2]][[2]], fixed=TRUE))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    expect_equal(carat_cut_factor_results, filter_results[[2]][[2]])
    ##########################################################################################################
    filter_selections <- c('carat', 'cut', 'color')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    # same as above since color is NULL and therefore even though there are NULLs we are not filtering tem
    expected_filtered <- dataset %>% filter(!is.na(carat),
                                            carat >= 0.5,
                                            carat <= 2,
                                            is.na(cut) | cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$carat < 0.5))
    expect_false(any(filter_results[[1]]$carat > 2))
    expect_true(all(is.na(filter_results[[1]]$cut) | filter_results[[1]]$cut %in% c('Ideal', 'Premium', 'Good')))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    
    # caret should have the same checks as above since it is the first filter
    # more than 19443 values will be removed from other filters, but not from the carat filter directly
    num_na <- sum(is.na(dataset$carat))
    num_filtered_out <- sum(dataset$carat < 0.5 | dataset$carat > 2, na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "carat:"))
    expect_true(str_detect(filter_results[[2]][[1]], "0.5"))
    expect_true(str_detect(filter_results[[2]][[1]], "2"))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    
    categories_kept <- global_filter_list$cut %>% rt_remove_val(missing_value_string)
    unique_cut_values <- unique(filter_results[[1]]$cut)
    expect_true(any(is.na(unique_cut_values)))
    expect_true(setequal(unique_cut_values %>% rt_remove_val(NA), categories_kept))
    
    num_filtered_out <- sum(!is.na(t $cut) & !(t $cut %in% categories_kept))
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(grepl(paste0(global_filter_list$cut, collapse = ', '), filter_results[[2]][[2]], fixed=TRUE))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    expect_equal(carat_cut_factor_results, filter_results[[2]][[2]])
    
    expect_equal(filter_results[[2]][[3]], "color: Not Filtering")
})

test_that("filter: missing_values_options_logical", {
    context("generic_helpers::missing_values_options_logical")
  
    # originally did not include logical types in the code when creating filter controls and filtering NA values
    missing_value_string <- '<Missing Values (NA)>'
    
    dataset <- dataset_or_null('../example_datasets/credit.csv')
    dataset$default <- ifelse(dataset$default == 'yes', TRUE, FALSE) 
    
    # build filter list
    # build filter selection list (to mimic shiny and also reuse list)
    global_filter_list <- list(
        default = TRUE
    )

    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list,
                                  callback=NULL)
    
    expect_equal(filter_results[[2]][[1]], "default: TRUE (Removing 700 rows)")
    filtered_df <- filter_results[[1]]
    
    expect_true(all(filtered_df$default))
    expect_equal(nrow(filtered_df), 300)
    expect_true(rt_are_dataframes_equal(dataset %>% filter(default), filtered_df))
    
    global_filter_list <- list(
        default = FALSE
    )
    
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list,
                                  callback=NULL)
    
    expect_equal(filter_results[[2]][[1]], "default: FALSE (Removing 300 rows)")
    filtered_df <- filter_results[[1]]
    expect_true(!any(filtered_df$default))
    expect_equal(nrow(filtered_df), 700)
    expect_true(rt_are_dataframes_equal(dataset %>% filter(!default), filtered_df))
    
    
    # now test with NAs
    # make sure this dataset doesn't change
    expect_false(dataset$default[1])
    dataset$default[1] <- NA
    
    global_filter_list <- list(
        default = TRUE
    )
    
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list,
                                  callback=NULL)
    
    expect_equal(filter_results[[2]][[1]], "default: TRUE (Removing 699 rows; Removing 1 rows with missing values)")
    filtered_df <- filter_results[[1]]
    
    expect_true(all(filtered_df$default))
    expect_true(!any(is.na(filtered_df$default)))
    expect_equal(nrow(filtered_df), 300)
    expect_true(rt_are_dataframes_equal(dataset %>% filter(default), filtered_df))
    
    global_filter_list <- list(
        default = FALSE
    )
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list,
                                  callback=NULL)
    
    expect_equal(filter_results[[2]][[1]], "default: FALSE (Removing 300 rows; Removing 1 rows with missing values)")
    filtered_df <- filter_results[[1]]
    expect_true(!any(is.na(filtered_df$default)))
    expect_true(!any(filtered_df$default))
    expect_equal(nrow(filtered_df), 699)
    expect_true(rt_are_dataframes_equal(dataset %>% filter(!default), filtered_df))
    
    global_filter_list <- list(
        default = missing_value_string
    )
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list,
                                  callback=NULL)
    
    expect_equal(filter_results[[2]][[1]], "default: <Missing Values (NA)> (Removing 999 rows)")
    filtered_df <- filter_results[[1]]
    expect_true(all(is.na(filtered_df$default)))
    expect_equal(nrow(filtered_df), 1)
    expect_true(rt_are_dataframes_equal(dataset %>% filter(is.na(default)), filtered_df))
    
    global_filter_list <- list(
        default = c(missing_value_string, FALSE)
    )
    
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list,
                                  callback=NULL)
    
    expect_equal(filter_results[[2]][[1]], "default: <Missing Values (NA)>, FALSE (Removing 300 rows)")
    filtered_df <- filter_results[[1]]
    expect_equal(nrow(filtered_df), 700)
    expect_true(rt_are_dataframes_equal(dataset %>% filter(is.na(default) | !default), filtered_df))
    
    global_filter_list <- list(
        default = c(missing_value_string, TRUE)
    )
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list,
                                  callback=NULL)
    
    expect_equal(filter_results[[2]][[1]], "default: <Missing Values (NA)>, TRUE (Removing 699 rows)")
    filtered_df <- filter_results[[1]]
    expect_equal(nrow(filtered_df), 301)
    expect_true(rt_are_dataframes_equal(dataset %>% filter(is.na(default) | default), filtered_df))
    
    global_filter_list <- list(
        default = c(missing_value_string, TRUE, FALSE)
    )
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list,
                                  callback=NULL)
    
    expect_equal(filter_results[[2]][[1]], "default: <Missing Values (NA)>, TRUE, FALSE (Removing 0 rows)")
    filtered_df <- filter_results[[1]]
    expect_equal(nrow(filtered_df), 1000)
    expect_true(rt_are_dataframes_equal(dataset, filtered_df))
    
})

test_that("generic_helpers::filter_data - flights/date", {
    context("generic_helpers::filter_data - flights/date")
    
    dataset <- data.frame(nycflights13::flights %>%
                              mutate(date = make_date(year, month, day)) %>%
                              select(-year, -month, -day) %>%
                              select(date, everything()))
    
    # test Date, POSIXct, hms
    expect_true(is.Date(dataset$date))
    expect_true(is.POSIXct(dataset$time_hour))
    dataset$hms <- as_hms(with_tz(dataset$time_hour, tzone='UTC'))
    expect_true("hms" %in% class(dataset$hms))
    
    dataset[1:500, 'date'] <- NA
    dataset[501:1000, 'time_hour'] <- NA
    dataset[1001:1500, 'hms'] <- NA
    
    # build filter list
    # build filter selection list (to mimic shiny and also reuse list)
    global_filter_list <- list(
        date = c(ymd('2013-02-05'), ymd('2013-10-31')),
        time_hour = c(ymd('2013-03-05'), ymd('2013-11-30')),
        hms = c("09:01", "18:59")
    )
    ##########################################################################################################
    filter_selections <- c('date')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(date),
                                            date >= global_filter_list[[1]][1],
                                            date <= global_filter_list[[1]][2])
    # filter seems to remove row_names, but it is kept in the filter_data function
    rownames(expected_filtered) <- rownames(filter_results[[1]])
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$date < global_filter_list[[1]][1]))
    expect_false(any(filter_results[[1]]$date > global_filter_list[[1]][2]))
    
    num_na <- sum(is.na(dataset$date))
    num_filtered_out <- sum(dataset$date < global_filter_list[[1]][1] | dataset$date > global_filter_list[[1]][2], na.rm = TRUE)
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[1]], "date:"))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[1]][1])))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[1]][1])))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    writeLines(paste0(filter_results[[2]], collapse = "\n\n"), "output_files/filter_message__flights__date.txt")

    ##########################################################################################################
    filter_selections <- c('time_hour')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>%
        mutate(temp_time_hour = floor_date(time_hour, unit='days')) %>%
        filter(!is.na(temp_time_hour),
               temp_time_hour >= global_filter_list[[filter_selections]][1],
               temp_time_hour <= global_filter_list[[filter_selections]][2]) %>%
        select(-temp_time_hour)
    # filter seems to remove row_names, but it is kept in the filter_data function
    rownames(expected_filtered) <- rownames(filter_results[[1]])
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$time_hour < global_filter_list[[filter_selections]][1]))
    expect_true(floor_date(max(filter_results[[1]]$time_hour, na.rm = TRUE), unit='days') == global_filter_list[[2]][2])
    
    num_na <- sum(is.na(dataset$time_hour))
    time_hour_floor <- floor_date(dataset$time_hour, unit='day')
    num_filtered_out <- sum(time_hour_floor < global_filter_list[[2]][1] | time_hour_floor > global_filter_list[[2]][2], na.rm = TRUE)
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[1]], "time_hour:"))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[2]][1])))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[2]][1])))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    writeLines(paste0(filter_results[[2]], collapse = "\n\n"), "output_files/filter_message__flights__time_hour.txt")
    ##########################################################################################################
    filter_selections <- c('date', 'time_hour')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    
    expected_filtered <- dataset %>%
    #    mutate(time_hour = floor_date(time_hour, unit='days')) %>%
        filter(!is.na(date),
               date >= global_filter_list[[1]][1],
               date <= global_filter_list[[1]][2],
               !is.na(time_hour),
               time_hour >= global_filter_list[[2]][1]
               #time_hour <= global_filter_list[[2]][2]  # don't need this since it is already filtered by date
               )
    rownames(expected_filtered) <- rownames(filter_results[[1]])
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$date < global_filter_list[[1]][1]))
    expect_false(any(filter_results[[1]]$date > global_filter_list[[1]][2]))
    expect_false(any(filter_results[[1]]$time_hour < global_filter_list[[2]][1]))
    expect_false(any(filter_results[[1]]$time_hour > global_filter_list[[2]][2]))

    num_na <- sum(is.na(dataset$date))
    num_filtered_out <- sum(dataset$date < global_filter_list[[1]][1] | dataset$date > global_filter_list[[1]][2], na.rm = TRUE)
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[1]], "date:"))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[1]][1])))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[1]][2])))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # get dataset to the point time_hour will be filtered, which is after date is filtered
    t <- dataset %>% filter(!is.na(date),
                            date >= global_filter_list[[1]][1],
                            date <= global_filter_list[[1]][2])
    num_na <- sum(is.na(t$time_hour))  # the date filter removed all of the NAs from time_hour
    time_hour_floor <- floor_date(t$time_hour, unit='day')
    num_filtered_out <- sum(time_hour_floor < global_filter_list[[2]][1] | time_hour_floor > global_filter_list[[2]][2], na.rm = TRUE)
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[2]], "time_hour:"))
    expect_true(str_detect(filter_results[[2]][[2]], as.character(global_filter_list[[2]][1])))
    expect_true(str_detect(filter_results[[2]][[2]], as.character(global_filter_list[[2]][2])))
    expect_false(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    #expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))

    writeLines(paste0(filter_results[[2]], collapse = "\n\n"), "output_files/filter_message__flights__date__time_hour.txt")
    ##########################################################################################################
    filter_selections <- c('hms', 'date', 'time_hour')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(hms),
                                            hms >= hm(global_filter_list[[filter_selections[1]]][1]),
                                            hms <= hm(global_filter_list[[filter_selections[1]]][2]),
                                            !is.na(date),
                                            date >= global_filter_list[[filter_selections[2]]][1],
                                            date <= global_filter_list[[filter_selections[2]]][2],
                                            !is.na(time_hour),
                                            time_hour >= global_filter_list[[filter_selections[3]]][1],
                                            time_hour <= global_filter_list[[filter_selections[3]]][2])
    rownames(expected_filtered) <- rownames(filter_results[[1]])
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$hms < hm(global_filter_list[[filter_selections[1]]][1])))
    expect_false(any(filter_results[[1]]$hms > hm(global_filter_list[[filter_selections[1]]][2])))
    expect_false(any(filter_results[[1]]$date < global_filter_list[[filter_selections[2]]][1]))
    expect_false(any(filter_results[[1]]$date > global_filter_list[[filter_selections[2]]][2]))
    expect_false(any(filter_results[[1]]$time_hour < global_filter_list[[filter_selections[3]]][1]))
    expect_false(any(filter_results[[1]]$time_hour > global_filter_list[[filter_selections[3]]][2]))
    
    expect_equal(length(filter_results[[2]]), length(filter_selections))

    num_na <- sum(is.na(dataset$hms))
    num_filtered_out <- sum(dataset$hms < hm(global_filter_list[[filter_selections[1]]][1]) | dataset$hms > hm(global_filter_list[[filter_selections[1]]][2]), na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "hms:"))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[filter_selections[1]]][1])))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[filter_selections[1]]][2])))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))

    # get dataset to the point date will be filtered, which is after hms is filtered
    t <- dataset %>% filter(!is.na(hms),
                            hms >= hm(global_filter_list[[filter_selections[1]]][1]),
                            hms <= hm(global_filter_list[[filter_selections[1]]][2]))
    num_na <- sum(is.na(t$date))
    num_filtered_out <- sum(t$date < global_filter_list[[filter_selections[2]]][1] | t$date > global_filter_list[[filter_selections[2]]][2], na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[2]], "date:"))
    expect_true(str_detect(filter_results[[2]][[2]], as.character(global_filter_list[[filter_selections[2]]][1])))
    expect_true(str_detect(filter_results[[2]][[2]], as.character(global_filter_list[[filter_selections[2]]][2])))
    #expect_false(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    
    # get dataset to the point time_hour will be filtered, which is after hms/date is filtered
    t <- dataset %>% filter(!is.na(hms),
                            hms >= hm(global_filter_list[[filter_selections[1]]][1]),
                            hms <= hm(global_filter_list[[filter_selections[1]]][2]),
                            !is.na(date),
                            date >= global_filter_list[[filter_selections[2]]][1],
                            date <= global_filter_list[[filter_selections[2]]][2])
    num_na <- sum(is.na(t$time_hour))
    time_hour_floor <- floor_date(t$time_hour, unit='day')
    num_filtered_out <- sum(time_hour_floor < global_filter_list[[filter_selections[3]]][1] | time_hour_floor > global_filter_list[[filter_selections[3]]][2], na.rm = TRUE)
    
    num_filtered_out <- sum(t$time_hour < global_filter_list[[filter_selections[3]]][1] | t$time_hour > global_filter_list[[filter_selections[3]]][2], na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[3]], "time_hour:"))
    expect_true(str_detect(filter_results[[2]][[3]], as.character(global_filter_list[[filter_selections[3]]][1])))
    expect_true(str_detect(filter_results[[2]][[3]], as.character(global_filter_list[[filter_selections[3]]][2])))
    expect_false(str_detect(filter_results[[2]][[3]], paste(my_number_format(num_na), "rows with missing values")))
    #expect_true(str_detect(filter_results[[2]][[3]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[3]], paste(my_number_format(num_filtered_out), "rows")))
    writeLines(paste0(filter_results[[2]], collapse = "\n\n"), "output_files/filter_message__flights__hms__date__time_hour.txt")
    
    ##########################################################################################################
    filter_selections <- c('hms')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(hms),
                                            hms >= hm(global_filter_list[[filter_selections[1]]][1]),
                                            hms <= hm(global_filter_list[[filter_selections[1]]][2]))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$hms < hm(global_filter_list[[filter_selections[1]]][1])))
    expect_false(any(filter_results[[1]]$hms > hm(global_filter_list[[filter_selections[1]]][2])))
    
    expect_equal(length(filter_results[[2]]), length(filter_selections))

    num_na <- sum(is.na(dataset$hms))
    num_filtered_out <- sum(dataset$hms < hm(global_filter_list[[filter_selections[1]]][1]) | dataset$hms > hm(global_filter_list[[filter_selections[1]]][2]), na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[1]], "hms:"))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[filter_selections[1]]][1])))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[filter_selections[1]]][2])))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    writeLines(paste0(filter_results[[2]], collapse = "\n\n"), "output_files/filter_message__flights__hms.txt")
    ##########################################################################################################
    filter_selections <- c('date', 'hms')
    filter_results <- filter_data(dataset=dataset,
                                  filter_list=global_filter_list[filter_selections],
                                  callback=NULL)
    expected_filtered <- dataset %>% filter(!is.na(date),
                                            date >= global_filter_list[[filter_selections[1]]][1],
                                            date <= global_filter_list[[filter_selections[1]]][2],
                                            !is.na(hms),
                                            hms >= hm(global_filter_list[[filter_selections[2]]][1]),
                                            hms <= hm(global_filter_list[[filter_selections[2]]][2]))
    expect_true(rt_are_dataframes_equal(expected_filtered, filter_results[[1]]))
    expect_false(any(filter_results[[1]]$date < global_filter_list[[filter_selections[1]]][1]))
    expect_false(any(filter_results[[1]]$date > global_filter_list[[filter_selections[1]]][2]))
    expect_false(any(filter_results[[1]]$hms < hm(global_filter_list[[filter_selections[2]]][1])))
    expect_false(any(filter_results[[1]]$hms > hm(global_filter_list[[filter_selections[2]]][2])))
    
    num_na <- sum(is.na(dataset$date))
    num_filtered_out <- sum(dataset$date < global_filter_list[[1]][1] | dataset$date > global_filter_list[[1]][2], na.rm = TRUE)
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[1]], "date:"))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[1]][1])))
    expect_true(str_detect(filter_results[[2]][[1]], as.character(global_filter_list[[1]][2])))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(my_number_format(num_filtered_out), "rows")))
    
    # get dataset to the point hms will be filtered, which is after date is filtered
    t <- dataset %>% filter(!is.na(date),
                            date >= global_filter_list[[1]][1],
                            date <= global_filter_list[[1]][2])
    num_na <- sum(is.na(t$hms))  # the date filter removed all of the NAs from hms
    num_filtered_out <- sum(t$hms < hm(global_filter_list[[3]][1]) | t$hms > hm(global_filter_list[[3]][2]), na.rm = TRUE)
    expect_equal(length(filter_results[[2]]), length(filter_selections))
    expect_true(str_detect(filter_results[[2]][[2]], "hms:"))
    expect_true(str_detect(filter_results[[2]][[2]], as.character(global_filter_list[[3]][1])))
    expect_true(str_detect(filter_results[[2]][[2]], as.character(global_filter_list[[3]][2])))
    expect_false(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    #expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(my_number_format(num_filtered_out), "rows")))
    writeLines(paste0(filter_results[[2]], collapse = "\n\n"), "output_files/filter_message__flights__date__hms.txt")
})

test_that("generic_helpers::mutate_factor_lump", {
    context("generic_helpers::mutate_factor_lump")
    
    dataset <- dataset_or_null('../example_datasets/credit.csv') %>%
        mutate(checking_balance = as.character(checking_balance),
               employment_duration = as.character(employment_duration))
    
    actual_types <- map_chr(colnames(dataset), ~ class(dataset[, .])[1])
    expect_identical(actual_types, c('character', 'integer', 'factor', 'factor', 'integer', 'factor', 'character',
                                     'integer', 'integer', 'integer', 'factor', 'factor', 'integer', 'factor',
                                     'integer', 'factor', 'factor'))
    
    expected_types <- replace(actual_types, actual_types == 'character', 'factor')
    
    # factor_lump_number NULL
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = NULL)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = NA)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    # characters & factors
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = 1)
    expect_identical(colnames(dataset), colnames(lumped_dataset))
    found_types <- map_chr(colnames(lumped_dataset), ~ class(lumped_dataset[, .])[1])
    expect_identical(expected_types, found_types)
    expect_true(rt_are_dataframes_equal(lumped_dataset %>% select_if(is.numeric),
                                        dataset %>% select_if(is.numeric)))
    all_factors <- colnames(lumped_dataset)[found_types == 'factor']
    # all factors should have 2 levels, the top level and "Other"
    expect_true(all(map_lgl(all_factors, ~ length(levels(lumped_dataset[, .])) == 2)))
    # it looks like if there are 2 factors, rather than keeping changing 1 to Other (and still
    # having 2 factors) it keeps the original factor
    original_number_of_levels <- map_int(all_factors, ~ length(unique(dataset[, .])))
    found_other_level_if_expected <- map2_lgl(all_factors, original_number_of_levels, ~ {
        # if there are more than 2 levels (even though the factor_lump number is 1), we expect Other to be
        # a level
        if(.y > 2) {
            return ("Other" %in% levels(lumped_dataset[, .]))
        } else {
            return (!"Other" %in% levels(lumped_dataset[, .]))
        }
    })
    expect_true(all(found_other_level_if_expected))

    # ignore_columns
    all_categoric <- colnames(dataset)[expected_types == 'factor']
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = 1, ignore_columns = all_categoric)
    expect_identical(colnames(dataset), colnames(lumped_dataset))
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    # subset
    ignore_columns <- c("checking_balance", "credit_history")
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = 1,
                                                     ignore_columns = ignore_columns)
    expect_identical(colnames(dataset), colnames(lumped_dataset))
    new_expected_types <- expected_types
    new_expected_types[1] <- 'character' # we are ignoring this column, and the other column is already a factor
    found_types <- map_chr(colnames(lumped_dataset), ~ class(lumped_dataset[, .])[1])
    expect_identical(new_expected_types, found_types)
    
    expect_identical(lumped_dataset$checking_balance, dataset$checking_balance)
    expect_identical(lumped_dataset$credit_history, dataset$credit_history)
    
    expect_true(rt_are_dataframes_equal(lumped_dataset %>% select_if(is.numeric),
                                        dataset %>% select_if(is.numeric)))
    
    all_factors <- colnames(lumped_dataset)[found_types == 'factor']
    all_factors <- all_factors %>% rt_remove_val(ignore_columns)
    # all factors should have 2 levels, the top level and "Other"
    expect_true(all(map_lgl(all_factors, ~ length(levels(lumped_dataset[, .])) == 2)))
    # it looks like if there are 2 factors, rather than keeping changing 1 to Other (and still
    # having 2 factors) it keeps the original factor
    original_number_of_levels <- map_int(all_factors, ~ length(unique(dataset[, .])))
    found_other_level_if_expected <- map2_lgl(all_factors, original_number_of_levels, ~ {
        # if there are more than 2 levels (even though the factor_lump number is 1), we expect Other to be
        # a level
        if(.y > 2) {
            return ("Other" %in% levels(lumped_dataset[, .]))
        } else {
            return (!"Other" %in% levels(lumped_dataset[, .]))
        }
    })
    expect_true(all(found_other_level_if_expected))
})

test_that("generic_helpers::mutate_factor_lump::single_character", {
    context("generic_helpers::mutate_factor_lump:single_character")
    
    dataset <- dataset_or_null('../example_datasets/credit.csv') %>%
        select(checking_balance) %>%
        mutate(checking_balance = as.character(checking_balance))
    
    # factor_lump_number NULL
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = NULL)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = NA)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    # characters & factors
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = 1)
    expect_true(rt_are_dataframes_equal(lumped_dataset,
                                        dataset %>%
                                            mutate(checking_balance = fct_lump(checking_balance, n=1))))
})

test_that("generic_helpers::mutate_factor_lump::all_numeric", {
    context("generic_helpers::mutate_factor_lump::all_numeric")
    
    dataset <- dataset_or_null('../example_datasets/credit.csv') %>%
        select(months_loan_duration)
        
    # factor_lump_number NULL
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = NULL)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = NA)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    # characters & factors
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = 1)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    dataset <- dataset_or_null('../example_datasets/credit.csv') %>%
        select(months_loan_duration, amount)
    
    # factor_lump_number NULL
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = NULL)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = NA)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
    
    # characters & factors
    lumped_dataset <- dataset %>% mutate_factor_lump(factor_lump_number = 1)
    expect_true(rt_are_dataframes_equal(lumped_dataset, dataset))
})

test_that("generic_helpers::mutate_factor_reorder", {
    context("generic_helpers::mutate_factor_reorder")

    diamonds_order_by_freq <- diamonds %>% count(cut, sort=TRUE)
    diamonds_order_by_median <- diamonds %>%
        group_by(cut) %>%
        summarise(n=n(),
                  median_carat=median(carat)) %>%
        arrange(desc(median_carat))
    
    # DEFAULT
    mutated <- diamonds %>% mutate_factor_reorder('Default', 'cut')
    expect_identical(levels(mutated$cut), levels(diamonds$cut))
    
    # FREQ
    # CHAR
    mutated <- diamonds %>% 
        mutate(cut = as.character(cut)) %>%
        mutate_factor_reorder('Frequency', 'cut')
    expect_identical(levels(mutated$cut), as.character(diamonds_order_by_freq$cut))
    
    # FACTOR
    mutated <- diamonds %>% mutate_factor_reorder('Frequency', 'cut')
    expect_identical(levels(mutated$cut), as.character(diamonds_order_by_freq$cut))
    
    # MEDIAN
    # CHAR
    mutated <- diamonds %>% 
        mutate(cut = as.character(cut)) %>%
        mutate_factor_reorder('carat', 'cut')
    expect_identical(levels(mutated$cut), as.character(diamonds_order_by_median$cut))
    
    # FACTOR
    mutated <- diamonds %>% 
        mutate_factor_reorder('carat', 'cut')
    expect_identical(levels(mutated$cut), as.character(diamonds_order_by_median$cut))


    # LOGICAL
    local_dataset <- dataset_or_null('../example_datasets/credit.csv') %>%
        mutate(default = ifelse(default == 'yes', TRUE, FALSE))

    mutated <- local_dataset %>% 
        mutate_factor_reorder('Frequency', 'default')
    expect_identical(levels(mutated$default), c('FALSE', 'TRUE'))

    mutated <- local_dataset %>% 
        mutate_factor_reorder('Default', 'default')
    expect_true(is.logical(mutated$default))  # default should not change the variable type
    
    mutated <- local_dataset %>% 
        mutate_factor_reorder('amount', 'default')
    expect_identical(levels(mutated$default), c('TRUE', 'FALSE'))
})

test_that("add_x_annotations", {
    context("add_x_annotations")
    local_dataset <- data.frame(nycflights13::flights %>%
        mutate(date = make_date(year, month, day)) %>%
        select(-year, -month, -day) %>%
        select(date, everything()))
    
    vertical_annotations <- list(c("2013-03-15", "Event 3 (2013-03-15)"),
                                 c("2013-01-01", "Event 1"),
                                 c("2013-02-01", "Event 2"))
    
    horizontal_annotations <- list(c(5000, "Event A"),
                                   c(6000, "Event B"),
                                   c(8000, "Event C"))

    local_primary_variable <- 'date'
    local_y_zoom_min <- NULL
    local_y_zoom_max <- NULL
    ggplot_object <- local_dataset %>%
        select(local_primary_variable) %>%
        mutate_factor_lump(factor_lump_number=10) %>%
        rt_explore_plot_time_series(variable=local_primary_variable,
                                    comparison_variable=NULL,
                                    comparison_function=NULL,
                                    comparison_function_name=NULL,
                                    color_variable=NULL,
                                    y_zoom_min=local_y_zoom_min,
                                    y_zoom_max=local_y_zoom_max,
                                    show_points=TRUE,
                                    show_labels=TRUE,
                                    date_floor='week',
                                    date_break_format='%Y-%m-%d',
                                    date_breaks_width='2 weeks',
                                    base_size=11)
    annotated_object <- ggplot_object %>%
        add_vertical_annotations(NULL, y_location=max(0, local_y_zoom_min), is_date=TRUE) %>%
        add_horizontal_annotations(NULL, x_location=max(min(local_dataset[, local_primary_variable]), NULL))
    test_save_plot(file_name='output_files/annotations__v__h__date_NULL.png', annotated_object)

    annotated_object <- ggplot_object %>%
        add_vertical_annotations(vertical_annotations, y_location=max(0, local_y_zoom_min), is_date=TRUE) %>%
        add_horizontal_annotations(horizontal_annotations, x_location=max(min(local_dataset[, local_primary_variable]), NULL))
    test_save_plot(file_name='output_files/annotations__v__h__date.png', annotated_object)
    
    local_x_zoom_min <- ymd('2013-06-10')
    local_y_zoom_min <- 1000
    annotated_object <- ggplot_object %>%
        add_vertical_annotations(vertical_annotations, y_location=max(0, local_y_zoom_min), is_date=TRUE) %>%
        add_horizontal_annotations(horizontal_annotations, x_location=max(min(local_dataset[, local_primary_variable]), local_x_zoom_min))
    test_save_plot(file_name='output_files/annotations__v__h__date__adjusted.png', annotated_object)

    # scatter
    local_dataset <- dataset_or_null('../example_datasets/credit.csv')
    
    vertical_annotations <- list(c(0, "Event 1"),
                                 c(2000, "Event 2"),
                                 c(12000, "Event 3"))
    
    horizontal_annotations <- list(c(10, "Event A"),
                                   c(30, "Event B"),
                                   c(40, "Event C"))
    
    local_primary_variable <- 'months_loan_duration'
    local_comparison_variable <- 'amount'
    local_y_zoom_min <- NULL
    local_x_zoom_min <- NULL
    ggplot_object <- local_dataset %>%
        rt_explore_plot_scatter(variable=local_primary_variable,
                                    comparison_variable=local_comparison_variable,
                                    y_zoom_min=local_y_zoom_min,
                                    y_zoom_max=local_y_zoom_max,
                                    base_size=11)
    annotated_object <- ggplot_object %>%
        add_vertical_annotations(NULL, y_location=max(0, local_y_zoom_min)) %>%
        add_horizontal_annotations(NULL, x_location=max(min(local_dataset[, local_primary_variable]), local_x_zoom_min))
    test_save_plot(file_name='output_files/annotations__v__h__scatter_NULL.png', annotated_object)
    
    annotated_object <- ggplot_object %>%
        add_vertical_annotations(vertical_annotations, y_location=max(0, local_y_zoom_min)) %>%
        add_horizontal_annotations(horizontal_annotations, x_location=max(min(local_dataset[, local_primary_variable]), local_x_zoom_min))
    test_save_plot(file_name='output_files/annotations__v__h__scatter.png', annotated_object)
    
    local_x_zoom_min <- 6000
    local_y_zoom_min <- 60

    annotated_object <- ggplot_object %>%
        add_vertical_annotations(vertical_annotations, y_location=max(0, local_y_zoom_min)) %>%
        add_horizontal_annotations(horizontal_annotations, x_location=max(min(local_dataset[, local_primary_variable]), local_x_zoom_min))
    test_save_plot(file_name='output_files/annotations__v__h__scatter__adjusted.png', annotated_object)
})

test_that("add_x_annotations:POSIXct", {
    context("add_x_annotations:POSIXct")
    local_dataset <- data.frame(nycflights13::flights %>%
                                    mutate(date = make_date(year, month, day)) %>%
                                    select(-year, -month, -day) %>%
                                    select(date, everything())) %>%
        mutate(date = as.POSIXct(date))
    
    vertical_annotations <- list(c("2013-03-15", "Event 3 (2013-03-15)"),
                                 c("2013-01-01", "Event 1"),
                                 c("2013-02-01", "Event 2"))
    
    horizontal_annotations <- list(c(5000, "Event A"),
                                   c(6000, "Event B"),
                                   c(8000, "Event C"))
    
    local_primary_variable <- 'date'
    local_y_zoom_min <- NULL
    local_y_zoom_max <- NULL
    ggplot_object <- local_dataset %>%
        select(local_primary_variable) %>%
        mutate_factor_lump(factor_lump_number=10) %>%
        rt_explore_plot_time_series(variable=local_primary_variable,
                                    comparison_variable=NULL,
                                    comparison_function=NULL,
                                    comparison_function_name=NULL,
                                    color_variable=NULL,
                                    y_zoom_min=local_y_zoom_min,
                                    y_zoom_max=local_y_zoom_max,
                                    show_points=TRUE,
                                    show_labels=TRUE,
                                    date_floor='week',
                                    date_break_format='%Y-%m-%d',
                                    date_breaks_width='2 weeks',
                                    base_size=11)
    
    annotated_object <- ggplot_object %>%
        add_vertical_annotations(vertical_annotations, y_location=max(0, local_y_zoom_min), is_date=TRUE) %>%
        add_horizontal_annotations(horizontal_annotations,
                                   x_location=max(min(local_dataset[, local_primary_variable]), NULL),
                                   x_location_is_date=TRUE)
    test_save_plot(file_name='output_files/annotations__v__h__date__POSIXct.png', annotated_object)
    
    local_x_zoom_min <- ymd('2013-06-10')
    local_y_zoom_min <- 1000
    annotated_object <- ggplot_object %>%
        add_vertical_annotations(vertical_annotations, y_location=max(0, local_y_zoom_min), is_date=TRUE) %>%
        add_horizontal_annotations(horizontal_annotations,
                                   x_location=max(min(local_dataset[, local_primary_variable]), local_x_zoom_min),
                                   x_location_is_date=TRUE)
    test_save_plot(file_name='output_files/annotations__v__h__date__adjusted__POSIXct.png', annotated_object)
})

test_that("get_base_url", {
    mock_session <- list(clientData=list(url_protocol='http:',
                                         url_hostname='127.0.0.1',
                                         url_port='5249',
                                         url_pathname='/'))
    expect_equal(get_base_url(mock_session), 'http://127.0.0.1:5249/')
    
    mock_session <- list(clientData=list(url_protocol='http:',
                                         url_hostname='127.0.0.1',
                                         url_port='5249',
                                         url_pathname='/custom/reports/'))
    expect_equal(get_base_url(mock_session), 'http://127.0.0.1:5249/custom/reports/')
    
    mock_session <- list(clientData=list(url_protocol='https:',
                                         url_hostname='www.myreports.com',
                                         url_port='',
                                         url_pathname='/custom/reports/'))
    expect_equal(get_base_url(mock_session), 'https://www.myreports.com/custom/reports/')

    mock_session <- list(clientData=list(url_protocol='https:',
                                         url_hostname='www.myreports.com',
                                         url_port=NULL,
                                         url_pathname='/custom/reports/'))
    expect_equal(get_base_url(mock_session), 'https://www.myreports.com/custom/reports/')
    
    
    mock_session <- list(clientData=list(url_protocol='https:',
                                         url_hostname='www.myreports.com',
                                         url_port=NULL,
                                         url_pathname='/custom/reports'))
    expect_equal(get_base_url(mock_session), 'https://www.myreports.com/custom/reports')
})

test_that("build_parse_url_params", {
    context("build_parse_url_params")
    input <- var_plots__default_values
    parameters <- build_parameters_list(input=input, preloaded_dataset='flights')
    
    expect_equal(length(parameters), 2)
    expect_true(!is.null(parameters$data))
    expect_equal(parameters$data, 'flights')
    expect_true(!is.null(parameters$tab))
    expect_equal(parameters$tab, 'Graphs')
    
    mock_input <- list(
        'var_plots__variable' = 'Test values with spaces and sh*&%t.!',
        'var_plots__comparison' = 'expected_value_comparison',
        'var_plots__num_cat_aggregation_type' = 'Average Value Per Record',
        'var_plots__sum_by_variable' = 'expected_value_sum_by_variable',
        'var_plots__color_variable' = 'expected_value_color_variable',
        'var_plots__facet_variable' = 'expected_value_facet_variable',
        'var_plots__size_variable' = 'expected_value_size_variable',
        'var_plots__numeric_group_comp_variable' = TRUE,
        'var_plots__numeric_aggregation_function' = 'month',
        'var_plots__numeric_aggregation' = 'week',
        'var_plots__multi_value_delimiter' = "; ",
        'var_plots__filter_factor_lump_number'=20,
        'var_plots__label_variables' = c('this', 'has', 'multiple', 'values'),
        'var_plots__annotate_points' = FALSE,
        'var_plots__show_points' = FALSE,
        'var_plots__year_over_year' = TRUE,
        'var_plots__include_zero_y_axis' = FALSE,
        'var_plots__numeric_graph_type' = "Boxplot Whoot",
        'var_plots__categoric_view_type' = "Bar Whoot",
        'var_plots__order_by_variable' = "Default Whoot",
        'var_plots__show_variable_totals' = FALSE,
        'var_plots__show_comparison_totals' = FALSE,
        'var_plots__histogram_bins' = 1000,
        'var_plots__transparency' = 1999,
        'var_plots__jitter' = TRUE,
        'var_plots__numeric_aggregation_count_minimum' = 3009,
        'var_plots__numeric_show_resampled_conf_int' = TRUE,
        'var_plots__trend_line' = 'None Whoot',
        'var_plots__trend_line_se' = 'Yes Whoot',
        'var_plots__ts_date_floor' = 'None Whoot',
        'var_plots__ts_date_break_format' = 'Auto Whoot',
        'var_plots__ts_breaks_width' = 'not null',
        'var_plots__scale_x_log_base_10' = TRUE,
        'var_plots__x_zoom_min' = 'not NA',
        'var_plots__x_zoom_max' = 40,
        'var_plots__scale_y_log_base_10' = TRUE,
        'var_plots__y_zoom_min' = 'not NA',
        'var_plots__y_zoom_max' = 'not NA',
        'var_plots__custom_title' = "This is my title",
        'var_plots__custom_subtitle' = "Test & out &amp; amp;",
        'var_plots__custom_x_axis_label' = "This is my X Axis Label",
        'var_plots__custom_y_axis_label' = "This is my Y Axis Label",
        'var_plots__custom_caption' = "This is my caption",
        'var_plots__custom_tag' = "This is my Tag",
        'var_plots__pretty_text' = TRUE,
        'var_plots__base_size' = 155,
        'var_plots__vertical_annotations' = "vertical annotations",
        'var_plots__horizontal_annotations' = "horizontal annotations"
    )
    parameters <- build_parameters_list(input=mock_input, preloaded_dataset='flights')
    
    expected_names <- str_replace(c('data', 'tab', names(mock_input)), 'var_plots__', '')
    # need unique(names) because var_plots__label_variables has multiple values which should be expanded
    expect_identical(unique(names(parameters)), expected_names)
    # this list should have been converted to 4 list elements with the same names and different values
    label_variable_list <- parameters[which(names(parameters) == 'label_variables')]
    expect_equal(length(label_variable_list), length(mock_input$var_plots__label_variables))
    expect_equal(label_variable_list[[1]], mock_input$var_plots__label_variables[1])
    expect_equal(label_variable_list[[2]], mock_input$var_plots__label_variables[2])
    expect_equal(label_variable_list[[3]], mock_input$var_plots__label_variables[3])
    expect_equal(label_variable_list[[4]], mock_input$var_plots__label_variables[4])
    
    for(variable in names(parameters) %>% rt_remove_val(c('data', 'tab', 'label_variables'))) {
        expect_equal(parameters[[variable]], mock_input[[paste0('var_plots__',variable)]])
    }
    
    custom_url <- build_custom_url(base_url = 'http://127.0.0.1:3158/', parameters_list = parameters)
    expected_url <- "http://127.0.0.1:3158/?data=flights&tab=Graphs&variable=Test%20values%20with%20spaces%20and%20sh%2A%26%25t.%21&comparison=expected_value_comparison&num_cat_aggregation_type=Average%20Value%20Per%20Record&sum_by_variable=expected_value_sum_by_variable&color_variable=expected_value_color_variable&facet_variable=expected_value_facet_variable&size_variable=expected_value_size_variable&numeric_group_comp_variable=TRUE&numeric_aggregation_function=month&numeric_aggregation=week&multi_value_delimiter=%3B%20&filter_factor_lump_number=20&label_variables=this&label_variables=has&label_variables=multiple&label_variables=values&annotate_points=FALSE&show_points=FALSE&year_over_year=TRUE&include_zero_y_axis=FALSE&numeric_graph_type=Boxplot%20Whoot&categoric_view_type=Bar%20Whoot&order_by_variable=Default%20Whoot&show_variable_totals=FALSE&show_comparison_totals=FALSE&histogram_bins=1000&transparency=1999&jitter=TRUE&numeric_aggregation_count_minimum=3009&numeric_show_resampled_conf_int=TRUE&trend_line=None%20Whoot&trend_line_se=Yes%20Whoot&ts_date_floor=None%20Whoot&ts_date_break_format=Auto%20Whoot&ts_breaks_width=not%20null&scale_x_log_base_10=TRUE&x_zoom_min=not%20NA&x_zoom_max=40&scale_y_log_base_10=TRUE&y_zoom_min=not%20NA&y_zoom_max=not%20NA&custom_title=This%20is%20my%20title&custom_subtitle=Test%20%26%20out%20%26amp%3B%20amp%3B&custom_x_axis_label=This%20is%20my%20X%20Axis%20Label&custom_y_axis_label=This%20is%20my%20Y%20Axis%20Label&custom_caption=This%20is%20my%20caption&custom_tag=This%20is%20my%20Tag&pretty_text=TRUE&base_size=155&vertical_annotations=vertical%20annotations&horizontal_annotations=horizontal%20annotations"
    expect_equal(custom_url, expected_url)
    expect_equal(nchar(custom_url), 1647)  # redundant, but want to track how long the url is (max url ~2000)
    
    # extract_url_parameters only expects the ?.... part of the url
    url_search <- str_replace(custom_url, 'http://127.0.0.1:3158/', '')
    extracted_parameters <- extract_url_parameters(url_search=url_search)
    expect_identical(names(extracted_parameters), c('data', 'tab', names(mock_input)))
    
    var_plots_extracted_parameters <- extracted_parameters[which(!names(extracted_parameters) %in% c('data', 'tab'))]
    expect_identical(names(var_plots_extracted_parameters), names(mock_input))
    
    for(variable in names(mock_input)) {
        
        expect_equal(var_plots_extracted_parameters[[variable]], mock_input[[variable]])
    }
    
    # seems like when using a shiny app that is redirecting after e.g. sso login, '&' get double-encoded
    double_encoded_search <- str_replace_all(string = url_search, pattern = '&', replacement = '&amp;')
    extracted_parameters <- extract_url_parameters(url_search=double_encoded_search)
    expect_identical(names(extracted_parameters), c('data', 'tab', names(mock_input)))
    
    var_plots_extracted_parameters <- extracted_parameters[which(!names(extracted_parameters) %in% c('data', 'tab'))]
    expect_identical(names(var_plots_extracted_parameters), names(mock_input))
    
    for(variable in names(mock_input)) {
        
        expect_equal(var_plots_extracted_parameters[[variable]], mock_input[[variable]])
    }
    # this should have been tested in the for loop above but want to explicitly test and call out
    # first, make sure this hasn't changed, need to explicitly test & &amp; amp; values in case these show up in the parameters
    expect_equal(mock_input[['var_plots__custom_subtitle']], "Test & out &amp; amp;")
    expect_equal(var_plots_extracted_parameters[['var_plots__custom_subtitle']], mock_input[['var_plots__custom_subtitle']])
})

test_that("build_parse_url_params - filtering", {
    context("build_parse_url_params - filtering")
    input <- var_plots__default_values
    
    filter_list <- list(
        carat = c(0.5, 2),
        cut = c('Ideal', 'Premium', 'Good'),
        color = NULL,
        price = c(326, 18823),
        date = c(ymd('2013-02-05'), ymd('2013-10-31')),
        time_hour = c(ymd('2013-03-05'), ymd('2013-11-30')),
        hms = c("09:01", "18:59")
    )
    
    parameters <- build_parameters_list(input=input, preloaded_dataset='flights', filter_list = filter_list)
    
    # removes NULL, which we want (don't send empty filters, even if they are selected, uses up url characters)
    expect_equal(length(parameters), length(flatten(filter_list)) + 2)
    expect_true(!is.null(parameters$data))
    expect_equal(parameters$data, 'flights')
    expect_true(!is.null(parameters$tab))
    expect_equal(parameters$tab, 'Graphs')
    
    expected_names <- c(rep('carat', 2), rep('cut', 3), rep('price', 2), rep('date', 2), rep('time_hour', 2), rep('hms', 2))
    expected_names <- paste0(global__url_params_filter_prefix, expected_names)
    expect_identical(names(parameters) %>% rt_remove_val(c('data', 'tab')), expected_names)

    # TO URL
    custom_url <- build_custom_url(base_url = 'http://127.0.0.1:3158/', parameters_list = parameters)
    expected_url <- "http://127.0.0.1:3158/?data=flights&tab=Graphs&%21%21_carat=0.5&%21%21_carat=2&%21%21_cut=Ideal&%21%21_cut=Premium&%21%21_cut=Good&%21%21_price=326&%21%21_price=18823&%21%21_date=2013-02-05&%21%21_date=2013-10-31&%21%21_time_hour=2013-03-05&%21%21_time_hour=2013-11-30&%21%21_hms=09%3A01&%21%21_hms=18%3A59"
    expect_equal(custom_url, expected_url)
    expect_equal(nchar(custom_url), 306)
    
    # BACK TO PARAMETERS
    url_search <- str_replace(expected_url, 'http://127.0.0.1:3158/', '')
    extracted_parameters <- extract_url_parameters(url_search = url_search)
    
    expected_parameters <- filter_list
    # remove `color` which has NULL value
    expected_parameters <- expected_parameters[which(names(expected_parameters) != 'color')]
    # add expected prefix
    names(expected_parameters) <- paste0(global__url_params_filter_prefix, names(expected_parameters))
    # add other values
    expected_parameters <- c(list('data'='flights', 'tab'='Graphs'), expected_parameters)
    expect_equal(length(expected_parameters), length(extracted_parameters))
    expect_identical(names(expected_parameters), names(extracted_parameters))
    for(param_name in names(expected_parameters)) {
        expect_true(all(expected_parameters[[param_name]] == extracted_parameters[[param_name]]))
    }

    # Now Test with var_plots__
    mock_input <- list(
        'var_plots__variable' = 'Test values with spaces and sh*&%t.!',
        'var_plots__comparison' = 'expected_value_comparison',
        'var_plots__sum_by_variable' = 'expected_value_sum_by_variable',
        'var_plots__color_variable' = 'expected_value_color_variable',
        'var_plots__facet_variable' = 'expected_value_facet_variable',
        'var_plots__size_variable' = 'expected_value_size_variable',
        'var_plots__numeric_group_comp_variable' = TRUE,
        'var_plots__numeric_aggregation_function' = 'month',
        'var_plots__numeric_aggregation' = 'week',
        'var_plots__multi_value_delimiter' = "; ",
        'var_plots__filter_factor_lump_number'=20,
        'var_plots__label_variables' = c('this', 'has', 'multiple', 'values'),
        'var_plots__annotate_points' = FALSE,
        'var_plots__show_points' = FALSE,
        'var_plots__year_over_year' = TRUE,
        'var_plots__include_zero_y_axis' = FALSE,
        'var_plots__numeric_graph_type' = "Boxplot Whoot",
        'var_plots__categoric_view_type' = "Bar Whoot",
        'var_plots__order_by_variable' = "Default Whoot",
        'var_plots__show_variable_totals' = FALSE,
        'var_plots__show_comparison_totals' = FALSE,
        'var_plots__histogram_bins' = 1000,
        'var_plots__transparency' = 1999,
        'var_plots__jitter' = TRUE,
        'var_plots__numeric_aggregation_count_minimum' = 3009,
        'var_plots__numeric_show_resampled_conf_int' = TRUE,
        'var_plots__trend_line' = 'None Whoot',
        'var_plots__trend_line_se' = 'Yes Whoot',
        'var_plots__ts_date_floor' = 'None Whoot',
        'var_plots__ts_date_break_format' = 'Auto Whoot',
        'var_plots__ts_breaks_width' = 'not null',
        'var_plots__scale_x_log_base_10' = TRUE,
        'var_plots__x_zoom_min' = 'not NA',
        'var_plots__x_zoom_max' = 40,
        'var_plots__scale_y_log_base_10' = TRUE,
        'var_plots__y_zoom_min' = 'not NA',
        'var_plots__y_zoom_max' = 'not NA',
        'var_plots__custom_title' = "This is my title",
        'var_plots__custom_subtitle' = "This is my subtitle",
        'var_plots__custom_x_axis_label' = "This is my X Axis Label",
        'var_plots__custom_y_axis_label' = "This is my Y Axis Label",
        'var_plots__custom_caption' = "This is my caption",
        'var_plots__custom_tag' = "This is my Tag",
        'var_plots__pretty_text' = TRUE,
        'var_plots__base_size' = 155,
        'var_plots__vertical_annotations' = "vertical annotations",
        'var_plots__horizontal_annotations' = "horizontal annotations"
    )
    parameters <- build_parameters_list(input=mock_input, preloaded_dataset='flights', filter_list = filter_list)
    
    filter_variable_names <- c('!!_carat', '!!_cut', '!!_price', '!!_date', '!!_time_hour', '!!_hms')
    expected_names <- str_replace(c('data', 'tab', names(mock_input), filter_variable_names), 'var_plots__', '')
    # need unique(names) because var_plots__label_variables has multiple values which should be expanded
    expect_identical(unique(names(parameters)), expected_names)
    # this list should have been converted to 4 list elements with the same names and different values
    label_variable_list <- parameters[which(names(parameters) == 'label_variables')]
    expect_equal(length(label_variable_list), length(mock_input$var_plots__label_variables))
    expect_equal(label_variable_list[[1]], mock_input$var_plots__label_variables[1])
    expect_equal(label_variable_list[[2]], mock_input$var_plots__label_variables[2])
    expect_equal(label_variable_list[[3]], mock_input$var_plots__label_variables[3])
    expect_equal(label_variable_list[[4]], mock_input$var_plots__label_variables[4])
    
    for(variable in names(parameters) %>% rt_remove_val(c('data', 'tab', 'label_variables', filter_variable_names))) {
        expect_equal(parameters[[variable]], mock_input[[paste0('var_plots__',variable)]])
    }

    custom_url <- build_custom_url(base_url = 'http://127.0.0.1:3158/', parameters_list = parameters)
    expected_url <- "http://127.0.0.1:3158/?data=flights&tab=Graphs&variable=Test%20values%20with%20spaces%20and%20sh%2A%26%25t.%21&comparison=expected_value_comparison&sum_by_variable=expected_value_sum_by_variable&color_variable=expected_value_color_variable&facet_variable=expected_value_facet_variable&size_variable=expected_value_size_variable&numeric_group_comp_variable=TRUE&numeric_aggregation_function=month&numeric_aggregation=week&multi_value_delimiter=%3B%20&filter_factor_lump_number=20&label_variables=this&label_variables=has&label_variables=multiple&label_variables=values&annotate_points=FALSE&show_points=FALSE&year_over_year=TRUE&include_zero_y_axis=FALSE&numeric_graph_type=Boxplot%20Whoot&categoric_view_type=Bar%20Whoot&order_by_variable=Default%20Whoot&show_variable_totals=FALSE&show_comparison_totals=FALSE&histogram_bins=1000&transparency=1999&jitter=TRUE&numeric_aggregation_count_minimum=3009&numeric_show_resampled_conf_int=TRUE&trend_line=None%20Whoot&trend_line_se=Yes%20Whoot&ts_date_floor=None%20Whoot&ts_date_break_format=Auto%20Whoot&ts_breaks_width=not%20null&scale_x_log_base_10=TRUE&x_zoom_min=not%20NA&x_zoom_max=40&scale_y_log_base_10=TRUE&y_zoom_min=not%20NA&y_zoom_max=not%20NA&custom_title=This%20is%20my%20title&custom_subtitle=This%20is%20my%20subtitle&custom_x_axis_label=This%20is%20my%20X%20Axis%20Label&custom_y_axis_label=This%20is%20my%20Y%20Axis%20Label&custom_caption=This%20is%20my%20caption&custom_tag=This%20is%20my%20Tag&pretty_text=TRUE&base_size=155&vertical_annotations=vertical%20annotations&horizontal_annotations=horizontal%20annotations&%21%21_carat=0.5&%21%21_carat=2&%21%21_cut=Ideal&%21%21_cut=Premium&%21%21_cut=Good&%21%21_price=326&%21%21_price=18823&%21%21_date=2013-02-05&%21%21_date=2013-10-31&%21%21_time_hour=2013-03-05&%21%21_time_hour=2013-11-30&%21%21_hms=09%3A01&%21%21_hms=18%3A59"
    expect_equal(custom_url, expected_url)
    expect_equal(nchar(custom_url), 1839)
    
    
    # extract_url_parameters only expects the ?.... part of the url
    extracted_parameters <- extract_url_parameters(url_search=str_replace(custom_url, 'http://127.0.0.1:3158/', ''))
    expect_identical(names(extracted_parameters), c('data', 'tab', names(mock_input), filter_variable_names))
    
    var_plots_extracted_parameters <- extracted_parameters[which(!names(extracted_parameters) %in% c('data', 'tab') & 
                                                                     !str_starts(names(extracted_parameters),
                                                                                 global__url_params_filter_prefix))]
    expect_identical(names(var_plots_extracted_parameters), names(mock_input))
    
    # test var_plots__
    for(variable in names(mock_input)) {
        expect_true(all(var_plots_extracted_parameters[[variable]] == mock_input[[variable]]))
    }
    
    # test_filters & data/tab
    expected_parameters <- filter_list
    # remove color since it was null
    expected_parameters <- expected_parameters[which(names(expected_parameters) != 'color')]
    # append expected prefix
    names(expected_parameters) <- paste0(global__url_params_filter_prefix, names(expected_parameters))
    # add default parameters
    expected_parameters <- c(list('data'='flights', 'tab'='Graphs'), expected_parameters)
    
    other_extracted_parameters <- extracted_parameters[which(names(extracted_parameters) %in% c('data', 'tab') | 
                                                                     str_starts(names(extracted_parameters),
                                                                                global__url_params_filter_prefix))]
    
    expect_equal(length(expected_parameters), length(other_extracted_parameters))
    expect_identical(names(expected_parameters), names(other_extracted_parameters))
    for(param_name in names(expected_parameters)) {
        expect_true(all(expected_parameters[[param_name]] == other_extracted_parameters[[param_name]]))
    }
})

test_that("apply_r_code_to_dataset", {
    
    results <- select_preloaded_dataset("Credit", defualt_path = '../')
    original_dataset <- results$dataset
    
    expected_dataset <- original_dataset %>% mutate(months_loan_duration = months_loan_duration / 10)

    # success
    code_string <- 'dataset <- dataset %>% mutate(months_loan_duration = months_loan_duration / 10)'    
    results <- original_dataset %>% apply_r_code_to_dataset(r_code_string = code_string)
    expect_null(results$error_message)
    actual_dataset <- results$dataset
    expect_true(rt_are_dataframes_equal(expected_dataset, actual_dataset))
    
    # error
    code_string <- 'dataset <- dataset %>% mutateaaaa(months_loan_duration = months_loan_duration / 10)'
    results <- original_dataset %>% apply_r_code_to_dataset(r_code_string = code_string)
    expect_equal(results$error_message,
                 "Error in mutateaaaa(., months_loan_duration = months_loan_duration/10): could not find function \"mutateaaaa\"")
    actual_dataset <- results$dataset
    expect_true(rt_are_dataframes_equal(original_dataset, actual_dataset))
})

test_that("setting dynamic variables - comparison", {
    context("setting dynamic variables - comparison")

    global__should_log_message <<- FALSE
    results <- select_preloaded_dataset("Credit", defualt_path = '../')
    expect_equal(nrow(results$dataset), 1000)
    expect_equal(ncol(results$dataset), 17)
    expect_true(nchar(results$description) > 0)
    
    dataset <- results$dataset
    column_names <- colnames(dataset)
    
    ########
    # NULL Primary Variable
    ########
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=NULL,
                                                         current_value=NULL,
                                                         primary_date_converted_to_categoric=FALSE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=NULL,
                                                         current_value=global__select_variable_optional,
                                                         primary_date_converted_to_categoric=FALSE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=NULL,
                                                         current_value="Doesn't Matter Column Name",
                                                         primary_date_converted_to_categoric=FALSE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)

    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=NULL,
                                                         current_value=global__select_variable_optional,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=NULL,
                                                         current_value="Doesn't Matter Column Name",
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    ########
    # Default Primary Variable
    ########
    primary_variable_default <- var_plots__default_values[['var_plots__variable']]
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_variable_default,
                                                         current_value=NULL)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_variable_default,
                                                         current_value=global__select_variable_optional)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_variable_default,
                                                         current_value="Doesn't Matter Column Name")
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)


    primary_variable_default <- var_plots__default_values[['var_plots__variable']]
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_variable_default,
                                                         current_value=NULL,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_variable_default,
                                                         current_value=global__select_variable_optional,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_variable_default,
                                                         current_value="Doesn't Matter Column Name",
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    ########
    # Numeric Primary Variable
    # All columns names should be available as possible choices for the comparison
    ########
    primary_selection <- 'amount'
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=NULL)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=global__select_variable_optional)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value='phone')
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, 'phone')
    
    
    primary_selection <- 'amount'
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=NULL,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=global__select_variable_optional,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value='phone',
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, 'phone')
    
    ########
    # Categoric Primary Variable
    # All columns names should be available as possible choices for the comparison
    ########
    primary_selection <- 'purpose'
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=NULL)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=global__select_variable_optional)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value='dependents')
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, 'dependents')


    primary_selection <- 'purpose'
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=NULL,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=global__select_variable_optional,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value='dependents',
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, 'dependents')
    
    ########
    # Date Primary Variable
    # only numeric names should be available as possible choices for the comparison
    ########
    results <- select_preloaded_dataset("Flights", defualt_path = '../')
    dataset <- results$dataset
    column_names <- colnames(dataset)
    numeric_column_names <- colnames(dataset %>% select_if(is.numeric))
    
    primary_selection <- 'takeoff_datetime'
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=NULL)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, numeric_column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=global__select_variable_optional)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, numeric_column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value='dep_delay')
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, numeric_column_names))
    expect_equal(comparison_selection$selected, 'dep_delay')
    
    # if the date is converted to a categoric, we need to show all variables categoric/numeric
    primary_selection <- 'takeoff_datetime'
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=NULL,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=global__select_variable_optional,
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value='dep_delay',
                                                         primary_date_converted_to_categoric=TRUE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, 'dep_delay')
    
    # if the current value is categoric, but we switched primary_date_converted_to_categoric off, then
    # the current value should be switched back to default
    primary_selection <- 'takeoff_datetime'
    current_comparison <- 'origin'
    expect_true(is_categoric(dataset[[current_comparison]]))
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=current_comparison,
                                                         primary_date_converted_to_categoric=FALSE)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, numeric_column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
})

test_that("setting dynamic variables - color", {
    context("setting dynamic variables - color")
    
    global__should_log_message <<- FALSE
    results <- select_preloaded_dataset("Credit", defualt_path = '../')
    expect_equal(nrow(results$dataset), 1000)
    expect_equal(ncol(results$dataset), 17)
    expect_true(nchar(results$description) > 0)
    
    dataset <- results$dataset
    column_names <- colnames(dataset)
    categoric_column_names <- colnames(dataset %>% select_if(is_categoric))
    numeric_column_names <- colnames(dataset %>% select_if(is.numeric))

    ########
    # NULL Primary Variable & Comparison Variable
    ########
    primary_variable_default <- NULL
    comparison_variable_default <- NULL
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable_default,
                                         comparison_variable=comparison_variable_default,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable_default,
                                         comparison_variable=comparison_variable_default,
                                         current_value=global__select_variable_optional)
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable_default,
                                         comparison_variable=comparison_variable_default,
                                         current_value="Doesn't Matter Column Name")
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, global__select_variable_optional)

    ########
    # Default Primary Variable & Comparison Variable
    ########
    primary_variable_default <- var_plots__default_values[['var_plots__variable']]
    comparison_variable_default <- var_plots__default_values[['var_plots__comparison']]
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable_default,
                                         comparison_variable=comparison_variable_default,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable_default,
                                         comparison_variable=comparison_variable_default,
                                         current_value=global__select_variable_optional)
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable_default,
                                         comparison_variable=comparison_variable_default,
                                         current_value="Doesn't Matter Column Name")
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, global__select_variable_optional)
 

    ########
    # Categoric Primary Variable, Default Comaprison variable
    ########
    primary_variable <- 'credit_history'
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable_default,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, global__select_variable_optional)

    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable_default,
                                         current_value='purpose')
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, 'purpose')
    
    ########
    # Numeric Primary Variable, Default Comaprison variable
    ########
    primary_variable <- 'amount'
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable_default,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, global__select_variable_optional)

    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable_default,
                                         current_value='purpose')
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, 'purpose')

    ########
    # Categoric Primary Variable, Numeric Comaprison variable
    ########
    primary_variable <- 'credit_history'
    comparison_variable <- 'amount'
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, global__select_variable_optional)

    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable,
                                         current_value='purpose')
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, 'purpose')
    
    ########
    # Numeric Primary Variable, Categoric Comaprison variable
    ########
    primary_variable <- 'amount'
    comparison_variable <- 'credit_history'
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable,
                                         current_value='purpose')
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, 'purpose')
    
    ########
    # Numeric Primary Variable, Categoric Comaprison variable
    ########
    primary_variable <- 'amount'
    comparison_variable <- 'amount'
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_variable,
                                         comparison_variable=comparison_variable,
                                         current_value='purpose')
    expect_identical(selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(selection$selected, 'purpose')
    
    
    ########
    # Date Primary Variable
    # only numeric columns should be available as possible choices for the comparison
    # only categoric columns should be available as possible choices for the color
    ########
    results <- select_preloaded_dataset("Flights", defualt_path = '../')
    dataset <- results$dataset
    
    column_names <- colnames(dataset)
    categoric_column_names <- colnames(dataset %>% select_if(is_categoric))
    numeric_column_names <- colnames(dataset %>% select_if(is.numeric))
    
    primary_selection <- 'takeoff_datetime'
    comparison_variable <- NULL
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_selection,
                                         comparison_variable=comparison_variable,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_selection,
                                         comparison_variable=comparison_variable,
                                         current_value=global__select_variable_optional)
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, global__select_variable_optional)

    primary_selection <- 'takeoff_datetime'
    comparison_variable <- 'dep_delay'
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_selection,
                                         comparison_variable=comparison_variable,
                                         current_value=NULL)
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_selection,
                                         comparison_variable=comparison_variable,
                                         current_value=global__select_variable_optional)
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, global__select_variable_optional)
    
    selection <- var_plots__color__logic(dataset=dataset,
                                         primary_variable=primary_selection,
                                         comparison_variable=comparison_variable,
                                         current_value='origin')
    expect_identical(selection$choices, c(global__select_variable_optional, categoric_column_names))
    expect_equal(selection$selected, 'origin')
})

test_that("setting dynamic variables - trend_extend_date", {
    context("setting dynamic variables - trend_extend_date")

    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Flights", defualt_path = '../')$dataset
    trend_extend_date_default <- var_plots__default_values[['var_plots__trend_extend_date']]
    
    ########
    # NULL Primary Variable
    ########
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                          primary_variable=NULL,
                                                          current_value=NULL)
    expect_equal(date_selection, trend_extend_date_default)
    
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                         primary_variable=NULL,
                                                         current_value="Doesn't Matter Column Name")
    expect_equal(date_selection, trend_extend_date_default)
    
    ########
    # Default Primary Variable
    ########
    primary_variable_default <- var_plots__default_values[['var_plots__variable']]
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                         primary_variable=primary_variable_default,
                                                         current_value=NULL)
    expect_equal(date_selection, trend_extend_date_default)
    
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                         primary_variable=primary_variable_default,
                                                         current_value="Doesn't Matter Column Name")
    expect_equal(date_selection, trend_extend_date_default)
    
    ########
    # Date Primary Variable
    ########
    primary_selection <- 'takeoff_datetime'
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                          primary_variable=primary_selection,
                                                          current_value=NULL)
    # should be max date plus 6 months (floor of)
    expect_equal(date_selection, '2014-06-01')
    
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                          primary_variable=primary_selection,
                                                          current_value='0000-01-01')
    expect_equal(date_selection, '2014-06-01')
    
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                          primary_variable=primary_selection,
                                                          current_value=as.Date('0000-01-01'))
    expect_equal(date_selection, '2014-06-01')
    
    
    # if the currently selected value is less than the max value of the dataset, still default to max+6 months
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                          primary_variable=primary_selection,
                                                          current_value='2013-12-01')
    expect_equal(date_selection, '2014-06-01')
    
    # but if the current value is beyond the max value of the dataset, keep it
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                          primary_variable=primary_selection,
                                                          current_value='2014-12-23')
    # should be max date plus 6 months (floor of)
    expect_equal(date_selection, '2014-12-23')
    
    ########
    # should return default for NUMERIC OR CATEGORIC Primary Variable
    # All columns names should be available as possible choices for the date
    ########
    primary_selection <- 'dep_delay'
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=NULL)
    expect_equal(date_selection, trend_extend_date_default)
    
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value="Doesn't Matter")
    expect_equal(date_selection, trend_extend_date_default)
    
    primary_selection <- 'origin'
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value=NULL)
    expect_equal(date_selection, trend_extend_date_default)
    
    date_selection <- var_plots__trend_extend_date__logic(dataset=dataset,
                                                         primary_variable=primary_selection,
                                                         current_value="Doesn't Matter")
    expect_equal(date_selection, trend_extend_date_default)

})

test_that("get_default_value_for_updating", {
    context("get_default_value_for_updating")

    global__should_log_message <<- FALSE
    expect_error(get_default_value_for_updating('asdf'))
    expect_equal(get_default_value_for_updating('var_plots__variable'), global__select_variable)

    # NULL value should return empty character    
    expect_empty_char <- get_default_value_for_updating('var_plots__label_variables')
    expect_true(identical(expect_empty_char, character(0)))

    # NA value should return empty integer
    expect_empty_int <- get_default_value_for_updating('var_plots__x_zoom_min')
    expect_true(identical(expect_empty_int, integer(0)))
    
})

test_that("create_ggplot_plot - numeric", {
    context("create_ggplot_plot - numeric")
    
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Credit", defualt_path = '../')$dataset
    
    # single numeric
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'months_loan_duration')
    test_save_plot(file_name='graphs/plot__box_plot_defaults.png', plot=plot_object)

    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'months_loan_duration',
                                        y_zoom_min = 10,
                                        y_zoom_max = 60,
                                        custom_title = "Title",
                                        custom_subtitle = "Subtitle",
                                        custom_x_axis_label = "X",
                                        custom_y_axis_label = "Y",
                                        custom_caption = "Caption",
                                        custom_tag = "Tag",
                                        base_size = 17,
                                        pretty_text = TRUE,
                                        horizontal_annotations = "30;test")
    test_save_plot(file_name='graphs/plot__box_plot_options.png', plot=plot_object)
        
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'months_loan_duration',
                                        numeric_graph_type = 'Histogram')
    test_save_plot(file_name='graphs/plot__histogram_default.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'months_loan_duration',
                                        numeric_graph_type = 'Histogram',
                                        x_zoom_max = 60,
                                        custom_title = "Title",
                                        custom_subtitle = "Subtitle",
                                        custom_x_axis_label = "X",
                                        custom_y_axis_label = "Y",
                                        custom_caption = "Caption",
                                        custom_tag = "Tag",
                                        base_size = 17,
                                        pretty_text = TRUE)
    test_save_plot(file_name='graphs/plot__histogram_options.png', plot=plot_object)
    
    # scatter
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'months_loan_duration',
                                        comparison_variable = 'amount',
                                        scatter_add_histograms = FALSE)
    test_save_plot(file_name='graphs/plot__scatter_defaults.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'months_loan_duration',
                                        comparison_variable = 'amount',
                                        label_variables = c('age', 'purpose'),
                                        transparency = 0.90,
                                        jitter = TRUE,
                                        scatter_add_histograms = FALSE,
                                        trend_line = "Straight",
                                        trend_line_se = "Yes",
                                        x_zoom_min = 50,
                                        x_zoom_max = 12000,
                                        y_zoom_min = 10,
                                        y_zoom_max = 50,
                                        custom_title = "Title",
                                        custom_subtitle = "Subtitle",
                                        custom_x_axis_label = "X",
                                        custom_y_axis_label = "Y",
                                        custom_caption = "Caption",
                                        custom_tag = "Tag",
                                        base_size = 17,
                                        pretty_text = TRUE,
                                        horizontal_annotations = "30;test",
                                        vertical_annotations = "6000;test2")
    test_save_plot(file_name='graphs/plot__scatter_options.png', plot=plot_object)
})

test_that("create_ggplot_plot - scatter - histograms", {
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Credit", defualt_path = '../')$dataset
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'months_loan_duration',
                                        comparison_variable = 'amount',
                                        label_variables = c('age', 'purpose'),
                                        transparency = 0.90,
                                        jitter = TRUE,
                                        scatter_add_histograms = TRUE,
                                        trend_line = "Straight",
                                        trend_line_se = "Yes",
                                        x_zoom_min = 50,
                                        x_zoom_max = 12000,
                                        y_zoom_min = 10,
                                        y_zoom_max = 50,
                                        custom_title = "Title",
                                        custom_subtitle = "Subtitle",
                                        custom_x_axis_label = "X",
                                        custom_y_axis_label = "Y",
                                        custom_caption = "Caption",
                                        custom_tag = "Tag",
                                        base_size = 17,
                                        pretty_text = TRUE,
                                        horizontal_annotations = "30;test",
                                        vertical_annotations = "6000;test2")
    test_save_plot(file_name='graphs/plot__scatter__histograms.png', plot=plot_object)
})

test_that("create_ggplot_plot - numeric categoric", {
    context("create_ggplot_plot - numeric categoric")
  
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Credit", defualt_path = '../')$dataset
    
    numeric_variable <- 'amount'
    categoric_variable <- 'checking_balance'
    
    # single numeric
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = categoric_variable,
                                        comparison_variable = numeric_variable,
                                        num_cat_aggregation_type = 'Total')
    test_save_plot(file_name='graphs/plot__categoric_numeric__total.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = categoric_variable,
                                        comparison_variable = numeric_variable,
                                        num_cat_aggregation_type = 'Median')
    test_save_plot(file_name='graphs/plot__categoric_numeric__median.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = numeric_variable,
                                        comparison_variable = categoric_variable,
                                        num_cat_aggregation_type = 'Total')
    test_save_plot(file_name='graphs/plot__numeric_categoric__total.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = numeric_variable,
                                        comparison_variable = categoric_variable,
                                        num_cat_aggregation_type = 'Median')
    test_save_plot(file_name='graphs/plot__numeric_categoric__median.png', plot=plot_object)
})

test_that("create_ggplot_plot - date projection", {
    context("create_ggplot_plot - date projection")
    
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Flights", defualt_path = '../')$dataset
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime')
    test_save_plot(file_name='graphs/plot__time_series__default.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Straight')
    test_save_plot(file_name='graphs/plot__time_series__straight.png', plot=plot_object)

    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Straight', trend_line_se = 'Yes')
    test_save_plot(file_name='graphs/plot__time_series__straight__ci.png', plot=plot_object)
        
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Smooth')
    test_save_plot(file_name='graphs/plot__time_series__smooth.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Smooth', trend_line_se = 'Yes')
    test_save_plot(file_name='graphs/plot__time_series__smooth__ci.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Projection',
                                        trend_extend_date = as.Date('2015-01-01'))
    test_save_plot(file_name='graphs/plot__time_series__projection.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Projection',
                                        trend_extend_date = as.Date('2015-01-01'),
                                        trend_line_se = 'Yes')
    test_save_plot(file_name='graphs/plot__time_series__projection__ci.png', plot=plot_object)
    
    # test when min date is e.g. at the end of a given period
    dataset <- dataset %>% filter(takeoff_datetime >= ymd('2013-02-16'))
    #min(dataset$takeoff_datetime)
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Projection',
                                        trend_extend_date = as.Date('2015-01-01'),
                                        trend_line_se = 'Yes')
    test_save_plot(file_name='graphs/plot__time_series__projection__ci__half_period.png', plot=plot_object)
    
    
    # test week, 2/15 was a friday
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Projection',
                                        ts_date_floor='week',
                                        ts_date_breaks_width='4 weeks',
                                        trend_extend_date = as.Date('2015-01-01'),
                                        trend_line_se = 'Yes')
    test_save_plot(file_name='graphs/plot__time_series__projection__ci__half_period_week.png', plot=plot_object)
    
    # test week, 2/15 was a friday
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Projection',
                                        ts_date_floor='quarter',
                                        #ts_date_breaks_width='4 weeks',
                                        trend_extend_date = as.Date('2015-01-01'),
                                        trend_line_se = 'Yes')
    test_save_plot(file_name='graphs/plot__time_series__projection__ci__half_period_quarter.png', plot=plot_object)
    
    # test week, 2/15 was a friday
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Projection',
                                        ts_date_floor='day',
                                        ts_date_breaks_width='4 weeks',
                                        trend_extend_date = as.Date('2015-01-01'),
                                        trend_line_se = 'Yes')
    test_save_plot(file_name='graphs/plot__time_series__projection__ci__half_period_day.png', plot=plot_object)
})

test_that("create_ggplot_plot - date projection - POSIXct", { 
    context("create_ggplot_plot - date projection")
    
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Flights", defualt_path = '../')$dataset
    
    dataset$date <- as.POSIXct(dataset$takeoff_datetime)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'takeoff_datetime',
                                        trend_line = 'Projection',
                                        trend_extend_date = as.Date('2015-01-01'),
                                        trend_line_se = 'Yes')
    test_save_plot(file_name='graphs/plot__time_series__projection__ci__POSIXct.png', plot=plot_object)
})

test_that("create_ggplot_plot - convert date to categoric", {
    context("create_ggplot_plot - convert date to categoric")
    
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Flights", defualt_path = '../')$dataset
    primary_variable <- 'takeoff_datetime'
    dataset[[primary_variable]] <- rt_floor_date_factor(dataset[[primary_variable]], date_floor='month')
    
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        convert_primary_date_to_categoric=TRUE)
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dep_delay',
                                        convert_primary_date_to_categoric=TRUE)
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_num.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dep_delay',
                                        num_cat_aggregation_type = 'Average Value Per Record',
                                        convert_primary_date_to_categoric=TRUE)
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_num_avg.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'origin',
                                        categoric_view_type = "Stack",
                                        convert_primary_date_to_categoric=TRUE)
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_cat_stack.png', plot=plot_object)
})

test_that("create_ggplot_plot - convert date to categoric - order by", {
    context("create_ggplot_plot - convert date to categoric - order by")
    
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Flights", defualt_path = '../')$dataset
    primary_variable <- 'takeoff_datetime'
    dataset[[primary_variable]] <- rt_floor_date_factor(dataset[[primary_variable]], date_floor='month')

    # date : order by Freq
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        convert_primary_date_to_categoric=TRUE,
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim__order_by_freq.png', plot=plot_object)
    
    # date : order by numeric
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        convert_primary_date_to_categoric=TRUE,
                                        order_by_variable = 'dep_delay')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim__order_by_dep_delay.png', plot=plot_object)
    
    # date/numeric : order by freq
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dep_delay',
                                        convert_primary_date_to_categoric=TRUE,
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_num__order_by_freq.png', plot=plot_object)
    
    # date/numeric : order by numeric
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dep_delay',
                                        convert_primary_date_to_categoric=TRUE,
                                        order_by_variable = 'dep_delay')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_num__order_by_dep_delay.png', plot=plot_object)
    
    # date/numeric : order by freq
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dep_delay',
                                        convert_primary_date_to_categoric=TRUE,
                                        color_variable='dest',
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_num_col__order_by_freq.png', plot=plot_object)
    
    # date/numeric : order by numeric
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dep_delay',
                                        convert_primary_date_to_categoric=TRUE,
                                        color_variable='dest',
                                        order_by_variable = 'dep_delay')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_num_col__order_by_dep_delay.png', plot=plot_object)
    
    # date/categoric : order by freq
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dest',
                                        filter_factor_lump_number = 3,
                                        convert_primary_date_to_categoric=TRUE,
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_categoric__order_by_freq.png', plot=plot_object)
    
    # date/categoric : order by numeric
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dest',
                                        filter_factor_lump_number = 3,
                                        convert_primary_date_to_categoric=TRUE,
                                        order_by_variable = 'dep_delay')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_categoric__order_by_dep_delay.png', plot=plot_object)
    
    # date/categoric/categoric : order by freq
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dest',
                                        filter_factor_lump_number = 3,
                                        convert_primary_date_to_categoric=TRUE,
                                        facet_variable = 'origin',
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_categoric_facet__order_by_freq.png', plot=plot_object)
    
    # date/categoric/categoric : order by numeric
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = primary_variable,
                                        comparison_variable = 'dest',
                                        filter_factor_lump_number = 3,
                                        convert_primary_date_to_categoric=TRUE,
                                        facet_variable = 'origin',
                                        order_by_variable = 'dep_delay')
    test_save_plot(file_name='graphs/plot__date_as_categoric__prim_categoric_facet__order_by_dep_delay.png', plot=plot_object)
    
    
    ##########################################################################################################
    # Equivalent "Actual" Categoric Graphs
    ##########################################################################################################
})

test_that("create_ggplot_plot - bar", { 
    context("create_ggplot_plot - bar")
    
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Credit", defualt_path = '../')$dataset
    set.seed(42)
    dataset$ids_not_unique <- as.character(sample(1:100, 1000, replace=T))
    dataset$ids_unique <- as.character(1:1000)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance')
    test_save_plot(file_name='graphs/create_ggplot_object__single_categoric.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        count_distinct_variable = 'ids_unique')
    test_save_plot(file_name='graphs/create_ggplot_object__single_categoric__unique_id.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        count_distinct_variable = 'ids_not_unique')
    test_save_plot(file_name='graphs/create_ggplot_object__single_categoric__distinct_ids.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'default',
                                        count_distinct_variable = 'ids_unique')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__unique_id.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'default',
                                        count_distinct_variable = 'ids_not_unique')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__distinct_ids.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'default',
                                        sum_by_variable = 'amount')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__sum_by.png', plot=plot_object)

    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'default',
                                        sum_by_variable = 'amount',
                                        categoric_view_type = 'Stack')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__sum_by__stack.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'default',
                                        sum_by_variable = 'amount',
                                        categoric_view_type = 'Stack Percent')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__sum_by__stack_perc.png', plot=plot_object)
})

test_that("create_ggplot_plot - bar - facet", { 
    context("create_ggplot_plot - bar - facet")
    
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Credit", defualt_path = '../')$dataset
    set.seed(42)
    dataset$ids_not_unique <- as.character(sample(1:100, 1000, replace=T))
    dataset$ids_unique <- as.character(1:1000)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__single_categoric__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        count_distinct_variable = 'ids_unique',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__single_categoric__unique_id__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        count_distinct_variable = 'ids_not_unique',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__single_categoric__distinct_ids__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'credit_history',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'credit_history',
                                        count_distinct_variable = 'ids_unique',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__unique_id__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'credit_history',
                                        count_distinct_variable = 'ids_not_unique',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__distinct_ids__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        #comparison_variable = 'credit_history',
                                        sum_by_variable = 'amount',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__sum_by__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'credit_history',
                                        sum_by_variable = 'amount',
                                        categoric_view_type = 'Stack',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__sum_by__stack__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'credit_history',
                                        sum_by_variable = 'amount',
                                        categoric_view_type = 'Stack Percent',
                                        facet_variable = 'default')
    test_save_plot(file_name='graphs/create_ggplot_object__double_categoric__sum_by__stack_perc__facet.png', plot=plot_object)
})

test_that("create_ggplot_plot - bar - order by", { 
    context("create_ggplot_plot - bar - order by")
    
    global__should_log_message <<- FALSE
    dataset <- select_preloaded_dataset("Credit", defualt_path = '../')$dataset

    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__freq.png', plot=plot_object)

    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        order_by_variable = 'Default')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__no_freq.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        facet_variable = 'default',
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__freq__facet.png', plot=plot_object)

    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        facet_variable = 'default',
                                        order_by_variable = 'Default')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__no_freq__facet.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'credit_history',
                                        facet_variable = 'default',
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__freq__facet__comp.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'credit_history',
                                        facet_variable = 'default',
                                        order_by_variable = 'Default')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__no_freq__facet__comp.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'months_loan_duration',
                                        facet_variable = 'default',
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__freq__facet__comp_numeric.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        primary_variable = 'checking_balance',
                                        comparison_variable = 'months_loan_duration',
                                        facet_variable = 'default',
                                        order_by_variable = 'Default')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__no_freq__facet__comp_numeric.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        comparison_variable = 'checking_balance',
                                        primary_variable = 'months_loan_duration',
                                        facet_variable = 'default',
                                        order_by_variable = 'Frequency')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__freq__facet__comp_numeric_swap.png', plot=plot_object)
    
    plot_object <- create_ggplot_object(dataset = dataset,
                                        comparison_variable = 'checking_balance',
                                        primary_variable = 'months_loan_duration',
                                        facet_variable = 'default',
                                        order_by_variable = 'Default')
    test_save_plot(file_name='graphs/create_ggplot_object__bar__order_by__no_freq__facet__comp_numeric_swap.png', plot=plot_object)
})
