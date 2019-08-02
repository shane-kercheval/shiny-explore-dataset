library('testthat')
library(tidyverse)
library(rtools)
library(hms)

source('../helper_scripts/generic_helpers.R', chdir = TRUE)
source('../helper_scripts/definitions.R', chdir = TRUE)
source('../helper_scripts/logging_functions.R', chdir = TRUE)
source('../helper_scripts/variable_plots_helpers.R', chdir = TRUE)
source('../helper_scripts/dataset_loading_helpers.R', chdir = TRUE)
Sys.setenv(TZ='UTC')

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

test_that("generic_helpers::filter_data - flights/date", {
    context("generic_helpers::filter_data - flights/date")
    
    dataset <- data.frame(nycflights13::flights %>%
                              mutate(date = make_date(year, month, day)) %>%
                              select(-year, -month, -day) %>%
                              select(date, everything()))
    
    # test Date, POSIXct, hms
    expect_true(is.Date(dataset$date))
    expect_true(is.POSIXct(dataset$time_hour))
    dataset$hms <- as.hms(dataset$time_hour)
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

test_that("build_parse_url_params", {
    context("build_parse_url_params")
    input <- var_plots__input_list_default_values
    parameters <- build_parameters_list(input=input, preloaded_dataset='flights')
    
    expect_equal(length(parameters), 2)
    expect_true(!is.null(parameters$data))
    expect_equal(parameters$data, 'flights')
    expect_true(!is.null(parameters$tab))
    expect_equal(parameters$tab, 'Graphs')
    
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
    expected_url <- "http://127.0.0.1:3158/?data=flights&tab=Graphs&variable=Test%20values%20with%20spaces%20and%20sh%2A%26%25t.%21&comparison=expected_value_comparison&sum_by_variable=expected_value_sum_by_variable&color_variable=expected_value_color_variable&facet_variable=expected_value_facet_variable&size_variable=expected_value_size_variable&numeric_group_comp_variable=TRUE&numeric_aggregation_function=month&numeric_aggregation=week&multi_value_delimiter=%3B%20&filter_factor_lump_number=20&label_variables=this&label_variables=has&label_variables=multiple&label_variables=values&annotate_points=FALSE&show_points=FALSE&year_over_year=TRUE&include_zero_y_axis=FALSE&numeric_graph_type=Boxplot%20Whoot&categoric_view_type=Bar%20Whoot&order_by_variable=Default%20Whoot&show_variable_totals=FALSE&show_comparison_totals=FALSE&histogram_bins=1000&transparency=1999&jitter=TRUE&numeric_aggregation_count_minimum=3009&numeric_show_resampled_conf_int=TRUE&trend_line=None%20Whoot&trend_line_se=Yes%20Whoot&ts_date_floor=None%20Whoot&ts_date_break_format=Auto%20Whoot&ts_breaks_width=not%20null&scale_x_log_base_10=TRUE&x_zoom_min=not%20NA&x_zoom_max=40&scale_y_log_base_10=TRUE&y_zoom_min=not%20NA&y_zoom_max=not%20NA&custom_title=This%20is%20my%20title&custom_subtitle=This%20is%20my%20subtitle&custom_x_axis_label=This%20is%20my%20X%20Axis%20Label&custom_y_axis_label=This%20is%20my%20Y%20Axis%20Label&custom_caption=This%20is%20my%20caption&custom_tag=This%20is%20my%20Tag&pretty_text=TRUE&base_size=155&vertical_annotations=vertical%20annotations&horizontal_annotations=horizontal%20annotations"
    expect_equal(custom_url, expected_url)
    expect_equal(nchar(custom_url), 1579)
    
    # extract_url_parameters only expects the ?.... part of the url
    extracted_parameters <- extract_url_parameters(url_search=str_replace(custom_url, 'http://127.0.0.1:3158/', ''))
    expect_identical(names(extracted_parameters), c('data', 'tab', names(mock_input)))
    
    var_plots_extracted_parameters <- extracted_parameters[which(!names(extracted_parameters) %in% c('data', 'tab'))]
    expect_identical(names(var_plots_extracted_parameters), names(mock_input))
    
    for(variable in names(mock_input)) {
        
        expect_equal(var_plots_extracted_parameters[[variable]], mock_input[[variable]])
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
                                                         current_value=NULL)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=NULL,
                                                         current_value=global__select_variable_optional)
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    comparison_selection <- var_plots__comparison__logic(dataset=dataset,
                                                         primary_variable=NULL,
                                                         current_value="Doesn't Matter Column Name")
    expect_identical(comparison_selection$choices, c(global__select_variable_optional, column_names))
    expect_equal(comparison_selection$selected, global__select_variable_optional)
    
    ########
    # Default Primary Variable
    ########
    primary_variable_default <- var_plots__input_list_default_values[['var_plots__variable']]
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
    
    ########
    # Date Primary Variable
    # only numeric names should be available as possible choices for the comparison
    ########
    results <- select_preloaded_dataset("Flights", defualt_path = '../')
    dataset <- results$dataset
    numeric_column_names <- colnames(dataset %>% select_if(is.numeric))
    
    primary_selection <- 'date'
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
    categoric_column_names <- colnames(dataset %>% select_if(purrr::negate(is.numeric)))
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
    primary_variable_default <- var_plots__input_list_default_values[['var_plots__variable']]
    comparison_variable_default <- var_plots__input_list_default_values[['var_plots__comparison']]
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
    categoric_column_names <- colnames(dataset %>% select_if(purrr::negate(is.numeric)))
    numeric_column_names <- colnames(dataset %>% select_if(is.numeric))
    
    primary_selection <- 'date'
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

    primary_selection <- 'date'
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
