library('testthat')
library(tidyverse)
library(rtools)
library(hms)

source('../helper_scripts/generic_helpers.R', chdir = TRUE)
Sys.setenv(TZ='UTC')

#source('unit_test_helper.R', chdir = TRUE)

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

#' @param vertical_annotations list of vectors; each list item is an annotation; first value of vector is x location of line; second value is text annotation
#' @param y_location y location of text
add_vertical_annotations <- function(ggplot_object, vertical_annotations, y_location=0, is_date=FALSE) {
    
    if(!is.null(vertical_annotations) &&
       vertical_annotations != "" &&
       length(vertical_annotations) > 0) {
        
        for(annotation in vertical_annotations) {
            
            if(is_date) {
                x_location <- ymd(annotation[1])
            } else {
                x_location <- as.numeric(annotation[1])
            }
            
            ggplot_object <- ggplot_object +
                geom_vline(xintercept = x_location, color='red') +
                geom_text(x=x_location,
                          y=y_location,
                          label=annotation[2],
                          color="red",
                          check_overlap=TRUE,
                          angle=90,
                          hjust=0,
                          vjust=-0.5)
        }
    }
    
    return (ggplot_object)
}

#' @param horizontal_annotations list of vectors; each list item is an annotation; first value of vector is y location of line; second value is text annotation
#' @param x_location x location of text
add_horizontal_annotations <- function(ggplot_object, horizontal_annotations, x_location=0) {
    
    if(!is.null(horizontal_annotations) &&
       horizontal_annotations != "" &&
       length(horizontal_annotations) > 0) {
        
        for(annotation in horizontal_annotations) {
            
            y_value <- as.numeric(annotation[1])
            ggplot_object <- ggplot_object +
                geom_hline(yintercept = y_value, color='red') +
                geom_text(y=y_value,
                          x=x_location,
                          label=annotation[2],
                          color="red",
                          check_overlap=TRUE,
                          hjust=-0.2,
                          vjust=-0.5)
        }
    }
    return (ggplot_object)
}

test_that("add_x_annotations", {
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