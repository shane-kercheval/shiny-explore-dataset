library('testthat')
library(tidyverse)
library(rtools)
library(hms)

source('../helper_scripts/generic_helpers.R', chdir = TRUE)
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))


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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
    
    expect_equal(filter_results[[2]][[3]], "color: Not Filtering")
    

    expect_true(str_detect(filter_results[[2]][[4]], "price:"))
    expect_true(str_detect(filter_results[[2]][[4]], "326"))
    expect_true(str_detect(filter_results[[2]][[4]], "18823"))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
    # dataset at time of cut being filtered
    t <- dataset %>% filter(!is.na(carat),
                            carat >= 0.5,
                            carat <= 2)
    num_na <- sum(is.na(t$cut))
    num_filtered_out <- sum(!is.na(t$cut) & !t$cut %in% c('Ideal', 'Premium', 'Good'))
    expect_true(str_detect(filter_results[[2]][[2]], "cut:"))
    expect_true(str_detect(filter_results[[2]][[2]], "Ideal, Premium, Good"))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
    

    expect_true(str_detect(filter_results[[2]][[3]], "price:"))
    expect_true(str_detect(filter_results[[2]][[3]], "326"))
    expect_true(str_detect(filter_results[[2]][[3]], "18823"))
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
    expect_true(str_detect(filter_results[[2]][[2]], "18823"))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))

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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_false(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    #expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))

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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))

    # get dataset to the point date will be filtered, which is after hms is filtered
    t <- dataset %>% filter(!is.na(hms),
                            hms >= hm(global_filter_list[[filter_selections[1]]][1]),
                            hms <= hm(global_filter_list[[filter_selections[1]]][2]))
    num_na <- sum(is.na(t$date))
    num_filtered_out <- sum(t$date < global_filter_list[[filter_selections[2]]][1] | t$date > global_filter_list[[filter_selections[2]]][2], na.rm = TRUE)
    expect_true(str_detect(filter_results[[2]][[2]], "date:"))
    expect_true(str_detect(filter_results[[2]][[2]], as.character(global_filter_list[[filter_selections[2]]][1])))
    expect_true(str_detect(filter_results[[2]][[2]], as.character(global_filter_list[[filter_selections[2]]][2])))
    #expect_false(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_false(str_detect(filter_results[[2]][[3]], paste(as.character(num_na), "rows with missing values")))
    #expect_true(str_detect(filter_results[[2]][[3]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[3]], paste(as.character(num_filtered_out), "rows")))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
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
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[1]], paste(as.character(num_filtered_out), "rows")))
    
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
    expect_false(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    #expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_na), "rows with missing values")))
    expect_true(str_detect(filter_results[[2]][[2]], paste(as.character(num_filtered_out), "rows")))
    writeLines(paste0(filter_results[[2]], collapse = "\n\n"), "output_files/filter_message__flights__date__hms.txt")
})
