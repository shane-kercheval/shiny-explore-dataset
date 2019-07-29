should_log_message <- TRUE

global__select_variable <- "<Select>"
global__select_variable_optional <- "<Select (optional)>"

global__num_num_aggregation_function_default <- 'Boxplot'
global__var_plots__numeric_aggregation_default <- 'Mean'

global__golden_ratio <- 0.618

global__date_part_vector <- c('None', 'day', 'week', 'month', 'quarter', 'year')
names(global__date_part_vector) <- c('None', 'Day', 'Week', 'Month', 'Quarter', 'Year')

global__date_break_format_vector <- c('Auto', '%Y-%m-%d', '%Y-%W', '%Y-%m', '%Y')
names(global__date_break_format_vector) <- c('Auto', 'Day', 'Week', 'Month', 'Year')



global__url_param_step_levels <- c("Parsed Parameters",
                                   "Updated Non-Dynamic Parameters",
                                   "Loaded Dataset",
                                   "Updated Dynamic Variables (Triggered by Dataset)",
                                   "Updated Comparison Variable",
                                   "Updated Color Variable",
                                   "Updated Categoric View Variable",
                                   "Can Create Graph from URL Parameters",
                                   "Successfully Created Graph from URL Parameters")

global__url_param_possible_steps <- factor(global__url_param_step_levels,
                                           levels=global__url_param_step_levels,
                                           ordered=TRUE)

create_url_param_step <- function(step_name) {
    #step_name <- "Loaded Dataset"
    stopifnot(step_name %in% global__url_param_possible_steps)
    
    return(factor(step_name, levels=global__url_param_step_levels, ordered=TRUE))
}

