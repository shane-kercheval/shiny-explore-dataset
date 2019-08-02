global__should_log_message <- TRUE

global__select_variable <- "<Select>"
global__select_variable_optional <- "<Select (optional)>"

global__num_num_aggregation_function_default <- 'Boxplot'
global__var_plots__numeric_aggregation_default <- 'Mean'

global__golden_ratio <- 0.618

global__date_part_vector <- c('None', 'day', 'week', 'month', 'quarter', 'year')
names(global__date_part_vector) <- c('None', 'Day', 'Week', 'Month', 'Quarter', 'Year')

global__date_break_format_vector <- c('Auto', '%Y-%m-%d', '%Y-%W', '%Y-%m', '%Y')
names(global__date_break_format_vector) <- c('Auto', 'Day', 'Week', 'Month', 'Year')

var_plots__input_list_default_values <- list(

    'var_plots__variable' = global__select_variable,
    'var_plots__comparison' = global__select_variable_optional,
    'var_plots__sum_by_variable' = global__select_variable_optional,
    'var_plots__color_variable' = global__select_variable_optional,
    'var_plots__facet_variable' = global__select_variable_optional,
    'var_plots__size_variable' = global__select_variable_optional,
    'var_plots__numeric_group_comp_variable' = FALSE,
    'var_plots__numeric_aggregation_function' = global__num_num_aggregation_function_default,
    'var_plots__numeric_aggregation' = global__var_plots__numeric_aggregation_default,
    'var_plots__multi_value_delimiter' = "",
    'var_plots__filter_factor_lump_number'="10",
    'var_plots__label_variables' = NULL,
    'var_plots__annotate_points' = TRUE,
    'var_plots__show_points' = TRUE,
    'var_plots__year_over_year' = FALSE,
    'var_plots__include_zero_y_axis' = TRUE,
    'var_plots__numeric_graph_type' = "Boxplot",
    'var_plots__categoric_view_type' = "Bar",
    'var_plots__order_by_variable' = "Default",
    'var_plots__show_variable_totals' = TRUE,
    'var_plots__show_comparison_totals' = TRUE,
    'var_plots__histogram_bins' = 30,
    'var_plots__transparency' = 60,
    'var_plots__jitter' = FALSE,
    'var_plots__numeric_aggregation_count_minimum' = 30,
    'var_plots__numeric_show_resampled_conf_int' = FALSE,
    'var_plots__trend_line' = 'None',
    'var_plots__trend_line_se' = 'Yes',
    'var_plots__ts_date_floor' = 'None',
    'var_plots__ts_date_break_format' = 'Auto',
    'var_plots__ts_breaks_width' = '',
    'var_plots__scale_x_log_base_10' = FALSE,
    'var_plots__x_zoom_min' = NA,
    'var_plots__x_zoom_max' = NA,
    'var_plots__scale_y_log_base_10' = FALSE,
    'var_plots__y_zoom_min' = NA,
    'var_plots__y_zoom_max' = NA,
    'var_plots__custom_title' = "",
    'var_plots__custom_subtitle' = "",
    'var_plots__custom_x_axis_label' = "",
    'var_plots__custom_y_axis_label' = "",
    'var_plots__custom_caption' = "",
    'var_plots__custom_tag' = "",
    'var_plots__pretty_text' = FALSE,
    'var_plots__base_size' = 15,
    'var_plots__vertical_annotations' = "",
    'var_plots__horizontal_annotations' = ""
)
