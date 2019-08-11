global__should_log_message <- TRUE
global__log_message_to <- c('browser_console', 'local_console')

global__select_variable <- "<Select>"
global__select_variable_optional <- "<Select (optional)>"

global__golden_ratio <- 0.618

global__date_part_vector <- c('day', 'week', 'month', 'quarter', 'year')
names(global__date_part_vector) <- c('Day', 'Week', 'Month', 'Quarter', 'Year')

global__date_break_format_vector <- c('Auto', '%Y-%m-%d', '%Y-%W', '%Y-%m', '%Y')
names(global__date_break_format_vector) <- c('Auto', 'Day', 'Week', 'Month', 'Year')

global__url_params_filter_prefix <- '!!_'

var_plots__default_values <- list(

    'var_plots__variable' = global__select_variable,
    'var_plots__comparison' = global__select_variable_optional,
    'var_plots__sum_by_variable' = global__select_variable_optional,
    'var_plots__color_variable' = global__select_variable_optional,
    'var_plots__facet_variable' = global__select_variable_optional,
    'var_plots__size_variable' = global__select_variable_optional,
    'var_plots__numeric_group_comp_variable' = FALSE,
    'var_plots__numeric_aggregation_function' = "Boxplot",
    'var_plots__numeric_aggregation' = "Mean",
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
    'var_plots__trend_line' = "None",
    'var_plots__trend_extend_date' = "0000-01-01",
    'var_plots__trend_line_se' = "No",
    'var_plots__ts_date_floor' = global__date_part_vector[3],
    'var_plots__ts_date_break_format' = global__date_break_format_vector[1],
    'var_plots__ts_breaks_width' = "",
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
    'var_plots__horizontal_annotations' = "",
    'var_plots__map_format' = FALSE,
    'var_plots__map_borders_database' = "",
    'var_plots__map_borders_regions' = ""
)

var_plots__variable_types <- list(

    'var_plots__variable' = 'updateSelectInput',
    'var_plots__comparison' = 'updateSelectInput',
    'var_plots__sum_by_variable' = 'updateSelectInput',
    'var_plots__color_variable' = 'updateSelectInput',
    'var_plots__facet_variable' = 'updateSelectInput',
    'var_plots__size_variable' = 'updateSelectInput',
    'var_plots__numeric_group_comp_variable' = 'updateCheckboxInput',
    'var_plots__numeric_aggregation_function' = 'updateSelectInput',
    'var_plots__numeric_aggregation' = 'updateSelectInput',
    'var_plots__multi_value_delimiter' = 'updateTextInput',
    'var_plots__filter_factor_lump_number'= 'updateSliderTextInput',
    'var_plots__label_variables' = 'updateSelectInput',
    'var_plots__annotate_points' = 'updateCheckboxInput',
    'var_plots__show_points' = 'updateCheckboxInput',
    'var_plots__year_over_year' = 'updateCheckboxInput',
    'var_plots__include_zero_y_axis' = 'updateCheckboxInput',
    'var_plots__numeric_graph_type' = 'updateSelectInput',
    'var_plots__categoric_view_type' = 'updateSelectInput',
    'var_plots__order_by_variable' = 'updateSelectInput',
    'var_plots__show_variable_totals' = 'updateCheckboxInput',
    'var_plots__show_comparison_totals' = 'updateCheckboxInput',
    'var_plots__histogram_bins' = 'updateNumericInput',
    'var_plots__transparency' = 'updateSliderTextInput',
    'var_plots__jitter' = 'updateCheckboxInput',
    'var_plots__numeric_aggregation_count_minimum' = 'updateNumericInput',
    'var_plots__numeric_show_resampled_conf_int' = 'updateCheckboxInput',
    'var_plots__trend_line' = 'updateRadioButtons',
    'var_plots__trend_extend_date' = 'updateRadioButtons',
    'var_plots__trend_line_se' = 'updateRadioButtons',
    'var_plots__ts_date_floor' = 'updateSelectInput',
    'var_plots__ts_date_break_format' = 'updateSelectInput',
    'var_plots__ts_breaks_width' = 'updateTextInput',
    'var_plots__scale_x_log_base_10' = 'updateCheckboxInput',
    'var_plots__x_zoom_min' = 'updateNumericInput',
    'var_plots__x_zoom_max' = 'updateNumericInput',
    'var_plots__scale_y_log_base_10' = 'updateCheckboxInput',
    'var_plots__y_zoom_min' = 'updateNumericInput',
    'var_plots__y_zoom_max' = 'updateNumericInput',
    'var_plots__custom_title' = 'updateTextInput',
    'var_plots__custom_subtitle' = 'updateTextInput',
    'var_plots__custom_x_axis_label' = 'updateTextInput',
    'var_plots__custom_y_axis_label' = 'updateTextInput',
    'var_plots__custom_caption' = 'updateTextInput',
    'var_plots__custom_tag' = 'updateTextInput',
    'var_plots__pretty_text' = 'updateCheckboxInput',
    'var_plots__base_size' = 'updateSliderTextInput',
    'var_plots__vertical_annotations' = 'updateTextAreaInput',
    'var_plots__horizontal_annotations' = 'updateTextAreaInput',
    'var_plots__map_format' = 'updateCheckboxInput',
    'var_plots__map_borders_database' = 'updateTextInput',
    'var_plots__map_borders_regions' = 'updateTextInput'

)
