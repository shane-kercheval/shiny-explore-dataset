#' @param session session from the app server
#' @param params named list of url params with corresponding values; should only be var_plot__ params
update_var_plot_variables_from_url_params <- function(session, params, dataset, input) {

    # the variables that are dynamic based on the dataset will not have been loaded
    # and will need to be initialized with the list of choices
    # for some variables, the choices are based on other variables
    # so we need to manually build them up. 

    column_names <- colnames(dataset)
    numeric_column_names <- colnames(dataset %>% select_if(is.numeric))
    categoric_column_names <- colnames(dataset %>% select_if(purrr::negate(is.numeric)))

    #######################################################################
    # Update Primary Variable - cache selected for comparison/color logic
    #######################################################################
    selected_variable <- global__select_variable
    if (!is.null(params[['variable']])) {

        selected_variable <- params[['variable']]
        log_message_variable('updating variable', params[['variable']])
    }
    updateSelectInput(session, 'var_plots__variable',
                      choices=c(global__select_variable, column_names),
                      selected=selected_variable)   
    
    #######################################################################
    # Update Comparison Variable - cache selected for color logic
    #######################################################################
    selected_comparison <- global__select_variable_optional
    if (!is.null(params[['comparison']])) {

        selected_comparison <- params[['comparison']]
        log_message_variable('updating comparison', params[['comparison']])
    }
    results <- var_plots__comparison__logic(dataset=dataset,
                                            primary_variable=selected_variable,
                                            current_value=selected_comparison)
    updateSelectInput(session, 'var_plots__comparison',
                      choices=results$choices,
                      selected=results$selected)
    
    #######################################################################
    # Update Color Variable
    #######################################################################
    selected_color <- global__select_variable_optional
    if (!is.null(params[['color_variable']])) {

        selected_color <- params[['color_variable']]
        log_message_variable('updating color_variable', params[['color_variable']])
    }
    results <- var_plots__color__logic(dataset=dataset,
                                            primary_variable=selected_variable,
                                            comparison_variable=selected_comparison,
                                            current_value=selected_color)
    updateSelectInput(session, 'var_plots__color_variable',
                      choices=results$choices,
                      selected=results$selected)


    #######################################################################
    # Update Sum-By-Variable
    #######################################################################
    selected_sum_by_variable <- global__select_variable_optional
    if (!is.null(params[['sum_by_variable']])) {

        selected_sum_by_variable <- params[['sum_by_variable']]
        log_message_variable('updating sum_by_variable', params[['sum_by_variable']])
    }
    updateSelectInput(session, 'var_plots__sum_by_variable',
                      choices=c(global__select_variable_optional, numeric_column_names),
                      selected=selected_sum_by_variable)

    #######################################################################
    # Update Categoric View
    #######################################################################
    if (!is.null(params[['categoric_view_type']])) {
        log_message_variable('updating categoric_view_type', params[['categoric_view_type']])
        updateSelectInput(session, 'var_plots__categoric_view_type', selected=params[['categoric_view_type']])
    }
    selected_value <- "Bar"
    if (!is.null(params[['categoric_view_type']])) {

        selected_value <- params[['categoric_view_type']]
        log_message_variable('updating categoric_view_type', params[['categoric_view_type']])
    }
    results <- var_plots__categoric_view_type__logic(dataset=dataset,
                                                     comparison_variable=selected_comparison,
                                                     sum_by_variable=selected_sum_by_variable,
                                                     current_value=selected_value)
    updateSelectInput(session, 'var_plots__categoric_view_type',
                      choices=results$choices,
                      selected=results$selected)

    #######################################################################
    # Update Other Dynamic values that don't depend on other variables
    #######################################################################
    selected_facet_variable <- global__select_variable_optional
    if (!is.null(params[['facet_variable']])) {

        selected_facet_variable <- params[['facet_variable']]
        log_message_variable('updating facet_variable', params[['facet_variable']])
    }
    updateSelectInput(session, 'var_plots__facet_variable',
                      choices=c(global__select_variable_optional, categoric_column_names),
                      selected=selected_facet_variable)

    selected_size_variable <- global__select_variable_optional
    if (!is.null(params[['size_variable']])) {

        selected_size_variable <- params[['size_variable']]
        log_message_variable('updating size_variable', params[['size_variable']])
    }
    updateSelectInput(session, 'var_plots__size_variable',
                      choices=c(global__select_variable_optional, column_names),
                      selected=selected_size_variable)

    #######################################################################
    # Update Non-Dynamic
    # These should already have `choices` defined in UI
    #######################################################################
    if (!is.null(params[['numeric_group_comp_variable']])) {

        log_message_variable('updating numeric_group_comp_variable', params[['numeric_group_comp_variable']])
        updateCheckboxInput(session, 'var_plots__numeric_group_comp_variable', value=params[['numeric_group_comp_variable']])
    }
    if (!is.null(params[['numeric_aggregation_function']])) {

        log_message_variable('updating numeric_aggregation_function', params[['numeric_aggregation_function']])
        updateSelectInput(session, 'var_plots__numeric_aggregation_function', selected=params[['numeric_aggregation_function']])
    }
    if (!is.null(params[['numeric_aggregation']])) {

        log_message_variable('updating numeric_aggregation', params[['numeric_aggregation']])
        updateSelectInput(session, 'var_plots__numeric_aggregation', selected=params[['numeric_aggregation']])
    }
    if (!is.null(params[['multi_value_delimiter']])) {

        log_message_variable('updating multi_value_delimiter', params[['multi_value_delimiter']])
        updateTextInput(session, 'var_plots__multi_value_delimiter', value=params[['multi_value_delimiter']])
    }
    if (!is.null(params[['filter_factor_lump_number']])) {
        log_message_variable('updating filter_factor_lump_number', params[['filter_factor_lump_number']])
        updateSliderTextInput(session, 'var_plots__filter_factor_lump_number', selected=params[['filter_factor_lump_number']])
    }
    if (!is.null(params[['label_variables']])) {
        log_message_variable('updating label_variables', params[['label_variables']])
        updateSelectInput(session, 'var_plots__label_variables', selected=params[['label_variables']])
    }
    if (!is.null(params[['annotate_points']])) {
        log_message_variable('updating annotate_points', params[['annotate_points']])
        updateCheckboxInput(session, 'var_plots__annotate_points', value=params[['annotate_points']])
    }
    if (!is.null(params[['show_points']])) {
        log_message_variable('updating show_points', params[['show_points']])
        updateCheckboxInput(session, 'var_plots__show_points', value=params[['show_points']])
    }
    if (!is.null(params[['year_over_year']])) {
        log_message_variable('updating year_over_year', params[['year_over_year']])
        updateCheckboxInput(session, 'var_plots__year_over_year', value=params[['year_over_year']])
    }
    if (!is.null(params[['include_zero_y_axis']])) {
        log_message_variable('updating include_zero_y_axis', params[['include_zero_y_axis']])
        updateCheckboxInput(session, 'var_plots__include_zero_y_axis', value=params[['include_zero_y_axis']])
    }
    if (!is.null(params[['numeric_graph_type']])) {
        log_message_variable('updating numeric_graph_type', params[['numeric_graph_type']])
        updateSelectInput(session, 'var_plots__numeric_graph_type', selected=params[['numeric_graph_type']])
    }
    if (!is.null(params[['order_by_variable']])) {
        log_message_variable('updating order_by_variable', params[['order_by_variable']])
        updateSelectInput(session, 'var_plots__order_by_variable', selected=params[['order_by_variable']])
    }
    if (!is.null(params[['show_variable_totals']])) {
        log_message_variable('updating show_variable_totals', params[['show_variable_totals']])
        updateCheckboxInput(session, 'var_plots__show_variable_totals', value=params[['show_variable_totals']])
    }
    if (!is.null(params[['show_comparison_totals']])) {
        log_message_variable('updating show_comparison_totals', params[['show_comparison_totals']])
        updateCheckboxInput(session, 'var_plots__show_comparison_totals', value=params[['show_comparison_totals']])
    }
    if (!is.null(params[['histogram_bins']])) {
        log_message_variable('updating histogram_bins', params[['histogram_bins']])
        updateNumericInput(session, 'var_plots__histogram_bins', value=params[['histogram_bins']])
    }
    if (!is.null(params[['transparency']])) {
        log_message_variable('updating transparency', params[['transparency']])
        updateSliderTextInput(session, 'var_plots__transparency', selected=params[['transparency']])
    }
    if (!is.null(params[['jitter']])) {
        log_message_variable('updating jitter', params[['jitter']])
        updateCheckboxInput(session, 'var_plots__jitter', value=params[['jitter']])
    }
    if (!is.null(params[['numeric_aggregation_count_minimum']])) {
        log_message_variable('updating numeric_aggregation_count_minimum', params[['numeric_aggregation_count_minimum']])
        updateNumericInput(session, 'var_plots__numeric_aggregation_count_minimum', value=params[['numeric_aggregation_count_minimum']])
    }
    if (!is.null(params[['numeric_show_resampled_conf_int']])) {
        log_message_variable('updating numeric_show_resampled_conf_int', params[['numeric_show_resampled_conf_int']])
        updateCheckboxInput(session, 'var_plots__numeric_show_resampled_conf_int', value=params[['numeric_show_resampled_conf_int']])
    }
    if (!is.null(params[['trend_line']])) {
        log_message_variable('updating trend_line', params[['trend_line']])
        updateRadioButtons(session, 'var_plots__trend_line', selected=params[['trend_line']])
    }
    if (!is.null(params[['trend_line_se']])) {
        log_message_variable('updating trend_line_se', params[['trend_line_se']])
        updateRadioButtons(session, 'var_plots__trend_line_se', selected=params[['trend_line_se']])
    }
    if (!is.null(params[['ts_date_floor']])) {
        log_message_variable('updating ts_date_floor', params[['ts_date_floor']])
        updateSelectInput(session, 'var_plots__ts_date_floor', selected=params[['ts_date_floor']])
    }
    if (!is.null(params[['ts_date_break_format']])) {
        log_message_variable('updating ts_date_break_format', params[['ts_date_break_format']])
        updateSelectInput(session, 'var_plots__ts_date_break_format', selected=params[['ts_date_break_format']])
    }
    if (!is.null(params[['ts_breaks_width']])) {
        log_message_variable('updating ts_breaks_width', params[['ts_breaks_width']])
        updateTextInput(session, 'var_plots__ts_breaks_width', value=params[['ts_breaks_width']])
    }
    if (!is.null(params[['scale_x_log_base_10']])) {
        log_message_variable('updating scale_x_log_base_10', params[['scale_x_log_base_10']])
        updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=params[['scale_x_log_base_10']])
    }
    if (!is.null(params[['x_zoom_min']])) {
        log_message_variable('updating x_zoom_min', params[['x_zoom_min']])
        updateNumericInput(session, 'var_plots__x_zoom_min', value=params[['x_zoom_min']])
    }
    if (!is.null(params[['x_zoom_max']])) {
        log_message_variable('updating x_zoom_max', params[['x_zoom_max']])
        updateNumericInput(session, 'var_plots__x_zoom_max', value=params[['x_zoom_max']])
    }
    if (!is.null(params[['scale_y_log_base_10']])) {
        log_message_variable('updating scale_y_log_base_10', params[['scale_y_log_base_10']])
        updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=params[['scale_y_log_base_10']])
    }
    if (!is.null(params[['y_zoom_min']])) {
        log_message_variable('updating y_zoom_min', params[['y_zoom_min']])
        updateNumericInput(session, 'var_plots__y_zoom_min', value=params[['y_zoom_min']])
    }
    if (!is.null(params[['y_zoom_max']])) {
        log_message_variable('updating y_zoom_max', params[['y_zoom_max']])
        updateNumericInput(session, 'var_plots__y_zoom_max', value=params[['y_zoom_max']])
    }
    if (!is.null(params[['custom_title']])) {
        log_message_variable('updating custom_title', params[['custom_title']])
        updateTextInput(session, 'var_plots__custom_title', value=params[['custom_title']])
    }
    if (!is.null(params[['custom_subtitle']])) {
        log_message_variable('updating custom_subtitle', params[['custom_subtitle']])
        updateTextInput(session, 'var_plots__custom_subtitle', value=params[['custom_subtitle']])
    }
    if (!is.null(params[['custom_x_axis_label']])) {
        log_message_variable('updating custom_x_axis_label', params[['custom_x_axis_label']])
        updateTextInput(session, 'var_plots__custom_x_axis_label', value=params[['custom_x_axis_label']])
    }
    if (!is.null(params[['custom_y_axis_label']])) {
        log_message_variable('updating custom_y_axis_label', params[['custom_y_axis_label']])
        updateTextInput(session, 'var_plots__custom_y_axis_label', value=params[['custom_y_axis_label']])
    }
    if (!is.null(params[['custom_caption']])) {
        log_message_variable('updating custom_caption', params[['custom_caption']])
        updateTextInput(session, 'var_plots__custom_caption', value=params[['custom_caption']])
    }
    if (!is.null(params[['custom_tag']])) {
        log_message_variable('updating custom_tag', params[['custom_tag']])
        updateTextInput(session, 'var_plots__custom_tag', value=params[['custom_tag']])
    }
    if (!is.null(params[['pretty_text']])) {
        log_message_variable('updating pretty_text', params[['pretty_text']])
        updateCheckboxInput(session, 'var_plots__pretty_text', value=params[['pretty_text']])
    }
    if (!is.null(params[['base_size']])) {
        log_message_variable('updating base_size', params[['base_size']])
        updateSliderTextInput(session, 'var_plots__base_size', selected=params[['base_size']])
    }
    if (!is.null(params[['vertical_annotations']])) {
        log_message_variable('updating vertical_annotations', params[['vertical_annotations']])
        updateTextAreaInput(session, 'var_plots__vertical_annotations', value=params[['vertical_annotations']])
    }
    if (!is.null(params[['horizontal_annotations']])) {
        log_message_variable('updating horizontal_annotations', params[['horizontal_annotations']])
        updateTextAreaInput(session, 'var_plots__horizontal_annotations', value=params[['horizontal_annotations']])
    }
}
