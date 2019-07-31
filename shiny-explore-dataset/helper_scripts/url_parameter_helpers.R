#' @param session session from the app server
#' @param params named list of url params with corresponding values; should only be var_plot__ params
update_var_plot_variables_from_url_params <- function(session, params) {

    # if (!is.null(params[['variable']])) {

    #     log_message_variable('variable', params[['variable']])
    #     updateSelectInput(session, 'var_plots__variable', selected=params[['variable']])
    # }
    # if (!is.null(params[['comparison']])) {

    #     log_message_variable('comparison', params[['comparison']])
    #     updateSelectInput(session, 'var_plots__comparison', selected=params[['comparison']])
    # }
    # if (!is.null(params[['sum_by_variable']])) {

    #     log_message_variable('sum_by_variable', params[['sum_by_variable']])
    #     updateSelectInput(session, 'var_plots__sum_by_variable', selected=params[['sum_by_variable']])
    # }
    # if (!is.null(params[['color_variable']])) {

    #     log_message_variable('color_variable', params[['color_variable']])
    #     updateSelectInput(session, 'var_plots__color_variable', selected=params[['color_variable']])
    # }
    # if (!is.null(params[['facet_variable']])) {

    #     log_message_variable('facet_variable', params[['facet_variable']])
    #     updateSelectInput(session, 'var_plots__facet_variable', selected=params[['facet_variable']])
    # }
    # if (!is.null(params[['size_variable']])) {

    #     log_message_variable('size_variable', params[['size_variable']])
    #     updateSelectInput(session, 'var_plots__size_variable', selected=params[['size_variable']])
    # }
    if (!is.null(params[['numeric_group_comp_variable']])) {

        log_message_variable('numeric_group_comp_variable', params[['numeric_group_comp_variable']])
        updateCheckboxInput(session, 'var_plots__numeric_group_comp_variable', value=params[['numeric_group_comp_variable']])
    }
    if (!is.null(params[['numeric_aggregation_function']])) {

        log_message_variable('numeric_aggregation_function', params[['numeric_aggregation_function']])
        updateSelectInput(session, 'var_plots__numeric_aggregation_function', selected=params[['numeric_aggregation_function']])
    }
    if (!is.null(params[['numeric_aggregation']])) {

        log_message_variable('numeric_aggregation', params[['numeric_aggregation']])
        updateSelectInput(session, 'var_plots__numeric_aggregation', selected=params[['numeric_aggregation']])
    }
    if (!is.null(params[['multi_value_delimiter']])) {

        log_message_variable('multi_value_delimiter', params[['multi_value_delimiter']])
        updateTextInput(session, 'var_plots__multi_value_delimiter', value=params[['multi_value_delimiter']])
    }
    if (!is.null(params[['filter_factor_lump_number']])) {
        log_message_variable('filter_factor_lump_number', params[['filter_factor_lump_number']])
        updateSliderTextInput(session, 'var_plots__filter_factor_lump_number', selected=params[['filter_factor_lump_number']])
    }
    if (!is.null(params[['label_variables']])) {
        log_message_variable('label_variables', params[['label_variables']])
        updateSelectInput(session, 'var_plots__label_variables', selected=params[['label_variables']])
    }
    if (!is.null(params[['annotate_points']])) {
        log_message_variable('annotate_points', params[['annotate_points']])
        updateCheckboxInput(session, 'var_plots__annotate_points', value=params[['annotate_points']])
    }
    if (!is.null(params[['show_points']])) {
        log_message_variable('show_points', params[['show_points']])
        updateCheckboxInput(session, 'var_plots__show_points', value=params[['show_points']])
    }
    if (!is.null(params[['year_over_year']])) {
        log_message_variable('year_over_year', params[['year_over_year']])
        updateCheckboxInput(session, 'var_plots__year_over_year', value=params[['year_over_year']])
    }
    if (!is.null(params[['include_zero_y_axis']])) {
        log_message_variable('include_zero_y_axis', params[['include_zero_y_axis']])
        updateCheckboxInput(session, 'var_plots__include_zero_y_axis', value=params[['include_zero_y_axis']])
    }
    if (!is.null(params[['numeric_graph_type']])) {
        log_message_variable('numeric_graph_type', params[['numeric_graph_type']])
        updateSelectInput(session, 'var_plots__numeric_graph_type', selected=params[['numeric_graph_type']])
    }
    if (!is.null(params[['categoric_view_type']])) {
        log_message_variable('categoric_view_type', params[['categoric_view_type']])
        updateSelectInput(session, 'var_plots__categoric_view_type', selected=params[['categoric_view_type']])
    }
    if (!is.null(params[['order_by_variable']])) {
        log_message_variable('order_by_variable', params[['order_by_variable']])
        updateSelectInput(session, 'var_plots__order_by_variable', selected=params[['order_by_variable']])
    }
    if (!is.null(params[['show_variable_totals']])) {
        log_message_variable('show_variable_totals', params[['show_variable_totals']])
        updateCheckboxInput(session, 'var_plots__show_variable_totals', value=params[['show_variable_totals']])
    }
    if (!is.null(params[['show_comparison_totals']])) {
        log_message_variable('show_comparison_totals', params[['show_comparison_totals']])
        updateCheckboxInput(session, 'var_plots__show_comparison_totals', value=params[['show_comparison_totals']])
    }
    if (!is.null(params[['histogram_bins']])) {
        log_message_variable('histogram_bins', params[['histogram_bins']])
        updateNumericInput(session, 'var_plots__histogram_bins', value=params[['histogram_bins']])
    }
    if (!is.null(params[['transparency']])) {
        log_message_variable('transparency', params[['transparency']])
        updateSliderTextInput(session, 'var_plots__transparency', selected=params[['transparency']])
    }
    if (!is.null(params[['jitter']])) {
        log_message_variable('jitter', params[['jitter']])
        updateCheckboxInput(session, 'var_plots__jitter', value=params[['jitter']])
    }
    if (!is.null(params[['numeric_aggregation_count_minimum']])) {
        log_message_variable('numeric_aggregation_count_minimum', params[['numeric_aggregation_count_minimum']])
        updateNumericInput(session, 'var_plots__numeric_aggregation_count_minimum', value=params[['numeric_aggregation_count_minimum']])
    }
    if (!is.null(params[['numeric_show_resampled_conf_int']])) {
        log_message_variable('numeric_show_resampled_conf_int', params[['numeric_show_resampled_conf_int']])
        updateCheckboxInput(session, 'var_plots__numeric_show_resampled_conf_int', value=params[['numeric_show_resampled_conf_int']])
    }
    if (!is.null(params[['trend_line']])) {
        log_message_variable('trend_line', params[['trend_line']])
        updateRadioButtons(session, 'var_plots__trend_line', selected=params[['trend_line']])
    }
    if (!is.null(params[['trend_line_se']])) {
        log_message_variable('trend_line_se', params[['trend_line_se']])
        updateRadioButtons(session, 'var_plots__trend_line_se', selected=params[['trend_line_se']])
    }
    if (!is.null(params[['ts_date_floor']])) {
        log_message_variable('ts_date_floor', params[['ts_date_floor']])
        updateSelectInput(session, 'var_plots__ts_date_floor', selected=params[['ts_date_floor']])
    }
    if (!is.null(params[['ts_date_break_format']])) {
        log_message_variable('ts_date_break_format', params[['ts_date_break_format']])
        updateSelectInput(session, 'var_plots__ts_date_break_format', selected=params[['ts_date_break_format']])
    }
    if (!is.null(params[['ts_breaks_width']])) {
        log_message_variable('ts_breaks_width', params[['ts_breaks_width']])
        updateTextInput(session, 'var_plots__ts_breaks_width', value=params[['ts_breaks_width']])
    }
    if (!is.null(params[['scale_x_log_base_10']])) {
        log_message_variable('scale_x_log_base_10', params[['scale_x_log_base_10']])
        updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=params[['scale_x_log_base_10']])
    }
    if (!is.null(params[['x_zoom_min']])) {
        log_message_variable('x_zoom_min', params[['x_zoom_min']])
        updateNumericInput(session, 'var_plots__x_zoom_min', value=params[['x_zoom_min']])
    }
    if (!is.null(params[['x_zoom_max']])) {
        log_message_variable('x_zoom_max', params[['x_zoom_max']])
        updateNumericInput(session, 'var_plots__x_zoom_max', value=params[['x_zoom_max']])
    }
    if (!is.null(params[['scale_y_log_base_10']])) {
        log_message_variable('scale_y_log_base_10', params[['scale_y_log_base_10']])
        updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=params[['scale_y_log_base_10']])
    }
    if (!is.null(params[['y_zoom_min']])) {
        log_message_variable('y_zoom_min', params[['y_zoom_min']])
        updateNumericInput(session, 'var_plots__y_zoom_min', value=params[['y_zoom_min']])
    }
    if (!is.null(params[['y_zoom_max']])) {
        log_message_variable('y_zoom_max', params[['y_zoom_max']])
        updateNumericInput(session, 'var_plots__y_zoom_max', value=params[['y_zoom_max']])
    }
    if (!is.null(params[['custom_title']])) {
        log_message_variable('custom_title', params[['custom_title']])
        updateTextInput(session, 'var_plots__custom_title', value=params[['custom_title']])
    }
    if (!is.null(params[['custom_subtitle']])) {
        log_message_variable('custom_subtitle', params[['custom_subtitle']])
        updateTextInput(session, 'var_plots__custom_subtitle', value=params[['custom_subtitle']])
    }
    if (!is.null(params[['custom_x_axis_label']])) {
        log_message_variable('custom_x_axis_label', params[['custom_x_axis_label']])
        updateTextInput(session, 'var_plots__custom_x_axis_label', value=params[['custom_x_axis_label']])
    }
    if (!is.null(params[['custom_y_axis_label']])) {
        log_message_variable('custom_y_axis_label', params[['custom_y_axis_label']])
        updateTextInput(session, 'var_plots__custom_y_axis_label', value=params[['custom_y_axis_label']])
    }
    if (!is.null(params[['custom_caption']])) {
        log_message_variable('custom_caption', params[['custom_caption']])
        updateTextInput(session, 'var_plots__custom_caption', value=params[['custom_caption']])
    }
    if (!is.null(params[['custom_tag']])) {
        log_message_variable('custom_tag', params[['custom_tag']])
        updateTextInput(session, 'var_plots__custom_tag', value=params[['custom_tag']])
    }
    if (!is.null(params[['pretty_text']])) {
        log_message_variable('pretty_text', params[['pretty_text']])
        updateCheckboxInput(session, 'var_plots__pretty_text', value=params[['pretty_text']])
    }
    if (!is.null(params[['base_size']])) {
        log_message_variable('base_size', params[['base_size']])
        updateSliderTextInput(session, 'var_plots__base_size', selected=params[['base_size']])
    }
    if (!is.null(params[['vertical_annotations']])) {
        log_message_variable('vertical_annotations', params[['vertical_annotations']])
        updateTextAreaInput(session, 'var_plots__vertical_annotations', value=params[['vertical_annotations']])
    }
    if (!is.null(params[['horizontal_annotations']])) {
        log_message_variable('horizontal_annotations', params[['horizontal_annotations']])
        updateTextAreaInput(session, 'var_plots__horizontal_annotations', value=params[['horizontal_annotations']])
    }
}