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
    if (!is.null(params[['var_plots__variable']])) {

        selected_variable <- params[['var_plots__variable']]
        log_message_variable('updating variable', params[['var_plots__variable']])
    }
    updateSelectInput(session, 'var_plots__variable',
                      choices=c(global__select_variable, column_names),
                      selected=selected_variable)   
    
    #######################################################################
    # Update Comparison Variable - cache selected for color logic
    #######################################################################
    selected_comparison <- global__select_variable_optional
    if (!is.null(params[['var_plots__comparison']])) {

        selected_comparison <- params[['var_plots__comparison']]
        log_message_variable('updating comparison', params[['var_plots__comparison']])
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
    if (!is.null(params[['var_plots__color_variable']])) {

        selected_color <- params[['var_plots__color_variable']]
        log_message_variable('updating color_variable', params[['var_plots__color_variable']])
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
    if (!is.null(params[['var_plots__sum_by_variable']])) {

        selected_sum_by_variable <- params[['var_plots__sum_by_variable']]
        log_message_variable('updating sum_by_variable', params[['var_plots__sum_by_variable']])
    }
    updateSelectInput(session, 'var_plots__sum_by_variable',
                      choices=c(global__select_variable_optional, numeric_column_names),
                      selected=selected_sum_by_variable)

    #######################################################################
    # Update Categoric View
    #######################################################################
    if (!is.null(params[['var_plots__categoric_view_type']])) {
        log_message_variable('updating categoric_view_type', params[['var_plots__categoric_view_type']])
        updateSelectInput(session, 'var_plots__categoric_view_type', selected=params[['var_plots__categoric_view_type']])
    }
    selected_value <- "Bar"
    if (!is.null(params[['var_plots__categoric_view_type']])) {

        selected_value <- params[['var_plots__categoric_view_type']]
        log_message_variable('updating categoric_view_type', params[['var_plots__categoric_view_type']])
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
    if (!is.null(params[['var_plots__facet_variable']])) {

        selected_facet_variable <- params[['var_plots__facet_variable']]
        log_message_variable('updating facet_variable', params[['var_plots__facet_variable']])
    }
    updateSelectInput(session, 'var_plots__facet_variable',
                      choices=c(global__select_variable_optional, categoric_column_names),
                      selected=selected_facet_variable)

    selected_size_variable <- global__select_variable_optional
    if (!is.null(params[['var_plots__size_variable']])) {

        selected_size_variable <- params[['var_plots__size_variable']]
        log_message_variable('updating size_variable', params[['var_plots__size_variable']])
    }
    updateSelectInput(session, 'var_plots__size_variable',
                      choices=c(global__select_variable_optional, column_names),
                      selected=selected_size_variable)

    selected_label_variables <- NULL
    if (!is.null(params[['var_plots__label_variables']])) {

        selected_label_variables <- params[['var_plots__label_variables']]
        log_message_variable('updating label_variables', paste0(params[['var_plots__label_variables']], collapse="; "))
    }
    updateSelectInput(session, 'var_plots__label_variables',
                      choices=column_names,
                      selected=selected_label_variables)

    selected_order_by_variable <- "Default"
    if (!is.null(params[['var_plots__order_by_variable']])) {

        selected_order_by_variable <- params[['var_plots__order_by_variable']]
        log_message_variable('updating order_by_variable', params[['var_plots__order_by_variable']])
    }
    updateSelectInput(session, 'var_plots__order_by_variable',
                      choices=c("Default", "Frequency", numeric_column_names),
                      selected=selected_order_by_variable)

    #######################################################################
    # Update Non-Dynamic
    # These should already have `choices` defined in UI
    #######################################################################
    if (!is.null(params[['var_plots__numeric_group_comp_variable']])) {

        log_message_variable('updating numeric_group_comp_variable', params[['var_plots__numeric_group_comp_variable']])
        updateCheckboxInput(session, 'var_plots__numeric_group_comp_variable', value=params[['var_plots__numeric_group_comp_variable']])
    }
    if (!is.null(params[['var_plots__numeric_aggregation_function']])) {

        log_message_variable('updating numeric_aggregation_function', params[['var_plots__numeric_aggregation_function']])
        updateSelectInput(session, 'var_plots__numeric_aggregation_function', selected=params[['var_plots__numeric_aggregation_function']])
    }
    if (!is.null(params[['var_plots__numeric_aggregation']])) {

        log_message_variable('updating numeric_aggregation', params[['var_plots__numeric_aggregation']])
        updateSelectInput(session, 'var_plots__numeric_aggregation', selected=params[['var_plots__numeric_aggregation']])
    }
    if (!is.null(params[['var_plots__multi_value_delimiter']])) {

        log_message_variable('updating multi_value_delimiter', params[['var_plots__multi_value_delimiter']])
        updateTextInput(session, 'var_plots__multi_value_delimiter', value=params[['var_plots__multi_value_delimiter']])
    }
    if (!is.null(params[['var_plots__filter_factor_lump_number']])) {

        # this object is actually a string, not a number, because "Off" can be chosen, and setting the
        # control as a number doesn't work.
        lump_number_string <- as.character(params[['var_plots__filter_factor_lump_number']])
        log_message_variable('updating filter_factor_lump_number', lump_number_string)
        # perhaps its a bug, but it seems like for all the updateSliderTextInput controls I have to 
        # pass choices as well
        updateSliderTextInput(session,
                              'var_plots__filter_factor_lump_number',
                              choices=as.character(c("Off", seq(1, 10), seq(15, 50, 5))),
                              selected=lump_number_string)
    }
    if (!is.null(params[['var_plots__annotate_points']])) {
        log_message_variable('updating annotate_points', params[['var_plots__annotate_points']])
        updateCheckboxInput(session, 'var_plots__annotate_points', value=params[['var_plots__annotate_points']])
    }
    if (!is.null(params[['var_plots__show_points']])) {
        log_message_variable('updating show_points', params[['var_plots__show_points']])
        updateCheckboxInput(session, 'var_plots__show_points', value=params[['var_plots__show_points']])
    }
    if (!is.null(params[['var_plots__year_over_year']])) {
        log_message_variable('updating year_over_year', params[['var_plots__year_over_year']])
        updateCheckboxInput(session, 'var_plots__year_over_year', value=params[['var_plots__year_over_year']])
    }
    if (!is.null(params[['var_plots__include_zero_y_axis']])) {
        log_message_variable('updating include_zero_y_axis', params[['var_plots__include_zero_y_axis']])
        updateCheckboxInput(session, 'var_plots__include_zero_y_axis', value=params[['var_plots__include_zero_y_axis']])
    }
    if (!is.null(params[['var_plots__numeric_graph_type']])) {
        log_message_variable('updating numeric_graph_type', params[['var_plots__numeric_graph_type']])
        updateSelectInput(session, 'var_plots__numeric_graph_type', selected=params[['var_plots__numeric_graph_type']])
    }
    if (!is.null(params[['var_plots__show_variable_totals']])) {
        log_message_variable('updating show_variable_totals', params[['var_plots__show_variable_totals']])
        updateCheckboxInput(session, 'var_plots__show_variable_totals', value=params[['var_plots__show_variable_totals']])
    }
    if (!is.null(params[['var_plots__show_comparison_totals']])) {
        log_message_variable('updating show_comparison_totals', params[['var_plots__show_comparison_totals']])
        updateCheckboxInput(session, 'var_plots__show_comparison_totals', value=params[['var_plots__show_comparison_totals']])
    }
    if (!is.null(params[['var_plots__histogram_bins']])) {
        log_message_variable('updating histogram_bins', params[['var_plots__histogram_bins']])
        updateNumericInput(session, 'var_plots__histogram_bins', value=params[['var_plots__histogram_bins']])
    }
    if (!is.null(params[['var_plots__transparency']])) {
        log_message_variable('updating transparency', params[['var_plots__transparency']])
        #updateSliderTextInput(session, 'var_plots__transparency', selected=params[['var_plots__transparency']])

        updateSliderTextInput(session, 'var_plots__transparency',
                                            choices=c(seq(0, 90, 10), 99),
                                            selected=params[['var_plots__transparency']])
    }
    if (!is.null(params[['var_plots__jitter']])) {
        log_message_variable('updating jitter', params[['var_plots__jitter']])
        updateCheckboxInput(session, 'var_plots__jitter', value=params[['var_plots__jitter']])
    }
    if (!is.null(params[['var_plots__numeric_aggregation_count_minimum']])) {
        log_message_variable('updating numeric_aggregation_count_minimum', params[['var_plots__numeric_aggregation_count_minimum']])
        updateNumericInput(session, 'var_plots__numeric_aggregation_count_minimum', value=params[['var_plots__numeric_aggregation_count_minimum']])
    }
    if (!is.null(params[['var_plots__numeric_show_resampled_conf_int']])) {
        log_message_variable('updating numeric_show_resampled_conf_int', params[['var_plots__numeric_show_resampled_conf_int']])
        updateCheckboxInput(session, 'var_plots__numeric_show_resampled_conf_int', value=params[['var_plots__numeric_show_resampled_conf_int']])
    }
    if (!is.null(params[['var_plots__trend_line']])) {
        log_message_variable('updating trend_line', params[['var_plots__trend_line']])
        updateRadioButtons(session, 'var_plots__trend_line', selected=params[['var_plots__trend_line']])
    }
    if (!is.null(params[['var_plots__trend_extend_date']])) {
        log_message_variable('updating trend_extend_date', params[['var_plots__trend_extend_date']])
        updateDateInput(session, inputId='var_plots__trend_extend_date', value = params[['var_plots__trend_extend_date']])
    }
    if (!is.null(params[['var_plots__trend_line_se']])) {
        log_message_variable('updating trend_line_se', params[['var_plots__trend_line_se']])
        updateRadioButtons(session, 'var_plots__trend_line_se', selected=params[['var_plots__trend_line_se']])
    }
    if (!is.null(params[['var_plots__ts_date_floor']])) {
        log_message_variable('updating ts_date_floor', params[['var_plots__ts_date_floor']])
        updateSelectInput(session, 'var_plots__ts_date_floor', selected=params[['var_plots__ts_date_floor']])
    }
    if (!is.null(params[['var_plots__ts_date_break_format']])) {
        log_message_variable('updating ts_date_break_format', params[['var_plots__ts_date_break_format']])
        updateSelectInput(session, 'var_plots__ts_date_break_format', selected=params[['var_plots__ts_date_break_format']])
    }
    if (!is.null(params[['var_plots__ts_breaks_width']])) {
        log_message_variable('updating ts_breaks_width', params[['var_plots__ts_breaks_width']])
        updateTextInput(session, 'var_plots__ts_breaks_width', value=params[['var_plots__ts_breaks_width']])
    }
    if (!is.null(params[['var_plots__scale_x_log_base_10']])) {
        log_message_variable('updating scale_x_log_base_10', params[['var_plots__scale_x_log_base_10']])
        updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=params[['var_plots__scale_x_log_base_10']])
    }
    if (!is.null(params[['var_plots__x_zoom_min']])) {
        log_message_variable('updating x_zoom_min', params[['var_plots__x_zoom_min']])
        updateNumericInput(session, 'var_plots__x_zoom_min', value=params[['var_plots__x_zoom_min']])
    }
    if (!is.null(params[['var_plots__x_zoom_max']])) {
        log_message_variable('updating x_zoom_max', params[['var_plots__x_zoom_max']])
        updateNumericInput(session, 'var_plots__x_zoom_max', value=params[['var_plots__x_zoom_max']])
    }
    if (!is.null(params[['var_plots__scale_y_log_base_10']])) {
        log_message_variable('updating scale_y_log_base_10', params[['var_plots__scale_y_log_base_10']])
        updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=params[['var_plots__scale_y_log_base_10']])
    }
    if (!is.null(params[['var_plots__y_zoom_min']])) {
        log_message_variable('updating y_zoom_min', params[['var_plots__y_zoom_min']])
        updateNumericInput(session, 'var_plots__y_zoom_min', value=params[['var_plots__y_zoom_min']])
    }
    if (!is.null(params[['var_plots__y_zoom_max']])) {
        log_message_variable('updating y_zoom_max', params[['var_plots__y_zoom_max']])
        updateNumericInput(session, 'var_plots__y_zoom_max', value=params[['var_plots__y_zoom_max']])
    }
    if (!is.null(params[['var_plots__custom_title']])) {
        log_message_variable('updating custom_title', params[['var_plots__custom_title']])
        updateTextInput(session, 'var_plots__custom_title', value=params[['var_plots__custom_title']])
    }
    if (!is.null(params[['var_plots__custom_subtitle']])) {
        log_message_variable('updating custom_subtitle', params[['var_plots__custom_subtitle']])
        updateTextInput(session, 'var_plots__custom_subtitle', value=params[['var_plots__custom_subtitle']])
    }
    if (!is.null(params[['var_plots__custom_x_axis_label']])) {
        log_message_variable('updating custom_x_axis_label', params[['var_plots__custom_x_axis_label']])
        updateTextInput(session, 'var_plots__custom_x_axis_label', value=params[['var_plots__custom_x_axis_label']])
    }
    if (!is.null(params[['var_plots__custom_y_axis_label']])) {
        log_message_variable('updating custom_y_axis_label', params[['var_plots__custom_y_axis_label']])
        updateTextInput(session, 'var_plots__custom_y_axis_label', value=params[['var_plots__custom_y_axis_label']])
    }
    if (!is.null(params[['var_plots__custom_caption']])) {
        log_message_variable('updating custom_caption', params[['var_plots__custom_caption']])
        updateTextInput(session, 'var_plots__custom_caption', value=params[['var_plots__custom_caption']])
    }
    if (!is.null(params[['var_plots__custom_tag']])) {
        log_message_variable('updating custom_tag', params[['var_plots__custom_tag']])
        updateTextInput(session, 'var_plots__custom_tag', value=params[['var_plots__custom_tag']])
    }
    if (!is.null(params[['var_plots__pretty_text']])) {
        log_message_variable('updating pretty_text', params[['var_plots__pretty_text']])
        updateCheckboxInput(session, 'var_plots__pretty_text', value=params[['var_plots__pretty_text']])
    }
    if (!is.null(params[['var_plots__base_size']])) {
        log_message_variable('updating base_size', params[['var_plots__base_size']])
        updateSliderTextInput(session,
                      'var_plots__base_size',
                      choices=seq(6, 20, 1),
                      selected=params[['var_plots__base_size']])
    }
    if (!is.null(params[['var_plots__vertical_annotations']])) {
        log_message_variable('updating vertical_annotations', params[['var_plots__vertical_annotations']])
        updateTextAreaInput(session, 'var_plots__vertical_annotations', value=params[['var_plots__vertical_annotations']])
    }
    if (!is.null(params[['var_plots__horizontal_annotations']])) {
        log_message_variable('updating horizontal_annotations', params[['var_plots__horizontal_annotations']])
        updateTextAreaInput(session, 'var_plots__horizontal_annotations', value=params[['var_plots__horizontal_annotations']])
    }
}
