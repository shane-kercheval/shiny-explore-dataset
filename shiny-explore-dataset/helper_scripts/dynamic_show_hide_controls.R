hide_show_numeric_numeric <- function(session) {

    log_message('hide_show_numeric_numeric')
    
    # scatterplot

    shinyjs::show('variable_plots_point_size_UI')
    shinyjs::show('variable_plots_point_color_UI')

    shinyjs::show('div_variable_plots_group_scatter_controls')
    shinyjs::show('div_variable_plots_group_x_zoom_controls')
    shinyjs::show('div_variable_plots_group_y_zoom_controls')
    shinyjs::show('variable_plots_base_size')
    shinyjs::show('variable_plots_annotate_points')

    shinyjs::hide('variable_plots_histogram_bins')
    shinyjs::hide('div_variable_plots_group_barchar_controls')
    shinyjs::hide('variable_plots_numeric_graph_type')
    shinyjs::hide('variable_plots_sum_by_variable_UI')
}

hide_show_numeric_categoric <- function(session, showing_boxplot) {
    
    log_message('hide_show_numeric_categoric')
    
    # could be a boxplot or a histogram; if it is a boxplot, we want to show y-axis-controls, otherwise x-axis
    if(showing_boxplot) {

        shinyjs::hide('variable_plots_histogram_bins')
        shinyjs::show('div_variable_plots_group_y_zoom_controls')
        shinyjs::hide('div_variable_plots_group_x_zoom_controls')
        # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
        updateCheckboxInput(session, 'variable_plots_scale_x_log_base_10', value=FALSE)
    

    } else {

        shinyjs::show('variable_plots_histogram_bins')
        shinyjs::hide('div_variable_plots_group_y_zoom_controls')
        shinyjs::show('div_variable_plots_group_x_zoom_controls')
        # if we are hiding the y-controls, uncheck the scale_y_log10 option so it isn't carried over
        updateCheckboxInput(session, 'variable_plots_scale_y_log_base_10', value=FALSE)
    }

    shinyjs::hide('variable_plots_point_size_UI')
    shinyjs::hide('variable_plots_point_color_UI')

    shinyjs::show('variable_plots_base_size')
    shinyjs::show('variable_plots_numeric_graph_type')

    shinyjs::hide('div_variable_plots_group_scatter_controls')
    shinyjs::hide('div_variable_plots_group_barchar_controls')
    shinyjs::hide('variable_plots_annotate_points')
    shinyjs::hide('variable_plots_sum_by_variable_UI')
}

hide_show_categoric_numeric <- function(session) {
    
    log_message('hide_show_categoric_numeric')
    
    # multi-boxplot
    shinyjs::hide('variable_plots_point_size_UI')
    shinyjs::hide('variable_plots_point_color_UI')

    shinyjs::show('div_variable_plots_group_y_zoom_controls')
    shinyjs::show('variable_plots_base_size')

    shinyjs::hide('div_variable_plots_group_x_zoom_controls')
    # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
    updateCheckboxInput(session, 'variable_plots_scale_x_log_base_10', value=FALSE)

    shinyjs::hide('div_variable_plots_group_scatter_controls')
    shinyjs::hide('variable_plots_histogram_bins')
    shinyjs::hide('div_variable_plots_group_barchar_controls')
    shinyjs::hide('variable_plots_numeric_graph_type')
    shinyjs::hide('variable_plots_annotate_points')
    shinyjs::hide('variable_plots_sum_by_variable_UI')
}

hide_show_categoric_categoric <- function(session) {

    log_message('hide_show_categoric_categoric')
    
    # grouped barchart
    shinyjs::show('variable_plots_sum_by_variable_UI') # categoric with categoric (or NULL) can select numeric sum_by_variable
    shinyjs::hide('variable_plots_point_size_UI')
    shinyjs::hide('variable_plots_point_color_UI')

    shinyjs::show('div_variable_plots_group_barchar_controls')
    shinyjs::show('variable_plots_base_size')

    shinyjs::hide('div_variable_plots_group_x_zoom_controls')
    shinyjs::hide('div_variable_plots_group_y_zoom_controls')
    # if we are hiding the x/y-controls, uncheck the scale_x/y_log10 option so it isn't carried over
    updateCheckboxInput(session, 'variable_plots_scale_x_log_base_10', value=FALSE)
    updateCheckboxInput(session, 'variable_plots_scale_y_log_base_10', value=FALSE)

    shinyjs::hide('div_variable_plots_group_scatter_controls')
    shinyjs::hide('variable_plots_histogram_bins')
    shinyjs::hide('variable_plots_numeric_graph_type')
    shinyjs::hide('variable_plots_annotate_points')
}

observe__variable_plots__hide_show_uncollapse_on_primary_vars <- function(input, output, session) {
    observe({

        req(input$variable_plots_variable)
        req(input$variable_plots_comparison)

        local_primary_variable <- input$variable_plots_variable
        local_comparison_variable <- input$variable_plots_comparison

        if(local_primary_variable == select_variable || local_comparison_variable == select_variable_optional) {

            shinyjs::hide('variable_plots_sum_by_variable_UI')
            shinyjs::hide('variable_plots_point_size_UI')
            shinyjs::hide('variable_plots_point_color_UI')
        }

        if(local_primary_variable != select_variable || local_comparison_variable != select_variable_optional) {

            updateCollapse(session, 'variable_plots_bscollapse', open='Plot Options')
        }
    })
}
