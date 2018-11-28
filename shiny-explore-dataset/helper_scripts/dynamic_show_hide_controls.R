hide_show_numeric_numeric <- function(session) {

    log_message('hide_show_numeric_numeric')
    
    # scatterplot

    shinyjs::show('selected_variable_plot_point_size_UI')
    shinyjs::show('selected_variable_plot_point_color_UI')

    shinyjs::show('div_variable_plots_group_scatter_controls')
    shinyjs::show('div_variable_plots_group_x_zoom_controls')
    shinyjs::show('div_variable_plots_group_y_zoom_controls')
    shinyjs::show('selected_variable_plots_base_size')
    shinyjs::show('selected_variable_plots_annotate_points')

    shinyjs::hide('selected_variable_plots_histogram_bins')
    shinyjs::hide('div_variable_plots_group_barchar_controls')
    shinyjs::hide('selected_variable_plots_numeric_graph_type')
}

hide_show_numeric_categoric <- function(session, showing_boxplot) {
    
    log_message('hide_show_numeric_categoric')
    
    # could be a boxplot or a histogram; if it is a boxplot, we want to show y-axis-controls, otherwise x-axis
    if(showing_boxplot) {

        shinyjs::hide('selected_variable_plots_histogram_bins')
        shinyjs::show('div_variable_plots_group_y_zoom_controls')
        shinyjs::hide('div_variable_plots_group_x_zoom_controls')
        # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
        updateCheckboxInput(session, 'selected_variable_plots_scale_x_log_base_10', value=FALSE)
    

    } else {

        shinyjs::show('selected_variable_plots_histogram_bins')
        shinyjs::hide('div_variable_plots_group_y_zoom_controls')
        shinyjs::show('div_variable_plots_group_x_zoom_controls')
        # if we are hiding the y-controls, uncheck the scale_y_log10 option so it isn't carried over
        updateCheckboxInput(session, 'selected_variable_plots_scale_y_log_base_10', value=FALSE)
    }

    shinyjs::hide('selected_variable_plot_point_size_UI')
    shinyjs::hide('selected_variable_plot_point_color_UI')

    shinyjs::show('selected_variable_plots_base_size')
    shinyjs::show('selected_variable_plots_numeric_graph_type')

    shinyjs::hide('div_variable_plots_group_scatter_controls')
    shinyjs::hide('div_variable_plots_group_barchar_controls')
    shinyjs::hide('selected_variable_plots_annotate_points')
}

hide_show_categoric_numeric <- function(session) {
    
    log_message('hide_show_categoric_numeric')
    
    # multi-boxplot
    shinyjs::hide('selected_variable_plot_point_size_UI')
    shinyjs::hide('selected_variable_plot_point_color_UI')

    shinyjs::show('div_variable_plots_group_y_zoom_controls')
    shinyjs::show('selected_variable_plots_base_size')

    shinyjs::hide('div_variable_plots_group_x_zoom_controls')
    # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
    updateCheckboxInput(session, 'selected_variable_plots_scale_x_log_base_10', value=FALSE)

    shinyjs::hide('div_variable_plots_group_scatter_controls')
    shinyjs::hide('selected_variable_plots_histogram_bins')
    shinyjs::hide('div_variable_plots_group_barchar_controls')
    shinyjs::hide('selected_variable_plots_numeric_graph_type')
    shinyjs::hide('selected_variable_plots_annotate_points')
}

hide_show_categoric_categoric <- function(session) {

    log_message('hide_show_categoric_categoric')
    
    # grouped barchart

    shinyjs::hide('selected_variable_plot_point_size_UI')
    shinyjs::hide('selected_variable_plot_point_color_UI')

    shinyjs::show('div_variable_plots_group_barchar_controls')
    shinyjs::show('selected_variable_plots_base_size')

    shinyjs::hide('div_variable_plots_group_x_zoom_controls')
    shinyjs::hide('div_variable_plots_group_y_zoom_controls')
    # if we are hiding the x/y-controls, uncheck the scale_x/y_log10 option so it isn't carried over
    updateCheckboxInput(session, 'selected_variable_plots_scale_x_log_base_10', value=FALSE)
    updateCheckboxInput(session, 'selected_variable_plots_scale_y_log_base_10', value=FALSE)
    

    shinyjs::hide('div_variable_plots_group_scatter_controls')
    shinyjs::hide('selected_variable_plots_histogram_bins')
    shinyjs::hide('selected_variable_plots_numeric_graph_type')
    shinyjs::hide('selected_variable_plots_annotate_points')
}

observe__variable_plot__hide_show_uncollapse_on_primary_vars <- function(input, output, session) {
    observe({

        req(input$selected_variable_plot_variable)
        req(input$selected_variable_plot_comparison)

        local_primary_variable <- input$selected_variable_plot_variable
        local_comparison_variable <- input$selected_variable_plot_comparison

        if(local_primary_variable == select_variable || local_comparison_variable == select_variable_optional) {

            shinyjs::hide('selected_variable_plot_point_size_UI')
            shinyjs::hide('selected_variable_plot_point_color_UI')
        }

        if(local_primary_variable != select_variable || local_comparison_variable != select_variable_optional) {

            updateCollapse(session, 'collapse_variable_plot_controls', open='Plot Options')
        }
    })
}
