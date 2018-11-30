hide_show_numeric_numeric <- function(session) {

    log_message('hide_show_numeric_numeric')
    
    # scatterplot

    shinyjs::show('var_plots__point_size_UI')
    shinyjs::show('var_plots__point_color_UI')

    shinyjs::show('div_var_plots__group_scatter_controls')
    shinyjs::show('div_var_plots__group_x_zoom_controls')
    shinyjs::show('div_var_plots__group_y_zoom_controls')
    shinyjs::show('var_plots__base_size')
    shinyjs::show('var_plots__annotate_points')

    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__sum_by_variable_UI')
}

hide_show_numeric_categoric <- function(session, showing_boxplot) {
    
    log_message('hide_show_numeric_categoric')
    
    # could be a boxplot or a histogram; if it is a boxplot, we want to show y-axis-controls, otherwise x-axis
    if(showing_boxplot) {

        shinyjs::hide('var_plots__histogram_bins')
        shinyjs::show('div_var_plots__group_y_zoom_controls')
        shinyjs::hide('div_var_plots__group_x_zoom_controls')
        # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
        updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)
    

    } else {

        shinyjs::show('var_plots__histogram_bins')
        shinyjs::hide('div_var_plots__group_y_zoom_controls')
        shinyjs::show('div_var_plots__group_x_zoom_controls')
        # if we are hiding the y-controls, uncheck the scale_y_log10 option so it isn't carried over
        updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=FALSE)
    }

    shinyjs::hide('var_plots__point_size_UI')
    shinyjs::hide('var_plots__point_color_UI')

    shinyjs::show('var_plots__base_size')
    shinyjs::show('var_plots__numeric_graph_type')

    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('var_plots__annotate_points')
    shinyjs::hide('var_plots__sum_by_variable_UI')
}

hide_show_categoric_numeric <- function(session) {
    
    log_message('hide_show_categoric_numeric')
    
    # multi-boxplot
    shinyjs::hide('var_plots__point_size_UI')
    shinyjs::hide('var_plots__point_color_UI')

    shinyjs::show('div_var_plots__group_y_zoom_controls')
    shinyjs::show('var_plots__base_size')

    shinyjs::hide('div_var_plots__group_x_zoom_controls')
    # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
    updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)

    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__annotate_points')
    shinyjs::hide('var_plots__sum_by_variable_UI')
}

hide_show_categoric_categoric <- function(session) {

    log_message('hide_show_categoric_categoric')
    
    # grouped barchart
    shinyjs::show('var_plots__sum_by_variable_UI') # categoric with categoric (or NULL) can select numeric sum_by_variable
    shinyjs::hide('var_plots__point_size_UI')
    shinyjs::hide('var_plots__point_color_UI')

    shinyjs::show('div_var_plots__group_barchar_controls')
    shinyjs::show('var_plots__base_size')

    shinyjs::hide('div_var_plots__group_x_zoom_controls')
    shinyjs::hide('div_var_plots__group_y_zoom_controls')
    # if we are hiding the x/y-controls, uncheck the scale_x/y_log10 option so it isn't carried over
    updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)
    updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=FALSE)

    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__annotate_points')
}

observe__var_plots__hide_show_uncollapse_on_primary_vars <- function(input, output, session) {
    observe({

        req(input$var_plots__variable)
        req(input$var_plots__comparison)

        local_primary_variable <- input$var_plots__variable
        local_comparison_variable <- input$var_plots__comparison

        if(local_primary_variable == select_variable || local_comparison_variable == select_variable_optional) {

            shinyjs::hide('var_plots__sum_by_variable_UI')
            shinyjs::hide('var_plots__point_size_UI')
            shinyjs::hide('var_plots__point_color_UI')
        }

        if(local_primary_variable != select_variable || local_comparison_variable != select_variable_optional) {

            updateCollapse(session, 'var_plots__bscollapse', open='Plot Options')
        }
    })
}
