should_log_message <- TRUE

select_variable <- "<Select>"
select_comparison_variable_optional <- "<Select (optional)>"

theme_base_size <- 16

##############################################################################################################
# LOGGING
##############################################################################################################
log_message <- function(message) {

    if(should_log_message) {

        cat(paste0('\n', message))
    }
}

log_message_variable <- function(variable_name, variable_value) {

    log_message(paste0(variable_name, ': `', variable_value, '`'))
}

log_message_generic <- function(message, description) {

    log_message(paste0(message, ': ', description))
}

log_message_block_start <- function(message) {

    log_message(paste0('\n\n#############################################\n',
                       message,
                       '\n#############################################\n'))
}

##############################################################################################################
# UI HELPERS
##############################################################################################################
add_trend_line <- function(plot, selected_trend_line) {

    if(selected_trend_line == 'None') {

        return (plot)

    } else {

        if(selected_trend_line == 'Straight') {
        
            return (plot + geom_smooth(method='lm'))
            
        } else if(selected_trend_line == 'Smooth') {
        
            return (plot + geom_smooth(method='loess'))
            
        } else {
            # this function isn't aware of the value which is an error   
            stopifnot(FALSE)   
        }
    }
}

prettyfy_plot <- function(plot, dataset, comparison_variable, annotate_points=FALSE) {

    # annotate_points requires a y-axis i.e. comparison_variable
    if(annotate_points && !is.null(comparison_variable) ) {

        plot <- plot + 
            geom_text(aes(label=dataset[, comparison_variable]), check_overlap=TRUE, vjust=1, hjust=1)
    }

    return (plot)
}

scale_axes_log10 <- function(plot, scale_x, scale_y) {
    
    if(scale_x) {
        
        plot <- plot + scale_x_log10(labels=comma_format())
    }
    if(scale_y) {
        
        plot <- plot + scale_y_log10(labels=comma_format())
    }
    
    return (plot)
}

##############################################################################################################
# DYNAMIC HIDE/SHOW
##############################################################################################################
hide_show_numeric_numeric <- function(session) {

    log_message('hide_show_numeric_numeric')
    
    # scatterplot

    shinyjs::show('div_variable_plots_group_scatter_controls')
    shinyjs::show('div_variable_plots_group_x_zoom_controls')
    shinyjs::show('div_variable_plots_group_y_zoom_controls')
    shinyjs::show('selected_variable_plots_base_size')
    shinyjs::show('selected_variable_plots_annotate_points')

    shinyjs::hide('selected_variable_plots_histogram_bins')
    shinyjs::hide('div_variable_plots_group_barchar_controls')
    shinyjs::hide('selected_variable_plots_numeric_graph_type')
}

hide_show_numeric_categoric <- function(session, show_y_controls) {
    
    log_message('hide_show_numeric_categoric')
    
    # could be a boxplot or a histogram; if it is a boxplot, we want to show y-axis-controls, otherwise x-axis
    if(show_y_controls) {

        shinyjs::show('div_variable_plots_group_y_zoom_controls')
        shinyjs::hide('div_variable_plots_group_x_zoom_controls')
        # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
        updateCheckboxInput(session, 'selected_variable_plots_scale_x_log_base_10', value=FALSE)
    

    } else {

        shinyjs::hide('div_variable_plots_group_y_zoom_controls')
        shinyjs::show('div_variable_plots_group_x_zoom_controls')
        # if we are hiding the y-controls, uncheck the scale_y_log10 option so it isn't carried over
        updateCheckboxInput(session, 'selected_variable_plots_scale_y_log_base_10', value=FALSE)
    }

    shinyjs::show('selected_variable_plots_base_size')
    shinyjs::show('selected_variable_plots_numeric_graph_type')

    shinyjs::hide('div_variable_plots_group_scatter_controls')
    shinyjs::hide('selected_variable_plots_histogram_bins')
    shinyjs::hide('div_variable_plots_group_barchar_controls')
    shinyjs::hide('selected_variable_plots_annotate_points')
}

hide_show_categoric_numeric <- function(session) {
    
    log_message('hide_show_categoric_numeric')
    
    # multi-boxplot

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
