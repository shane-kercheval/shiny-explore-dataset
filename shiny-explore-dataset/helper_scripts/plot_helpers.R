dplyr_friendly_variable <- function(x) {
    # if there is a column that is named in the form of "My Column", then aes_string requireds "`My Column`".
    # Wrapping a variable name such as "`my_column`" still works, so we wrap each variable in a "dplyr
    # friendly way"

    if (is.null(x)) {

        return (NULL)

    } else {

        return (paste0('`', x, '`'))
    }
}

add_trend_line <- function(plot, trend_line_type, confidence_interval=TRUE, color_variable=NULL) {
    # adds a trend line to a ggplot

    if(trend_line_type == 'None') {

        return (plot)

    } else {

        if(trend_line_type == 'Straight') {
        
            return (plot + geom_smooth(method='lm',
                                       se=confidence_interval,
                                       mapping=aes_string(color=dplyr_friendly_variable(color_variable))))
            
        } else if(trend_line_type == 'Smooth') {
        
            return (plot + geom_smooth(method='loess',
                                       se=confidence_interval,
                                       mapping=aes_string(color=dplyr_friendly_variable(color_variable))))
            
        } else {
            # this function isn't aware of the value which is an error   
            stopifnot(FALSE)   
        }
    }
}

pretyfy_annotations <- function(annotations) {
    stopifnot(is.numeric(annotations))

    if(any(annotations > 1000000)) {

        annotations <- paste0(round(annotations / 1000000, 2), 'M')

    } else if (any(annotations > 10000)) {

        annotations <- paste0(round(annotations / 1000, 1), 'K')

    } else if (any(annotations > 1000)) {

        annotations <- paste0(round(annotations / 1000, 2), 'K')
    }

    return (annotations)
}

prettyfy_plot <- function(plot, annotations=NULL) {

    if(!is.null(annotations) ) {

        plot <- plot + 
            geom_text(aes(label=annotations), check_overlap=TRUE, vjust=1, hjust=1)
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
