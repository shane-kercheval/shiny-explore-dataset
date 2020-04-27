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

        } else if(trend_line_type == 'Projection') {

            return (plot + geom_smooth(method='lm',
                                       se=confidence_interval,
                                       fullrange=TRUE,
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

scale_axes_log10 <- function(plot, scale_x, scale_y) {
    
    if(scale_x) {
        
        plot <- plot + scale_x_log10(labels=comma_format())
    }
    if(scale_y) {
        
        plot <- plot + scale_y_log10(labels=comma_format())
    }
    
    return (plot)
}

get_colors_from_word_df <- function(text_dataset) {
                
    text_dataset <- text_dataset %>%
        arrange(word) %>%
        separate(word, c('word_single', 'word_mis'), sep = '___', remove = FALSE)
    
    custom_colors <- rep(rt_colors(), 20)
    sorted_values <- sort(as.character(unique(text_dataset$word_single)))
    
    stopifnot(length(custom_colors) >= length(sorted_values))
    
    
    custom_colors <- custom_colors[1:length(sorted_values)]
    names(custom_colors) <- sorted_values
    
    return (map_chr(text_dataset$word_single, ~ custom_colors[.]))
}
