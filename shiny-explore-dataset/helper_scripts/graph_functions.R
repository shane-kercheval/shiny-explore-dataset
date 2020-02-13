library(dplyr)
library(ggplot2)
library(rtools)

#' returns a time-series plot of change from previous period (defined by date-floor)
#'
#' @param dataset dataframe
#' @param date_variable a variable (x-axis) that is a date type
#' @param date_floor converts dates to date_floor value and aggregates; options are e.g. "week", "month", "quarter"
#' @param aggregation_variable the additional numeric variable (y-axis)
#' @param aggregation_function if a comparison variable is supplied, a function must be given so the plot knows how to graph it (e.g. sum, mean, median)
#' @param aggregation_function_name name of the function so that it can be plotted on the Y-axis label
#' @param color_variable an optional variable (categoric) that seperates the time series
#' @param facet_variable an optional variable (categoric) that seperates the time series by facet
#' @param percent_change if TRUE then shows the percent change from X-1 period to X period; if FALSE, shows absolute value change
#' @param show_labels if TRUE adds labels to each point
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
rt_explore_plot_time_series_change <- function(dataset,
                                               date_variable,
                                               date_floor,
                                               aggregation_variable=NULL,
                                               aggregation_function=NULL,
                                               aggregation_function_name=NULL,
                                               color_variable=NULL,
                                               facet_variable=NULL,
                                               percent_change=FALSE,
                                               show_labels=FALSE,
                                               base_size=11) {

    # if using a aggregation variable, we must also have a function and function name
    stopifnot(!(!is.null(aggregation_variable) &&
        (is.null(aggregation_function) || is.null(aggregation_function_name))))

    dataset <- private__plot_time_series_change__floor_date(dataset, date_variable, date_floor)

    change_gain_loss_total <- suppressWarnings(private_create_gain_loss_total(dataset,
                                                                              date_variable,
                                                                              date_floor,
                                                                              facet_variable,
                                                                              percent_change,
                                                                              aggregation_variable,
                                                                              aggregation_function))

    change_gain_loss_by_group <- suppressWarnings(private_create_gain_loss_total_by_group(dataset,
                                                                          date_variable,
                                                                          date_floor,
                                                                          color_variable,
                                                                          facet_variable,
                                                                          percent_change,
                                                                          aggregation_variable,
                                                                          aggregation_function))

    if(is.null(color_variable)) {

        custom_colors <- NULL

    } else {

        custom_colors <- rt_get_colors_from_values(dataset[[color_variable]])
    }

    aggregation_label <- ""
    if(!is.null(aggregation_variable)) {
        aggregation_label <- paste0("of ", aggregation_function_name, " `", aggregation_variable, "`")
    }

    if(percent_change) {

        label_legend <- "Total Percent Change"
        label_y <- paste("Percent Change", aggregation_label, "from Previous Period")
        column_y <- 'percent_change'
        symbol_y <- sym(column_y)

    } else {

        label_legend <- "Total Gain/Loss"
        label_y <- paste("Gain/Loss", aggregation_label, "from Previous Period")
        column_y <- 'gain_loss'
        symbol_y <- sym(column_y)
    }
    label_x <- paste0(date_variable, " (", date_floor,")")

    # COLOR | FACET
    # ======|======
    # NULL  | NULL
    # NULL  | NOT
    # NOT   | NULL
    # NOT   | NOT
    if(is.null(color_variable) && is.null(facet_variable)) {

        ggplot_object <- change_gain_loss_total %>%
            ggplot(aes(x=period_label, y=!!symbol_y, fill=ifelse(!!symbol_y < 0, TRUE, FALSE))) +
            geom_bar(stat='identity') +
            geom_hline(yintercept = 0, color='black', size=0.3) +
            scale_fill_manual(values=c(rt_colors_good_bad()[1], rt_colors_good_bad()[2]), na.value = '#2A3132') +
            theme_light(base_size = base_size) +
            theme(legend.position = 'none',
                  axis.text.x = element_text(angle = 30, hjust = 1)) +
            labs(y=label_y,
                 x=label_x,
                 title=paste0(label_y, " (", date_variable,")"))


    } else if (is.null(color_variable) && !is.null(facet_variable)) {

        ggplot_object <- change_gain_loss_by_group %>%
            ggplot(aes(x=period_label, y=!!symbol_y, fill=ifelse(!!symbol_y < 0, TRUE, FALSE))) +
            geom_bar(stat='identity') +
            geom_hline(yintercept = 0, color='black', size=0.5) +
            scale_fill_manual(values=c(rt_colors_good_bad()[1], rt_colors_good_bad()[2]), na.value = '#2A3132') +
            theme_light(base_size = base_size) +
            theme(legend.position = 'none',
                  axis.text.x = element_text(angle = 30, hjust = 1)) +
            labs(y=label_y,
                 x=label_x,
                 title=paste0(label_y, " (", date_variable,")"),
                 subtitle = paste0("by `", facet_variable, "`"))+
            facet_wrap(facets = paste0("`",facet_variable, "`") , ncol = 1, scales = 'free_y', strip.position = "right")

    } else if (!is.null(color_variable) && is.null(facet_variable)) {

        ggplot_object <- change_gain_loss_by_group %>%
            ggplot(aes(x=period_label, y=!!symbol_y)) +
            geom_bar(aes(fill=!!sym(color_variable)), stat='identity', position = position_dodge(width=0.9)) +
            scale_fill_manual(values=rt_get_colors_from_values(change_gain_loss_by_group[[color_variable]]),
                              na.value = '#2A3132') +
            # hack to show legend without boxplot symbol
            geom_point(data=change_gain_loss_total, aes(x=period_label, y=!!symbol_y, shape=label_legend), size=-1) +
            scale_shape_manual(name = "", values = 95) +
            guides(fill=guide_legend(order=2),
                   shape = guide_legend(order=1,
                                        override.aes = list(shape = 95,
                                                            size = 10,
                                                            color='red'))) +
            
            geom_boxplot(data=change_gain_loss_total, aes(x=period_label, y=!!symbol_y),
                         show.legend = FALSE,
                         color='red') +
            theme_light(base_size = base_size) +
            theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
            labs(y=label_y,
                 x=label_x,
                 title=paste0(label_y, " (", date_variable,")"),
                 subtitle = paste0("by `", color_variable, "`"))

    } else {

        ggplot_object <- change_gain_loss_by_group %>%
            ggplot(aes(x=period_label, y=!!symbol_y)) +
            geom_bar(aes(fill=!!sym(color_variable)), stat='identity', position = position_dodge(width=0.9)) +
            geom_hline(yintercept = 0, color='black', size=0.2) +
            scale_fill_manual(values=rt_get_colors_from_values(change_gain_loss_by_group[[color_variable]]),
                              na.value = '#2A3132') +
            # hack to show legend without boxplot symbol
            geom_point(data=change_gain_loss_total,
                       aes(x=period_label, y=!!symbol_y, shape=label_legend), size=0) +
            scale_shape_manual(name = "", values = 95) +
            guides(fill=guide_legend(order=2),
                   shape = guide_legend(order=1,
                                        override.aes = list(shape = 95,
                                                            size = 10,
                                                            color='red'))) +

            geom_boxplot(data=change_gain_loss_total, aes(x=period_label, y=!!symbol_y),
                         show.legend = FALSE,
                         color='red') +
            theme_light(base_size = base_size) +
            theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
            labs(y=label_y,
                 x=label_x,
                 title=paste0(label_y, " (", date_variable,")"),
                 subtitle = paste0("by `", color_variable, "` & `", facet_variable,"`")) +
            facet_wrap(facets = paste0("`",facet_variable, "`") , ncol = 1, scales = 'free_y', strip.position = "right")
    }

    if(is.null(facet_variable)) {

        num_pretty_breaks <- 10

    } else {

        num_pretty_breaks <- 5
    }

    if(percent_change) {

        format_function_axis <- rt_pretty_percent
        format_function_text <- rt_pretty_percent

    } else {

        format_function_axis <- rt_pretty_numbers_short
        format_function_text <- rt_pretty_numbers_long

    }

    ggplot_object <- ggplot_object +
        scale_y_continuous(breaks=pretty_breaks(num_pretty_breaks), labels = format_function_axis)

    if(show_labels) {

        # vjsut for main graph
        # Color | FACET
        # 0 | 0 - outside of bars
        # 1 | 0 - middle of bars
        # 0 | 1 - inside of bars
        # 1 | 1 - middle of bars
        if(is.null(color_variable) && is.null(facet_variable)) {

            # outside of bars
            vjust_high <- -0.25
            vjust_low <- 1.1

        } else if (!is.null(color_variable)) {
        
            # middle of bars
            vjust_high <- 0.5
            vjust_low <- 0.3
        
        } else {
            # color is null & facet is not null, inside
            vjust_high <- 1.25
            vjust_low <- -0.5
        }

        # add per bar if color variable is supplied
        if(!is.null(color_variable)) {

            ggplot_object <- suppressWarnings(ggplot_object +
                geom_text(aes(label = ifelse(!!symbol_y >= 0, format_function_text(!!symbol_y), " "),
                              fill=!!sym(color_variable)),
                          position = position_dodge(width=0.9),
                          check_overlap = TRUE,
                          vjust=1.25))

            ggplot_object <- suppressWarnings(ggplot_object +
                geom_text(aes(label = ifelse(!!symbol_y < 0, format_function_text(!!symbol_y), " "),
                              fill=!!sym(color_variable)),
                          position = position_dodge(width=0.9),
                          check_overlap = TRUE,
                          vjust=-0.5))
        }

        ggplot_object <- suppressWarnings(ggplot_object +
            geom_text(data=change_gain_loss_total,
                      aes(label = ifelse(!!symbol_y >= 0, format_function_text(!!symbol_y), " ")),
                      check_overlap = TRUE,
                      vjust=vjust_high))

        ggplot_object <- suppressWarnings(ggplot_object +
            geom_text(data=change_gain_loss_total,
                      aes(label = ifelse(!!symbol_y < 0, format_function_text(!!symbol_y), " ")),
                      check_overlap = TRUE,
                      vjust=vjust_low))
    }

    return (ggplot_object)
}

#' returns date break format based on date floor
#'
#' @param datefloor datefloor
private__time_series_date_break_format_2 <- function(date_floor) {

    if(str_detect(date_floor, 'week')) {

        date_break_format <- '%Y-%m-%d'

    } else if (str_detect(date_floor, 'month')) {

        date_break_format <- '%Y-%m'

    } else if (str_detect(date_floor, 'quarter')) {

        date_break_format <- '%Y-%m'

    } else if (str_detect(date_floor, 'year')) {

        date_break_format <- '%Y'

    } else {

        date_break_format <- '%Y-%m-%d'
    }

    return (date_break_format)
}

#' @param aggregated_dataset is the dataset after it is aggregated by period
private__fill_missing_periods <- function(aggregated_dataset,
                                          date_floor,
                                          date_variable,
                                          color_variable=NULL,
                                          facet_variable=NULL) {
    
    null_if_blank <- function(x) {
        
        if(!is.null(x) && x == '') {
            x <- NULL
        }
        
        return (x)
    }
    
    color_variable <- null_if_blank(color_variable)
    facet_variable <- null_if_blank(facet_variable)
    
    helper_convert_to_floor <- function(values) {
        date_floor_values <- private__plot_time_series_change__floor_date(dataset=data.frame(dates=values),
                                                                          date_variable='dates',
                                                                          date_floor=date_floor) %>% 
            pull(dates)
        
        return(date_floor_values)
    }
    
    helper_get_unique_values <- function(dataset, variable_name) {
        if(is.null(variable_name)) {
            return ('')
        } else {
            return (unique(dataset[[variable_name]]))
        }
    }
    
    
    floor_date_values <- helper_convert_to_floor(aggregated_dataset[[date_variable]])

    max_period <- max(floor_date_values, na.rm = TRUE)
    min_period <- min(floor_date_values, na.rm = TRUE)
    
    # get a list of all possible dates so that we can fill in any gaps
    expected_dates <- helper_convert_to_floor(seq(min_period, max_period, by='days')) %>% unique()
    expected_color_variables <- helper_get_unique_values(aggregated_dataset, color_variable)
    expected_facet_variables <- helper_get_unique_values(aggregated_dataset, facet_variable)
    # if color_variable and/or facet_variable is NULL, those columns will have all '' values
    expected_date_facet_values <- expand_grid(date_variable=expected_dates,
                                              color_variable=expected_color_variables,
                                              facet_variable=expected_facet_variables)
    # get columns that aren't all blank
    not_all_blank <- function(values) {!is.character(values) || !all(values == '')}
    expected_date_facet_values <- expected_date_facet_values %>% select_if(not_all_blank)
    # rename column names appropriately; this will ignore any NULL variables
    colnames(expected_date_facet_values) <- c(date_variable, color_variable, facet_variable)
    
    # get the missing combinations, if any
    missing_dates_df <- dplyr::setdiff(expected_date_facet_values, aggregated_dataset[, c(date_variable, color_variable, facet_variable)])
    
    if(nrow(missing_dates_df) > 0) {
        
        missing_dates_df$n <- 0
        aggregated_dataset <- bind_rows(# ensure same order
                                        aggregated_dataset[, c(date_variable, color_variable, facet_variable, 'n')],
                                        missing_dates_df)
    }
    
    return (aggregated_dataset)
}

#' returns a dataframe summarizing the gain/loss per period per facet
#'
#' @param dataset dataframe
#' @param date_variable a variable (x-axis) that is a date type
#' @param date_floor converts dates to date_floor value and aggregates; options are e.g. "week", "month", "quarter"
#' @param facet_variable an optional variable (categoric) that seperates the time series by facet
#' @param percent_change if TRUE then shows the percent change from X-1 period to X period; if FALSE, shows absolute value change
#' @param aggregation_variable the additional numeric variable (y-axis)
#' @param aggregation_function if a comparison variable is supplied, a function must be given so the plot knows how to graph it (e.g. sum, mean, median)
#'
private_create_gain_loss_total <- function(dataset,
                                           date_variable,
                                           date_floor,
                                           facet_variable,
                                           percent_change,
                                           aggregation_variable,
                                           aggregation_function) {

    change_gain_loss_total <- dataset %>% filter(!is.na(!!sym(date_variable)))

    if(is.null(aggregation_variable)) {

        # if no aggregation_variable, we are just counting records
        # either have to aggregate by variable, or variable and/or color/facet

        if(is.null(facet_variable)) {

            change_gain_loss_total <- change_gain_loss_total %>% count(!!sym(date_variable))


        } else {

            change_gain_loss_total <- change_gain_loss_total %>%
                count(!!sym(date_variable), !!sym(facet_variable))
        }

    } else {

        if(is.null(facet_variable)) {

            change_gain_loss_total <- change_gain_loss_total %>%
                group_by(!!sym(date_variable)) %>%
                summarise(n=aggregation_function(!!sym(aggregation_variable)))

        } else {

            change_gain_loss_total <- change_gain_loss_total %>%
                group_by(!!sym(date_variable), !!sym(facet_variable)) %>%
                summarise(n=aggregation_function(!!sym(aggregation_variable)))
        }
    }
    
    change_gain_loss_total <- change_gain_loss_total %>%
        private__fill_missing_periods(date_floor=date_floor,
                                      date_variable=date_variable,
                                      facet_variable=facet_variable) %>%
        arrange(!!sym(date_variable))

    date_break_format <- private__time_series_date_break_format_2(date_floor=date_floor)

    change_gain_loss_total[[date_variable]] <- private__custom_date_format(date_floor,
        date_break_format)(floor_date(x=change_gain_loss_total[[date_variable]],
                                      unit=date_floor, week_start=1))

    if(is.null(facet_variable)) {

        change_gain_loss_total <- change_gain_loss_total %>%
            mutate(previous_period = dplyr::lag(!!sym(date_variable)),
                   period_label = paste(previous_period, '->', !!sym(date_variable)),
                   previous_n = dplyr::lag(n),
                   gain_loss = n - previous_n,
                   percent_change = (n - previous_n) / previous_n)

    } else {

        change_gain_loss_total <- change_gain_loss_total %>%
            group_by(!!sym(facet_variable)) %>%
            mutate(previous_period = dplyr::lag(!!sym(date_variable)),
                   period_label = paste(previous_period, '->', !!sym(date_variable)),
                   previous_n = dplyr::lag(n),
                   gain_loss = n - previous_n,
                   percent_change = (n - previous_n) / previous_n) %>%
            ungroup()

        if(percent_change) {
            column_y <- 'percent_change'
        } else {
            column_y <- 'gain_loss'
        }

        reorder_val <- abs(private__replace_na_with_0(change_gain_loss_total[[column_y]]))
        change_gain_loss_total[[facet_variable]] <- fct_reorder(.f=change_gain_loss_total[[facet_variable]],
                                                                .x=reorder_val,
                                                                #!!symbol_y,
                                                                .fun=sum,
                                                                .desc = TRUE)
    }

    change_gain_loss_total <- change_gain_loss_total %>% filter(!is.na(previous_period))

    return (change_gain_loss_total)
}

private__replace_na_with_0 <- function(x) {

    return (ifelse(is.na(x) | is.nan(x) | is.infinite(x), 0, x))
}

private__plot_time_series_change__floor_date <- function(dataset, date_variable, date_floor) {

    dataset <- dataset %>% filter(!is.na(!!sym(date_variable)))
    if(is.null(date_floor)) {

        dataset[[date_variable]] <- as.Date(dataset[[date_variable]])

    } else {

        dataset[[date_variable]] <- as.Date(floor_date(dataset[[date_variable]], unit=date_floor, week_start = 1))
    }

    return(dataset)
}

#' returns a dataframe summarizing the gain/loss per period per facet
#'
#' @param dataset dataframe
#' @param date_variable a variable (x-axis) that is a date type
#' @param date_floor converts dates to date_floor value and aggregates; options are e.g. "week", "month", "quarter"
#' @param color_variable an optional variable (categoric) that separates the time series
#' @param facet_variable an optional variable (categoric) that seperates the time series by facet
#' @param percent_change if TRUE then shows the percent change from X-1 period to X period; if FALSE, shows absolute value change
#' @param aggregation_variable the additional numeric variable (y-axis)
#' @param aggregation_function if a comparison variable is supplied, a function must be given so the plot knows how to graph it (e.g. sum, mean, median)
#'
private_create_gain_loss_total_by_group <- function(dataset,
                                                    date_variable,
                                                    date_floor,
                                                    color_variable,
                                                    facet_variable,
                                                    percent_change,
                                                    aggregation_variable,
                                                    aggregation_function) {

    if(is.null(color_variable) && is.null(facet_variable)) {
        return (NULL)
    }

    if(is.null(color_variable)) {

        color_variable <- ''
    }

    if(is.null(facet_variable)) {

        facet_variable <- ''
    }

    change_gain_loss_by_group <- dataset %>% filter(!is.na(!!sym(date_variable)))

    if(is.null(aggregation_variable)) {
        # if no aggregation_variable, we are just counting records
        # either have to aggregate by variable, or variable and/or color/facet

        change_gain_loss_by_group <- change_gain_loss_by_group %>%
            count(!!sym(date_variable), !!sym(color_variable), !!sym(facet_variable))

    } else {

        change_gain_loss_by_group <- change_gain_loss_by_group %>%
            group_by(!!sym(date_variable), !!sym(color_variable), !!sym(facet_variable)) %>%
            summarise(n=aggregation_function(!!sym(aggregation_variable)))
    }

    change_gain_loss_by_group <- change_gain_loss_by_group %>%
        private__fill_missing_periods(date_floor=date_floor,
                                      date_variable=date_variable,
                                      color_variable=color_variable,
                                      facet_variable=facet_variable) %>%
        arrange(!!sym(date_variable))

    date_break_format <- private__time_series_date_break_format_2(date_floor=date_floor)

    change_gain_loss_by_group[[date_variable]] <- private__custom_date_format(date_floor,
                                                                              date_break_format)(floor_date(x=change_gain_loss_by_group[[date_variable]],
                                                                                                            unit=date_floor, week_start=1))

    change_gain_loss_by_group <- change_gain_loss_by_group %>%
        group_by(!!sym(color_variable), !!sym(facet_variable)) %>%
        mutate(previous_period = dplyr::lag(!!sym(date_variable)),
               period_label = paste(previous_period, '->', !!sym(date_variable)),
               previous_n = dplyr::lag(n),
               gain_loss = n - previous_n,
               percent_change = (n - previous_n) / previous_n) %>%
        ungroup() %>%
        filter(!is.na(previous_period))

    if(!is.null(facet_variable) && facet_variable != '') {

        if(percent_change) {
            column_y <- 'percent_change'
        } else {
            column_y <- 'gain_loss'
        }

        reorder_val <- abs(private__replace_na_with_0(change_gain_loss_by_group[[column_y]]))
        change_gain_loss_by_group[[facet_variable]] <- fct_reorder(.f=change_gain_loss_by_group[[facet_variable]],
                                                                   .x=reorder_val,
                                                                   #!!symbol_y,
                                                                   .fun=sum,
                                                                   .desc = TRUE)
    }

    return (change_gain_loss_by_group)
}

private__custom_date_format <- function(date_floor, date_break_format) {

    if(is.null(date_floor) || date_floor != 'quarter') {

        return (date_format(date_break_format))

    } else {  # not NULL && quarter

        return (rt_as_year_qtr_format)
    }
}
