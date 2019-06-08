##############################################################################################################
# FILTERS
##############################################################################################################
get_dynamic_filter_selections <- function(input, columns) {

    # get all of the selections from the dynamic filters without triggering refresh for the first time
    selections_list <- purrr::map(columns, ~ isolate(input[[paste0('var_plots__dynamic_filter__', .)]]))
    names(selections_list) <- columns

    return (selections_list)

}

##############################################################################################################
# FILTERS - DYNAMIC CONTROL LIST
# builds controls based on the type of variables in the dataset
##############################################################################################################
reactive__filter_controls_list__creator <- function(input, dataset) {

    reactive({

        req(dataset())
        input$var_plots__filter_clear
        # local_filter_options_data <- filter_options_data()

        withProgress(value=1/2, message='Generating Filters',{
            
            ui_list <- imap(dataset(), ~ {

                #log_message_variable('class', class(.x)[1])

                input_id <- paste0('var_plots__dynamic_filter__', .y)
                filter_object <- NULL
                if(is_date_type(.x)) {
                    #'date'
                    min_index <- which.min(.x)
                    max_index <- which.max(.x)
                    min_value <- .x[min_index]
                    max_value <- .x[max_index]
                    
                    filter_object <- dateRangeInput(inputId=input_id,
                                                    label=.y,
                                                    start=min_value,
                                                    end=max_value)

                } else if(is.factor(.x)) {

                    if(length(levels(.x)) <= 1000) {

                        filter_object <- selectInput(inputId=input_id, label=.y, choices=levels(.x), selected = NULL, multiple = TRUE)
                    }
                } else if(is.numeric(.x)) {

                    min_value <- min(.x, na.rm = TRUE)
                    max_value <- max(.x, na.rm = TRUE)

                    filter_object <- sliderInput(inputId=input_id, label=.y, min=min_value, max=max_value, value=c(min_value, max_value))
                } else if(is.character(.x)) {
                    
                    if(length(unique(.x)) <= 1000) {

                        values_ordered_by_frequency <- as.character((as.data.frame(table(as.character(.x))) %>%
                                                                         arrange(desc(Freq)))$Var1)

                        filter_object <- selectInput(inputId=input_id,
                                    label=.y,
                                    choices=values_ordered_by_frequency,
                                    selected = NULL,
                                    multiple = TRUE)
                    }
                } else if(is.logical(.x)) {

                    filter_object <- selectInput(inputId=input_id,
                                label=.y,
                                choices=c(TRUE, FALSE),
                                selected = NULL,
                                multiple = TRUE)

                } else if("hms" %in% class(.x)) {

                    # create slider options (hours in 15 min intervals, 24:00 means <= 23:59:59)
                    hours <- str_pad(seq(0, 23), 2, side='left', pad=0)
                    minutes <- str_pad(seq(0, 45, 15), 2, side='left', pad=0)
                    hours_minutes <- sort(apply(expand.grid(hours, minutes), 1, paste, collapse=":"))
                    hours_minutes <- c(hours_minutes, "24:00")

                    filter_object <- sliderTextInput(inputId=input_id,
                                    label=.y,
                                    choices=hours_minutes,
                                    selected=c("00:00", "24:00"),
                                    grid=FALSE)

                } else {
                    #class(.)[1]
                    stopifnot(FALSE)
                }

                if(!is.null(filter_object)) {

                    filter_object <- shinyjs::hidden(filter_object)
                }

                return (filter_object)
            })
        })
        ui_list
    })
}

##############################################################################################################
# FILTER BUTTONS
# Events that control the color of the Filter collapse panel, so that it turns red when the filters haven't
# been applied (i.e. changes that haven't been applied)
##############################################################################################################
observeEvent__var_plots__filter_clear <- function(input, session) {

    observeEvent(input$var_plots__filter_clear, ({

        if(isolate(input$var_plots__filter_use)) {  # only update if we are using the filter

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'danger'))
        }
    }))
}

observeEvent__var_plots__filter_apply <- function(input, session) {

    observeEvent(input$var_plots__filter_apply, ({

        if(isolate(input$var_plots__filter_use)) {  # only update if we are using the filter

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'success'))
        }
    }))
}

observeEvent__var_plots__filter_use <- function(input, session) {

    observeEvent(input$var_plots__filter_use, {

        if(input$var_plots__filter_use) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'success'))

        } else {

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'default'))
        }
    })
}

#' The filter objects (e.g. sliderInput, selectInput, etc.) are created based on the dataset column types;
#' all of these objects are added to the `var_plots__filter_bscollapse__UI`. 
#' When someone selects the variables to filter, all of the filter objects are shown/hidden according.
#' This observeEvent does the hiding/showing 
observeEvent__var_plots__show_hide_dynamic_filters <- function(input, session, dataset) {

    observeEvent(input$var_plots__filter_controls_selections, {
        
        if(input$var_plots__filter_use) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'danger'))
        }

        dataset_columns <- colnames(dataset())
        filter_controls_selections <- input$var_plots__filter_controls_selections
        if('All Variables' %in% filter_controls_selections) {
            filter_controls_selections <- dataset_columns
        }

        unselected <- dataset_columns[!dataset_columns %in% filter_controls_selections]

        log_message_variable("Selected Filter Variables", paste(filter_controls_selections, collapse=";"))
        log_message_variable("Unselected Filter Variables", paste(unselected, collapse=";"))

        for(variable in filter_controls_selections) {

            shinyjs::show(paste0('var_plots__dynamic_filter__', variable))
        }

        for(variable in unselected) {

            shinyjs::hide(paste0('var_plots__dynamic_filter__', variable))
        }
    
    }, ignoreNULL = FALSE)  # ignoreNULL so that the observeEvent is triggered when the user removes all
                            # of the selections from the `var_plots__filter_controls_selections` inputSelect
}

observe__var_plots__bscollapse__dynamic_inputs <- function(input, session, dataset) {

    observe({

        req(dataset())

        # this is a hack to register all of the dynamic controls to the reactive event listener
        # also use it to check values (i.e. only update colors if the filters are active i.e. any are not null)
        selections <- list()
        for(column_name in colnames(dataset())) {
            value <- input[[paste0('var_plots__dynamic_filter__', column_name)]]
            selections <- append(selections, value)
        }

        # only update if we are using the filter
        # also, if any of the selections are not null, that means they have been initialized and we can begin
        # to mark as being changed otherwise, the filter section hasn't even been opened
        if(isolate(input$var_plots__filter_use) && any(map_lgl(selections, ~ !is.null(.)))) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'danger'))
        }
    })
}

##############################################################################################################
# FILTERED DATASET - Variable Plot's filtered dataset
# duplicate dataset (which is bad for large datasets) so that the filters don't have to be reapplied every time.
##############################################################################################################
reactive__var_plots__filtered_data__creator <- function(input, dataset) {

    reactive({

        local_dataset <- dataset()  # clear on new datasets

        # these are the columns we want to filter on; if the column is not in the selection, don't filter
        filter_controls_selections <- isolate(input$var_plots__filter_controls_selections)
        if('All Variables' %in% filter_controls_selections) {
            filter_controls_selections <- colnames(dataset())
        }

        if(!is.null(input$var_plots__filter_use) && input$var_plots__filter_use) {

            input$var_plots__filter_apply  # trigger for the "apply" button

            column_names <- colnames(local_dataset)
            num_columns <- length(column_names)
            withProgress(value=1 / num_columns, message='Applying Filters',{

                log_message_block_start('Filtering...')

                #### APPLY FILTERS

                # list with selections for each dynamic filter, and list names are the column names
                dynamic_filter_selections <- get_dynamic_filter_selections(input, column_names)

                index = 1
                for(column_name in column_names) {

                    incProgress(index / num_columns, detail = column_name)

                    # only filter column if the column is selected
                    if(column_name %in% filter_controls_selections) {

                        filter_selection <- dynamic_filter_selections[[column_name]]

                        if(is.null(filter_selection)) {

                            log_message_generic(column_name, 'skipping...')

                        } else {

                            symbol_column_name <- sym(column_name)
                        
                            log_message_generic(column_name,
                                                 paste('filtering -', paste0(filter_selection, collapse = '; ')))

                            if(is_date_type(local_dataset[, column_name]) ||
                                    is.numeric(local_dataset[, column_name])) {
                                #'date'
                                # for numerics/etc. need to remove NA values and then filter
                                local_dataset <- local_dataset %>%
                                    filter(!is.na(!!symbol_column_name)) %>%
                                    filter(!!symbol_column_name >= filter_selection[1] & !!symbol_column_name <= filter_selection[2])
                                
                            } else if(is.factor(local_dataset[, column_name]) ||
                                        is.character(local_dataset[, column_name])) {
                                #'factor'
                                local_dataset <- local_dataset %>%
                                    filter(!!symbol_column_name %in% filter_selection)
                            
                            } else if(is.logical(local_dataset[, column_name])) {

                                #'logical'
                                local_dataset <- local_dataset %>%
                                    filter(!!symbol_column_name %in% filter_selection)

                            } else if("hms" %in% class(local_dataset[, column_name])) {

                                # hours minutes seconds
                                local_dataset <- local_dataset %>%
                                    filter(!is.na(!!symbol_column_name)) %>%
                                    filter(!!symbol_column_name >= hm(filter_selection[1]) & !!symbol_column_name <= hm(filter_selection[2]))

                            } else {
                                #class(.)[1]
                                stopifnot(FALSE)
                            }
                        }
                    }
                    index <- index + 1
                }
                log_message('Done Filtering\n')
            })
        } else {
            log_message_block_start('Not Filtering')
        }

        return (local_dataset)
    })
}

##############################################################################################################
# CREATE GGPLOT OBJECT
##############################################################################################################
reactive__var_plots__ggplot__creator <- function(input, session, dataset) {
    reactive({

        req(input$var_plots__variable)
        req(input$var_plots__comparison)
        req(input$var_plots__filter_factor_lump_number)

        # reactive data
        local_dataset <- dataset()
        local_primary_variable <- input$var_plots__variable
        local_comparison_variable <- input$var_plots__comparison
        local_date_aggregation <- input$var_plots__date_aggregation

        local_sum_by_variable <- input$var_plots__sum_by_variable
        local_point_size <- input$var_plots__point_size
        local_color_variable <- input$var_plots__color_variable
        local_numeric_numeric_group_variable <- input$var_plots__numeric_numeric_group_variable
        local_numeric_numeric_aggregation_function <- input$var_plots__numeric_numeric_aggregation_function
        local_numeric_numeric_aggregation_count_minimum <- input$var_plots__numeric_numeric_aggregation_count_minimum
        local_numeric_numeric_show_resampled_confidence_interval <- input$var_plots__numeric_numeric_show_resampled_confidence_interval

        local_transparency <- input$var_plots__transparency / 100
        local_annotate_points <- input$var_plots__annotate_points
        local_base_size <- input$var_plots__base_size
        local_histogram_bins <- input$var_plots__histogram_bins
        local_jitter <- input$var_plots__jitter
        local_order_by_count <- input$var_plots__order_by_count
        local_numeric_graph_type <- input$var_plots__numeric_graph_type
        local_pretty_text <- input$var_plots__pretty_text
        local_scale_x_log_base_10 <- input$var_plots__scale_x_log_base_10
        local_scale_y_log_base_10 <- input$var_plots__scale_y_log_base_10
        local_show_variable_totals <- input$var_plots__show_variable_totals
        local_show_comparison_totals <- input$var_plots__show_comparison_totals
        local_stacked_comparison <- input$var_plots__stacked_comparison
        local_multi_value_delimiter <- input$var_plots__multi_value_delimiter
        local_trend_line <- input$var_plots__trend_line
        local_trend_line_se <- input$var_plots__trend_line_se
        local_x_zoom_min <- input$var_plots__x_zoom_min
        local_x_zoom_max <- input$var_plots__x_zoom_max
        local_y_zoom_min <- input$var_plots__y_zoom_min
        local_y_zoom_max <- input$var_plots__y_zoom_max

        # for time series plot
        local_show_points <- default_if_null_or_empty_string(input$var_plots__show_points, default=FALSE)
        local_ts_date_floor <- default_if_null_or_empty_string(input$var_plots__ts_date_floor, string_values_as_null='None')
        local_ts_date_break_format <- default_if_null_or_empty_string(input$var_plots__ts_date_break_format, string_values_as_null='Auto')
        local_ts_date_breaks_width <- default_if_null_or_empty_string(input$var_plots__ts_breaks_width)

        local_var_plots__filter_factor_lump_number <- input$var_plots__filter_factor_lump_number

        ggplot_object <- NULL

        if(local_primary_variable != select_variable && local_primary_variable %in% colnames(local_dataset)) {

            log_message_block_start('Creating ggplot object')
            
            # if there isn't a selection for these variables, then set them to NULL, because they will be
            # passed to rtools functions (and if they aren't null, rtools expects column names)
            local_comparison_variable <- null_if_select_variable_optional(local_comparison_variable)
            # these can actually be NULL (unlike local_comparison_variable which is req)
            # these can't be req because they aren't even shown initially
            local_sum_by_variable <- null_if_select_variable_optional(local_sum_by_variable)
            local_point_size <- null_if_select_variable_optional(local_point_size)
            local_color_variable <- null_if_select_variable_optional(local_color_variable)
            local_comparison_variable <- null_if_select_variable_optional(local_comparison_variable)

            if(is.na(local_var_plots__filter_factor_lump_number) ||
                    local_var_plots__filter_factor_lump_number == 0) {

                local_var_plots__filter_factor_lump_number <- NA
            }

            log_message_variable('primary_variable', local_primary_variable)
            log_message_variable('comparison_variable', local_comparison_variable)
            log_message_variable('var_plots__sum_by_variable', local_sum_by_variable)
            log_message_variable('var_plots__point_size', local_point_size)
            log_message_variable('var_plots__color_variable', local_color_variable)
            log_message_variable('var_plots__base_size', local_base_size)
            log_message_variable('var_plots__pretty_text', local_pretty_text)
            log_message_variable('var_plots__annotate_points', local_annotate_points)
            log_message_variable('var_plots__filter_factor_lump_number', local_var_plots__filter_factor_lump_number)
            
            if(local_pretty_text) {
                # if we change to pretty text, it will update the columns and all values to be "pretty",
                # but that means we have to take the variables they selected and change them to be
                # "pretty" as well so subsetting by them finds the correct column

                local_dataset <- rt_pretty_dataset(dataset=local_dataset)

                # R uses the "`My Variable`" syntax for variables with spaces which dplyr's xxx_() relies on
                local_primary_variable <- rt_pretty_text(local_primary_variable)
                if(!is.null(local_comparison_variable)) {

                    local_comparison_variable <- rt_pretty_text(local_comparison_variable)
                }
                if(!is.null(local_point_size)) {

                    local_point_size <- rt_pretty_text(local_point_size)
                }
                if(!is.null(local_color_variable)) {

                    local_color_variable <- rt_pretty_text(local_color_variable)
                }

                log_message_variable('updated primary_variable', local_primary_variable)
                log_message_variable('updated comparison_variable', local_comparison_variable)
                log_message_variable('updated var_plots__point_size', local_point_size)
                log_message_variable('updated var_plots__color_variable', local_color_variable)
                log_message_generic('column names', paste0(colnames(local_dataset), collapse = '; '))
            }

            if(is_date_type(local_dataset[, local_primary_variable])) {

                hide_show_date(session, has_comparison_variable=!is.null(local_comparison_variable))

                log_message_variable('var_plots__date_aggregation', local_date_aggregation)

                log_message_variable('var_plots__show_points', local_show_points)
                log_message_variable('var_plots__ts_date_floor', local_ts_date_floor)
                log_message_variable('var_plots__ts_date_break_format', local_ts_date_break_format)
                log_message_variable('var_plots__ts_breaks_width', local_ts_date_breaks_width)

                comparison_function <- NULL
                comparison_function_name <- NULL
                if(!is.null(local_comparison_variable)) {

                    comparison_function_name <- local_date_aggregation

                    if(local_date_aggregation == 'Mean') {

                        comparison_function <- function(x) { return (mean(x, na.rm=TRUE)) }

                    } else if (local_date_aggregation == 'Geometric Mean') {

                        comparison_function <- rt_geometric_mean

                    } else if (local_date_aggregation == 'Median') {

                        comparison_function <- function(x) { return (median(x, na.rm=TRUE)) }

                    } else if (local_date_aggregation == 'Sum') {

                        comparison_function_name = 'Sum of'
                        comparison_function <- function(x) { return (sum(x, na.rm=TRUE)) }

                    } else {

                        stopifnot(FALSE)
                    }
                }

                add_confidence_interval <- !is.null(local_trend_line_se) && local_trend_line_se == 'Yes'

                ggplot_object <- local_dataset %>%
                    custom_mutate(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                    rt_explore_plot_time_series(variable=local_primary_variable,
                                                comparison_variable=local_comparison_variable,
                                                comparison_function=comparison_function,
                                                comparison_function_name=comparison_function_name,
                                                color_variable=local_color_variable,
                                                y_zoom_min=local_y_zoom_min,
                                                y_zoom_max=local_y_zoom_max,
                                                show_points=local_show_points,
                                                show_labels=local_annotate_points,
                                                date_floor=local_ts_date_floor,
                                                date_break_format=local_ts_date_break_format,
                                                date_breaks_width=local_ts_date_breaks_width,
                                                base_size=local_base_size) %>%
                    scale_axes_log10(scale_x=FALSE,
                                     scale_y=local_scale_y_log_base_10) %>%
                    add_trend_line(trend_line_type=local_trend_line,
                                   confidence_interval=add_confidence_interval,
                                   color_variable=local_color_variable)

                # if(local_annotate_points && !is.null(local_comparison_variable)) {

                #     ggplot_object <- prettyfy_plot(plot=ggplot_object,
                #         annotations=pretyfy_annotations((local_dataset %>% arrange(!!sym(local_primary_variable)))[, local_comparison_variable]))
                # }
            ##############################################################################################
            # Numeric Primary Variable
            ##############################################################################################
            } else if(is.numeric(local_dataset[, local_primary_variable])) {

                ##########################################################################################
                # Numeric Secondary Variable
                ##########################################################################################
                if(!is.null(local_comparison_variable) &&
                        is.numeric(local_dataset[, local_comparison_variable])) {

                    hide_show_numeric_numeric(session, 
                        local_numeric_numeric_group_variable,
                        local_numeric_numeric_aggregation_function == "Boxplot")

                    log_message('**numeric numeric**')

                    log_message_variable('var_plots__transparency', local_transparency)
                    log_message_variable('var_plots__jitter', local_jitter)
                    log_message_variable('var_plots__trend_line', local_trend_line)
                    log_message_variable('var_plots__trend_line_se', local_trend_line_se)

                    log_message_variable('var_plots__x_zoom_min', local_x_zoom_min)
                    log_message_variable('var_plots__x_zoom_max', local_x_zoom_max)
                    log_message_variable('var_plots__y_zoom_min', local_y_zoom_min)
                    log_message_variable('var_plots__y_zoom_max', local_y_zoom_max)
                    log_message_variable('var_plots__annotate_points', local_annotate_points)
                    log_message_variable('var_plots__show_points', local_show_points)
                    log_message_variable('var_plots__scale_x_log_base_10', local_scale_x_log_base_10)
                    log_message_variable('var_plots__scale_y_log_base_10', local_scale_y_log_base_10)

                    local_map_format <- input$var_plots__map_format
                    local_map_borders_database <- input$var_plots___map_borders_database
                    local_map_borders_regions <- input$var_plots___map_borders_regions

                    log_message_variable('var_plots__map_format', local_map_format)
                    log_message_variable('var_plots__map_borders_database', local_map_borders_database)
                    log_message_variable('var_plots__map_borders_regions', local_map_borders_regions)

                    log_message_variable('var_plots__numeric_numeric_group_variable', local_numeric_numeric_group_variable)
                    log_message_variable('var_plots__numeric_numeric_aggregation_function', local_numeric_numeric_aggregation_function)
                    log_message_variable('var_plots__numeric_numeric_aggregation_count_minimum', local_numeric_numeric_aggregation_count_minimum)
                    log_message_variable('var_plots__numeric_numeric_show_resampled_confidence_interval', local_numeric_numeric_show_resampled_confidence_interval)


                    if(local_numeric_numeric_group_variable) {


                        aggregation_function <- NULL
                        aggregation_function_name <- NULL
                        if(local_numeric_numeric_aggregation_function != 'Boxplot') {

                            aggregation_function_name <- local_numeric_numeric_aggregation_function

                            if(local_numeric_numeric_aggregation_function == 'Mean') {

                                aggregation_function <- function(x) { return (mean(x, na.rm=TRUE)) }

                            } else if (local_numeric_numeric_aggregation_function == 'Geometric Mean') {

                                aggregation_function <- rt_geometric_mean

                            } else if (local_numeric_numeric_aggregation_function == 'Median') {

                                aggregation_function <- function(x) { return (median(x, na.rm=TRUE)) }

                            } else if (local_numeric_numeric_aggregation_function == 'Sum') {

                                aggregation_function_name = 'Sum of'
                                aggregation_function <- function(x) { return (sum(x, na.rm=TRUE)) }

                            } else {

                                stopifnot(FALSE)
                            }
                        }

                        ggplot_object <- local_dataset %>% 
                            custom_mutate(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                            rt_explore_plot_aggregate_2_numerics(variable=local_primary_variable,
                                                                 comparison_variable=local_comparison_variable,
                                                                 aggregation_function=aggregation_function,
                                                                 aggregation_function_name=aggregation_function_name,
                                                                 aggregation_count_minimum=local_numeric_numeric_aggregation_count_minimum,
                                                                 show_resampled_confidence_interval=local_numeric_numeric_show_resampled_confidence_interval,
                                                                 show_points=local_show_points,
                                                                 show_labels=local_annotate_points,
                                                                 x_zoom_min=local_x_zoom_min,
                                                                 x_zoom_max=local_x_zoom_max,
                                                                 y_zoom_min=local_y_zoom_min,
                                                                 y_zoom_max=local_y_zoom_max,
                                                                 base_size=local_base_size) %>%
                            scale_axes_log10(scale_x=local_scale_x_log_base_10,
                                             scale_y=local_scale_y_log_base_10)


                    } else {

                        add_confidence_interval <- !is.null(local_trend_line_se) && local_trend_line_se == 'Yes'

                        ggplot_object <- local_dataset %>% 
                            custom_mutate(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                            rt_explore_plot_scatter(variable=local_primary_variable,
                                                    comparison_variable=local_comparison_variable,
                                                    color_variable=local_color_variable,
                                                    size_variable=local_point_size,
                                                    # alpha is a measure of opacity which is the opposite of transparency, but transparency is more user-friendly
                                                    alpha= 1 - local_transparency,
                                                    jitter=local_jitter,
                                                    x_zoom_min=local_x_zoom_min,
                                                    x_zoom_max=local_x_zoom_max,
                                                    y_zoom_min=local_y_zoom_min,
                                                    y_zoom_max=local_y_zoom_max,
                                                    base_size=local_base_size) %>%
                            scale_axes_log10(scale_x=local_scale_x_log_base_10,
                                             scale_y=local_scale_y_log_base_10) %>%
                            add_trend_line(trend_line_type=local_trend_line,
                                           confidence_interval=add_confidence_interval,
                                           color_variable=local_color_variable)

                        if(local_annotate_points && !is.null(local_comparison_variable)) {

                            ggplot_object <- prettyfy_plot(plot=ggplot_object,
                                                           annotations=pretyfy_annotations(local_dataset[, local_comparison_variable]))
                        }

                        if(local_map_format) {

                            ggplot_object <- ggplot_object + coord_map()

                            if(!is_null_or_empty_string(local_map_borders_database)) {

                                regions <- str_split(local_map_borders_regions, pattern = ', ', simplify = TRUE)
                                regions <- as.character(regions)


                                ggplot_object <- ggplot_object +
                                    borders(database=local_map_borders_database,
                                            regions=regions)
                            }
                        }
                    }

                ##########################################################################################
                # NULL Or Categoric Secondary Variable
                ##########################################################################################
                } else {

                    show_boxplot <- local_numeric_graph_type == 'Boxplot'

                    hide_show_numeric_categoric(session=session, showing_boxplot=show_boxplot)

                    if(show_boxplot) {

                        log_message('**numeric null/categoric - boxplot**')

                        log_message_variable('var_plots__y_zoom_min', local_y_zoom_min)
                        log_message_variable('var_plots__y_zoom_max', local_y_zoom_max)
                        log_message_variable('var_plots__scale_y_log_base_10', local_scale_y_log_base_10)

                        ggplot_object <- local_dataset %>%
                            custom_mutate(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                            rt_explore_plot_boxplot(variable=local_primary_variable,
                                                    comparison_variable=local_comparison_variable,
                                                    y_zoom_min=local_y_zoom_min,
                                                    y_zoom_max=local_y_zoom_max,
                                                    base_size=local_base_size) %>%
                            scale_axes_log10(scale_x=FALSE,
                                             scale_y=local_scale_y_log_base_10)

                    } else {

                        log_message('**numeric null/categoric - histogram**')

                        log_message_variable('var_plots__histogram_bins', local_histogram_bins)
                        log_message_variable('var_plots__x_zoom_min', local_x_zoom_min)
                        log_message_variable('var_plots__x_zoom_max', local_x_zoom_max)
                        log_message_variable('var_plots__scale_x_log_base_10', local_scale_x_log_base_10)
                        
                        ggplot_object <- local_dataset %>%
                            custom_mutate(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                            rt_explore_plot_histogram(variable=local_primary_variable,
                                                      comparison_variable=local_comparison_variable,
                                                      num_bins=local_histogram_bins,
                                                      x_zoom_min=local_x_zoom_min,
                                                      x_zoom_max=local_x_zoom_max,
                                                      base_size=local_base_size) %>%
                            scale_axes_log10(scale_x=local_scale_x_log_base_10,
                                             scale_y=FALSE)
                    }
                }

            ##############################################################################################
            # Categoric Primary Variable
            ##############################################################################################
            } else {

                ##########################################################################################
                # Numeric Secondary Variable
                ##########################################################################################
                if(!is.null(local_comparison_variable) &&
                        is.numeric(local_dataset[, local_comparison_variable])) {

                    hide_show_categoric_numeric(session)

                    log_message('**categoric numeric**')

                    log_message_variable('var_plots__y_zoom_min', local_y_zoom_min)
                    log_message_variable('var_plots__y_zoom_max', local_y_zoom_max)
                    log_message_variable('var_plots__scale_y_log_base_10', local_scale_y_log_base_10)

                    ggplot_object <- local_dataset %>%
                        custom_mutate(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                        rt_explore_plot_boxplot(variable=local_comparison_variable,
                                                comparison_variable=local_primary_variable,
                                                y_zoom_min=local_y_zoom_min,
                                                y_zoom_max=local_y_zoom_max,
                                                base_size=local_base_size) %>%
                        scale_axes_log10(scale_x=FALSE,
                                         scale_y=local_scale_y_log_base_10)
                ##########################################################################################
                # NULL Or Categoric Secondary Variable
                ##########################################################################################
                } else {
                    
                    if(local_multi_value_delimiter == '' || !is.null(local_comparison_variable)) {

                        local_multi_value_delimiter <- NULL
                    }

                    hide_show_categoric_categoric(session,
                                                  input,
                                                  has_comparison_variable=!is.null(local_comparison_variable))

                    log_message('**categoric null/categoric**')

                    log_message_variable('var_plots__order_by_count', local_order_by_count)
                    log_message_variable('var_plots__show_variable_totals', local_show_variable_totals)
                    log_message_variable('var_plots__show_comparison_totals', local_show_comparison_totals)
                    log_message_variable('var_plots__multi_value_delimiter', local_multi_value_delimiter)

                    ggplot_object <- local_dataset %>%
                        custom_mutate(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                        rt_explore_plot_value_totals(variable=local_primary_variable,
                                                     comparison_variable=local_comparison_variable,
                                                     sum_by_variable=local_sum_by_variable,
                                                     order_by_count=local_order_by_count,
                                                     show_variable_totals=local_show_variable_totals,
                                                     show_comparison_totals=local_show_comparison_totals,
                                                     stacked_comparison=local_stacked_comparison,
                                                     multi_value_delimiter=local_multi_value_delimiter,
                                                     base_size=local_base_size)
                }
            }
        }

        return (ggplot_object)
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
renderUI__var_plots__variable__UI <- function(dataset) {

    renderUI({
        selectInput(inputId='var_plots__variable',
                    label = 'Variable',
                    choices = c(select_variable, colnames(dataset())),
                    selected = select_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__var_plots__comparison__UI <- function(input, dataset) {

    renderUI({

        # if we have a date type as the primary variable, the comparison should only be numeric

        req(input$var_plots__variable)

        selected_variable <- default_if_null_or_empty_string(input$var_plots__comparison, select_variable_optional)

        local_dataset <- dataset()
        local_primary_variable <- input$var_plots__variable

        dataset_columns <- colnames(local_dataset)

        variable_options <- NULL
        # only show numeric variables for dates
        if(local_primary_variable != select_variable &&
                local_primary_variable %in% dataset_columns &&  # in case datasets change
                is_date_type(local_dataset[, local_primary_variable])) {

            variable_options <- colnames(local_dataset %>% select_if(is.numeric))

        } else {

            variable_options <- dataset_columns
        }

        selectInput(inputId='var_plots__comparison',
                    label = 'Comparison Variable',
                    choices = c(select_variable_optional, variable_options),
                    selected = selected_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__var_plots__sum_by_variable__UI <- function(dataset) {

    renderUI({

        selectInput(inputId='var_plots__sum_by_variable',
                    label = 'Sum By Variable',
                    choices = c(select_variable_optional, colnames(dataset() %>% select_if(is.numeric))),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__var_plots__color_variable__UI <- function(input, dataset) {

    renderUI({
        # if we have a date type as the primary variable, color should only be non-numeric

        req(input$var_plots__variable)

        local_dataset <- dataset()
        local_primary_variable <- input$var_plots__variable

        dataset_columns <- colnames(local_dataset)

        variable_options <- NULL
        # only show numeric variables for dates
        if(local_primary_variable != select_variable &&
                local_primary_variable %in% dataset_columns &&  # in case datasets change
                is_date_type(local_dataset[, local_primary_variable])) {

            variable_options <- colnames(local_dataset %>% select_if(purrr::negate(is.numeric)))

        } else {

            variable_options <- dataset_columns
        }

        selectInput(inputId='var_plots__color_variable',
                    label = 'Color Variable',
                    choices = c(select_variable_optional, variable_options),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__var_plots__point_size__UI <- function(dataset) {

    renderUI({

        selectInput(inputId='var_plots__point_size',
                    label = 'Size Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__var_plots__filter_controls_selections__UI <- function(input, dataset) {
    renderUI({

        input$var_plots__filter_clear

        selectInput(inputId='var_plots__filter_controls_selections',
                    label = 'Filters',
                    choices = c('All Variables', colnames(dataset())),
                    selected = NULL,
                    multiple = TRUE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__var_plots__filter_bscollapse__UI <- function(input, dataset, filter_controls_list) {
 
    renderUI({
        tagList(list=filter_controls_list())

        # filter_controls_selections <- input$var_plots__filter_controls_selections
        # if('All Variables' %in% filter_controls_selections) {
        #     filter_controls_selections <- colnames(dataset())
        # }
        # tagList(list=filter_controls_list()[filter_controls_selections])
        
        # control_list <- list()
        # for(selection in filter_controls_selections) {
        #     if(is.null(isolate(input[[paste0('var_plots__dynamic_filter__', selection)]]))) {

        #         control_list <- append(control_list, filter_controls_list()[selection])

        #     } else {

        #         control_list <- append(control_list, isolate(output[[paste0('var_plots__dynamic_filter__', selection)]]))
        #     }
        # }
        #tagList(list=control_list)
    })
}

##############################################################################################################
# DYNAMICALLY SHOW/HIDE INPUT
##############################################################################################################
hide_show_date <- function(session, has_comparison_variable) {

    log_message('hide_show_date')
    
    shinyjs::show('div_var_plots__group_y_zoom_controls')
    shinyjs::show('var_plots__base_size')
    shinyjs::show('var_plots__annotate_points')
    shinyjs::show('var_plots__show_points')
    shinyjs::show('var_plots__color_variable__UI')
    shinyjs::show('div_var_plots__group_trend_controls')
    shinyjs::show('div_var_plots__group_time_series_controls')

    if(has_comparison_variable) {

        shinyjs::show('var_plots__date_aggregation')

    } else {

        shinyjs::hide('var_plots__date_aggregation')
    }

    shinyjs::hide('var_plots__point_size__UI')
    shinyjs::hide('var_plots__numeric_numeric_group_variable')
    shinyjs::hide('var_plots__numeric_numeric_aggregation_function')
    shinyjs::hide('var_plots__numeric_numeric_aggregation_count_minimum')
    shinyjs::hide('var_plots__numeric_numeric_show_resampled_confidence_interval')
    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('div_var_plots__group_x_zoom_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('div_var_plots__multi_barchar_controls')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__sum_by_variable__UI')
    shinyjs::hide('var_plots__multi_value_delimiter')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    shinyjs::hide('var_plots__map_format')
    shinyjs::hide('var_plots___map_borders_database')
    shinyjs::hide('var_plots___map_borders_regions')
}

hide_show_numeric_numeric <- function(session, is_grouping_main_variable, grouping_is_boxplot) {

    log_message('hide_show_numeric_numeric')
    
    # scatterplot; or if grouping the main variable, then boxplot or custom aggregation_function

    shinyjs::hide('var_plots__date_aggregation')
    shinyjs::show('var_plots__numeric_numeric_group_variable')

    if(is_grouping_main_variable) {

        shinyjs::show('var_plots__numeric_numeric_aggregation_function')
        shinyjs::show('var_plots__numeric_numeric_aggregation_count_minimum')

        if(grouping_is_boxplot) {
            
            shinyjs::hide('var_plots__show_points')
            shinyjs::hide('var_plots__annotate_points')
            shinyjs::hide('var_plots__numeric_numeric_show_resampled_confidence_interval')

        } else {

            shinyjs::show('var_plots__show_points')
            shinyjs::show('var_plots__annotate_points')
            shinyjs::show('var_plots__numeric_numeric_show_resampled_confidence_interval')
        }
    
        shinyjs::hide('var_plots__point_size__UI')
        shinyjs::hide('var_plots__color_variable__UI')

        shinyjs::hide('var_plots__map_format')
        shinyjs::hide('var_plots___map_borders_database')
        shinyjs::hide('var_plots___map_borders_regions')
        updateCollapse(session, 'var_plots__bscollapse', close="Map Options")

        shinyjs::hide('div_var_plots__group_scatter_controls')
        shinyjs::hide('div_var_plots__group_trend_controls')
        

    } else {

        shinyjs::hide('var_plots__numeric_numeric_aggregation_function')
        shinyjs::hide('var_plots__numeric_numeric_aggregation_count_minimum')

        shinyjs::show('var_plots__point_size__UI')
        shinyjs::show('var_plots__color_variable__UI')

        shinyjs::show('var_plots__map_format')
        shinyjs::show('var_plots___map_borders_database')
        shinyjs::show('var_plots___map_borders_regions')
        updateCollapse(session, 'var_plots__bscollapse', open="Map Options")

        shinyjs::show('div_var_plots__group_scatter_controls')
        shinyjs::show('div_var_plots__group_trend_controls')
        shinyjs::show('var_plots__annotate_points')
        shinyjs::hide('var_plots__show_points')
    }

    shinyjs::show('div_var_plots__group_x_zoom_controls')
    shinyjs::show('div_var_plots__group_y_zoom_controls')
    shinyjs::show('var_plots__base_size')
    
    shinyjs::hide('div_var_plots__group_time_series_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('div_var_plots__multi_barchar_controls')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__sum_by_variable__UI')
    shinyjs::hide('var_plots__multi_value_delimiter')
    #shinyjs::hide('var_plots__filter_factor_lump_number')

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

    shinyjs::hide('var_plots__date_aggregation')
    shinyjs::hide('var_plots__point_size__UI')
    shinyjs::hide('var_plots__numeric_numeric_group_variable')
    shinyjs::hide('var_plots__numeric_numeric_aggregation_function')
    shinyjs::hide('var_plots__numeric_numeric_aggregation_count_minimum')
    shinyjs::hide('var_plots__numeric_numeric_show_resampled_confidence_interval')
    shinyjs::hide('var_plots__color_variable__UI')

    shinyjs::show('var_plots__base_size')
    shinyjs::show('var_plots__numeric_graph_type')

    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('div_var_plots__group_trend_controls')
    shinyjs::hide('div_var_plots__group_time_series_controls')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('div_var_plots__multi_barchar_controls')
    shinyjs::hide('var_plots__annotate_points')
    shinyjs::hide('var_plots__show_points')
    shinyjs::hide('var_plots__sum_by_variable__UI')
    shinyjs::hide('var_plots__multi_value_delimiter')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    shinyjs::hide('var_plots__map_format')
    shinyjs::hide('var_plots___map_borders_database')
    shinyjs::hide('var_plots___map_borders_regions')

}

hide_show_categoric_numeric <- function(session) {
    
    log_message('hide_show_categoric_numeric')
    
    # multi-boxplot
    shinyjs::hide('var_plots__point_size__UI')
    shinyjs::hide('var_plots__numeric_numeric_group_variable')
    shinyjs::hide('var_plots__numeric_numeric_aggregation_function')
    shinyjs::hide('var_plots__numeric_numeric_aggregation_count_minimum')
    shinyjs::hide('var_plots__numeric_numeric_show_resampled_confidence_interval')
    shinyjs::hide('var_plots__color_variable__UI')

    shinyjs::show('div_var_plots__group_y_zoom_controls')
    shinyjs::show('var_plots__base_size')

    shinyjs::hide('div_var_plots__group_x_zoom_controls')
    # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
    updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)

    shinyjs::hide('var_plots__date_aggregation')
    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('div_var_plots__group_trend_controls')
    shinyjs::hide('div_var_plots__group_time_series_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('div_var_plots__multi_barchar_controls')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__annotate_points')
    shinyjs::hide('var_plots__show_points')
    shinyjs::hide('var_plots__sum_by_variable__UI')
    shinyjs::hide('var_plots__multi_value_delimiter')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    shinyjs::hide('var_plots__map_format')
    shinyjs::hide('var_plots___map_borders_database')
    shinyjs::hide('var_plots___map_borders_regions')

}

hide_show_categoric_categoric <- function(session, input, has_comparison_variable) {

    log_message('hide_show_categoric_categoric')
    
    # grouped barchart
    shinyjs::show('var_plots__sum_by_variable__UI') # categoric with categoric (or NULL) can select numeric sum_by_variable

    if(is.null(input$var_plots__comparison) || input$var_plots__comparison == select_variable_optional) {

        shinyjs::show('var_plots__multi_value_delimiter')

    } else {

        shinyjs::hide('var_plots__multi_value_delimiter')
    }

    shinyjs::hide('var_plots__point_size__UI')
    shinyjs::hide('var_plots__numeric_numeric_group_variable')
    shinyjs::hide('var_plots__numeric_numeric_aggregation_function')
    shinyjs::hide('var_plots__numeric_numeric_aggregation_count_minimum')
    shinyjs::hide('var_plots__numeric_numeric_show_resampled_confidence_interval')
    shinyjs::hide('var_plots__color_variable__UI')

    shinyjs::show('div_var_plots__group_barchar_controls')
    if(has_comparison_variable) {

        shinyjs::show('div_var_plots__multi_barchar_controls')

    } else {

        shinyjs::hide('div_var_plots__multi_barchar_controls')
    }

    shinyjs::show('var_plots__base_size')

    shinyjs::hide('var_plots__date_aggregation')
    shinyjs::hide('div_var_plots__group_x_zoom_controls')
    shinyjs::hide('div_var_plots__group_y_zoom_controls')
    # if we are hiding the x/y-controls, uncheck the scale_x/y_log10 option so it isn't carried over
    updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)
    updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=FALSE)

    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('div_var_plots__group_trend_controls')
    shinyjs::hide('div_var_plots__group_time_series_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__annotate_points')
    shinyjs::hide('var_plots__show_points')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    shinyjs::hide('var_plots__map_format')
    shinyjs::hide('var_plots___map_borders_database')
    shinyjs::hide('var_plots___map_borders_regions')

}

observe__var_plots__hide_show_uncollapse_on_primary_vars <- function(input, session) {
    observe({

        req(input$var_plots__variable)
        req(input$var_plots__comparison)

        local_primary_variable <- input$var_plots__variable
        local_comparison_variable <- input$var_plots__comparison

        if(local_primary_variable == select_variable || local_comparison_variable == select_variable_optional) {

            shinyjs::hide('var_plots__date_aggregation')
            shinyjs::hide('var_plots__sum_by_variable__UI')
            shinyjs::hide('var_plots__multi_value_delimiter')
            shinyjs::hide('var_plots__point_size__UI')
            shinyjs::hide('var_plots__numeric_numeric_group_variable')
            shinyjs::hide('var_plots__numeric_numeric_aggregation_function')
            shinyjs::hide('var_plots__numeric_numeric_aggregation_count_minimum')
            shinyjs::hide('var_plots__numeric_numeric_show_resampled_confidence_interval')
            shinyjs::hide('var_plots__color_variable__UI')
        }

        if(local_primary_variable != select_variable || local_comparison_variable != select_variable_optional) {

            updateCollapse(session, 'var_plots__bscollapse', open='Plot Options')
        }
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
renderPlot__variable_plot <- function(session, ggplot_object, messages) {

    renderPlot({

        withProgress(value=1/2, message='Plotting Graph',{

           messages$value <- capture_messages_warnings(function() print(ggplot_object()))
           log_message_variable('messages$value', messages$value)

        })

    }, height = function() {

        session$clientData$output_var_plots_width * 0.66  # set height to % of width
    })
}

renderPrint__reactiveValues__vp__ggplot_message <- function(message) {

    renderPrint({
        cat(message$value)
    })
}
