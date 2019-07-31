library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('helper_scripts/definitions.R')

shinyUI(fluidPage(theme="custom.css",

    useShinyjs(),
    navbarPage(id='navbar_page_app',
               title = 'Explore Dataset',
        tabPanel(
            'Load Dataset',
            tabsetPanel(type='tabs',
                tabPanel(
                    'Preloaded Dataset',
                    tags$br(),
                    #uiOutput('preloaded_dataset')
                    tags$div(style='margin-bottom: 40px !important;',
                        selectInput(inputId='preloaded_dataset',
                                    label='Choose a Dataset:',
                                    choices=c('Credit',
                                              'Diamonds',
                                              'Housing',
                                              'Insurance',
                                              'Iris',
                                              'Flights',
                                              'Wine Ratings',
                                              'Gapminder'),
                                    selected=NULL))
                ),
                tabPanel(
                    'Load .csv/.RDS',
                    tags$br(),
                    fileInput(inputId='uploadFile', 'Choose a File:')
                ),
                tabPanel(
                    'Load .csv from URL',
                    tags$br(),
                    textInput(inputId='load_data__url_csv', label="URL", value=NULL, width=600),
                    actionButton(inputId='load_data__url_csv_button', label='Load Data')
                )
            ),
            tags$br(),
            tabsetPanel(type='tabs',
                tabPanel(
                    "First 500 Records of Dataset",
                    tags$div(class='results-table', dataTableOutput(outputId='source_data__head_table'))
                ),
                tabPanel(
                    'Dataset Description',
                    tags$br(),
                    verbatimTextOutput(outputId='load_data__description')
                ),
                tabPanel(
                    'Variable Types',
                    tags$div(class='results-table', dataTableOutput(outputId='source_data__types_table'))
                ),
                tabPanel(
                    'Update Dataset w/ R Code',
                    textAreaInput(inputId='load_data__r_code_text', label="", value = "", width = 800, height = 300, cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
                    tags$div(class='error_output',
                        shinyjs::hidden(
                            verbatimTextOutput(outputId='load_data__r_code_error')
                        )
                    ),
                    bsTooltip(id='load_data__r_code_text',
                                  title="Write R code to manipulate the currently loaded dataset. The loaded dataset should be referred to explicitly as `dataset` in the code.",
                                  placement='top', trigger='hover'),
                    actionButton(inputId='load_data__r_code_apply', label='Run Code')
                )
            )
        ),
        tabPanel(
            'Graphs',
            column(3,
                class='column-input-control-style',

                bsCollapse(id='var_plots__bscollapse', open='Variables', multiple=TRUE,
                    bsCollapsePanel(
                        'Variables',
                        selectInput(inputId='var_plots__variable', label='Variable', choices=global__select_variable, selected=global__select_variable, width='100%'),
                        selectInput(inputId='var_plots__comparison', label='Comparison Variable', choices=global__select_variable_optional, selected=global__select_variable_optional, width='100%'),
                        checkboxInput(inputId='var_plots__numeric_group_comp_variable',
                                      label='Group Comparison Variable', value=FALSE, width='100%'),
                        bsTooltip(id='var_plots__numeric_group_comp_variable',
                                  title="Treats the `Comparison Variable` as discrete values, and aggregates the `Variable` based on the discrete groups of the `Comparision Variable`.",
                                  placement='top', trigger='hover'),
                        selectInput(inputId='var_plots__numeric_aggregation_function',
                                    label='Aggregation',
                                    choices=c('Boxplot', 'Mean', 'Geometric Mean', 'Median', 'Sum'),
                                    selected=global__num_num_aggregation_function_default,
                                    multiple=FALSE,
                                    selectize=TRUE,
                                    width=500),
                        bsTooltip(id='var_plots__numeric_aggregation_function',
                                  title="Determines how the `Variable` is aggregated, after grouping the `Comparison Variable`.",
                                  placement='top',
                                  trigger='hover'),
                        # NOTE: the variables below can't be hidden initially (but rather are dynamically hidden)
                        # because the values need to load the first time this tab is clicked
                        # if not, when they become active (and therefore the values change to the default,
                        # they will trigger an unncessary plot refresh
                        selectInput(inputId='var_plots__numeric_aggregation',
                                    label='Aggregation',
                                    choices=c('Mean', 'Geometric Mean', 'Median', 'Sum'),
                                    selected=global__var_plots__numeric_aggregation_default,
                                    multiple=FALSE,
                                    selectize=TRUE,
                                    width=500),
                        shinyjs::hidden(selectInput(inputId='var_plots__sum_by_variable', label='Sum By Variable', choices=global__select_variable_optional, selected=global__select_variable_optional, width='100%')),
                        shinyjs::hidden(selectInput(inputId='var_plots__color_variable', label='Color Variable', choices=global__select_variable_optional, selected=global__select_variable_optional, width='100%')),
                        shinyjs::hidden(selectInput(inputId='var_plots__facet_variable', label='Facet Variable', choices=global__select_variable_optional, selected=global__select_variable_optional, width='100%')),
                        shinyjs::hidden(selectInput(inputId='var_plots__size_variable', label='Size Variable', choices=global__select_variable_optional, selected=global__select_variable_optional, width='100%')),
                        # uiOutput('var_plots__sum_by_variable__UI'),
                        # uiOutput('var_plots__color_variable__UI'),
                        # uiOutput('var_plots__facet_variable__UI'),
                        # uiOutput('var_plots__size_variable__UI'),
                        textInput('var_plots__multi_value_delimiter',
                                  width=150,
                                  label="Multi-Value Delimiter"),
                        bsTooltip(id='var_plots__multi_value_delimiter',
                                  title="For variables that have multi-value instances seperated by a delimiter (e.g. value_x;value_y), this feature seperates the multi-value instance into multiple instances and counts them individually.",
                                  placement='top', trigger='hover'),
                        fluidRow(
                            div(style="display:inline-block; float:left; margin-bottom:0px; margin-top:10px; margin-left: 15px",
                                actionButton(inputId='var_plots__variables_buttons_clear', label='Clear')),
                            div(style="display:inline-block; float:left; margin-bottom:0px; margin-top:10px; margin-left: 10px",
                                actionButton(inputId='var_plots__variables_buttons_swap', label='Swap')),
                            bsTooltip(id='var_plots__variables_buttons_clear',
                                      title="Clear all of the variables selected.",
                                      placement='bottom', trigger='hover'),
                            bsTooltip(id='var_plots__variables_buttons_swap',
                                      title="Swap the Primary and Comparison Variables.",
                                      placement='bottom', trigger='hover')
                        ),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Filters',
                        fluidRow(
                            column(4,
                                checkboxInput(inputId='var_plots__filter_use',
                                              label='Use Filters', value=FALSE, width='100%'),
                                bsTooltip(id='var_plots__filter_use',
                                          title="Warning! Data that has NA/missing values will be removed.",
                                          placement='top', trigger='hover')
                            ),
                            column(8,
                                tags$div(style='margin-bottom: 10px;',
                                         actionButton(inputId='var_plots__filter_apply',
                                                      label='Apply Filters')),
                                bsTooltip(id='var_plots__filter_apply',
                                    title="Filter the data based on the selected variables and values.",
                                    placement='top', trigger='hover'),
                                tags$div(style='margin-bottom: 20px;',
                                         actionButton(inputId='var_plots__filter_clear',
                                                      label='Clear Filters')),
                                bsTooltip(id='var_plots__filter_clear',
                                          title="Remove all selected variables and reset to default values.",
                                          placement='bottom', trigger='hover')
                            )
                        ),
                        uiOutput('var_plots__filter_controls_selections__UI'),
                        uiOutput('var_plots__filter_bscollapse__UI')
                    ),
                    bsCollapsePanel(
                        'Graph Options',
                        fluidRow(
                            div(style="display:inline-block; float:left; margin-bottom:20px; margin-left: 15px",
                                actionButton(inputId='var_plots__graph_options_apply',
                                             label='Apply Options')),
                            div(style="display:inline-block; float:left; margin-bottom:20px; margin-left: 10px",
                                actionButton(inputId='var_plots__graph_options_clear',
                                             label='Clear'))
                        ),
                        selectInput(inputId='var_plots__numeric_graph_type',
                                    label='Type',
                                    choices=c('Boxplot', 'Histogram'),
                                    selected='Boxplot',
                                    width=500),
                        numericInput(inputId='var_plots__histogram_bins',
                                      label='Number of Bins',
                                      value=30),
                        selectInput(inputId='var_plots__categoric_view_type',
                                    label='View Type',
                                    choices=c("Bar", "Confidence"),
                                    selected="Bar",
                                    multiple=FALSE,
                                    selectize=TRUE,
                                    width='100%'),
                        sliderTextInput(inputId='var_plots__filter_factor_lump_number',
                                        label="Top N Categories",
                                        choices=as.character(c("Off", seq(1, 10), seq(15, 50, 5))),
                                        selected="10",
                                        grid=TRUE,
                                        width=500),
                        bsTooltip(id='var_plots__filter_factor_lump_number',
                                  title="Only show the top N categories. Groups all other categories into `Other` category.",
                                  placement='top', trigger='hover'),
                        selectInput(inputId='var_plots__label_variables',
                                    label = 'Label Variables',
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = TRUE,
                                    selectize = TRUE,
                                    width='100%',
                                    size = NULL),
                        numericInput(inputId='var_plots__numeric_aggregation_count_minimum',
                                     label='Minimum number of samples in group:',
                                     width='100%',
                                     value=30),
                        bsTooltip(id='var_plots__numeric_aggregation_count_minimum',
                                  title="The minimum number of samples/instances required in order to include the group in the graph.",
                                  placement='top', trigger='hover'),
                        checkboxInput(inputId='var_plots__annotate_points',
                                     label='Show Values', value=TRUE, width='100%'),
                        checkboxInput(inputId='var_plots__show_points',
                                      label='Show Points', value=TRUE, width='100%'),
                        bsTooltip(id='var_plots__show_points',
                                  title="Show the value associated with each point.",
                                  placement='top', trigger='hover'),
                        checkboxInput(inputId='var_plots__year_over_year',
                                      label='Year-Over-Year', value=FALSE, width='100%'),
                        checkboxInput(inputId='var_plots__numeric_show_resampled_conf_int',
                                      label='Show Confidence Interval (Resampling):',
                                      value=FALSE, width='100%'),
                        selectInput(inputId='var_plots__order_by_variable',
                                    label = "Order By",
                                    choices = c("Default", "Frequency"),
                                    selected = "Default",
                                    width='100%'),
                        tags$div(id='div_var_plots__group_barchar_controls',
                            bsTooltip(id='div_var_plots__group_barchar_controls',
                                      title="Orders by total number of records. Otherwise, orders by character or factor-level ordering.",
                                      placement='top', trigger='hover'),
                            checkboxInput(inputId='var_plots__show_variable_totals',
                                          label='Show Variable Values', value=TRUE, width='100%'),
                            checkboxInput(inputId='var_plots__show_comparison_totals',
                                          label='Show Comparison Values', value=TRUE, width='100%')
                        ),
                        tags$div(id='div_var_plots__group_scatter_controls',
                            sliderTextInput(inputId='var_plots__transparency',
                                            label='Transparency',
                                            choices=c(seq(0, 90, 10), 99),
                                            selected=60,
                                            post  = " %",
                                            width='100%',
                                            grid=TRUE),
                            checkboxInput(inputId='var_plots__jitter',
                                          label='Jitter', value=FALSE, width='100%'),
                            bsTooltip(id='var_plots__jitter',
                                      title="Adds a small amount of random variation to the location of each point, and is a useful way of handling overplotting caused by discreteness in datasets.",
                                      placement='top', trigger='hover')
                        ),
                        tags$div(id='div_var_plots__group_trend_controls',
                            radioButtons(inputId='var_plots__trend_line',
                                         label='Trend Line:',
                                         choices=c('None', 'Straight', 'Smooth'),
                                         selected='None',
                                         inline=TRUE,
                                         width='100%'),
                            radioButtons(inputId='var_plots__trend_line_se',
                                         label='Trend Confidence Interval:',
                                         choices=c('No', 'Yes'),
                                         selected='Yes',
                                         inline=TRUE,
                                         width='100%')
                        ),
                        tags$div(id='div_var_plots__group_time_series_controls',
                            # time series variables
                            selectInput(inputId='var_plots__ts_date_floor',
                                        label='Date Aggregation:',
                                        choices=global__date_part_vector,
                                        selected=names(global__date_part_vector)[1],
                                        width='100%'),
                            selectInput(inputId='var_plots__ts_date_break_format',
                                        label='Date Format:',
                                        choices=global__date_break_format_vector,
                                        selected=names(global__date_break_format_vector)[1],
                                        width='100%'),
                            textInput(inputId='var_plots__ts_breaks_width',
                                      label="Date Breaks",
                                      value = NULL,
                                      width='100%')
                        ),
                        tags$div(id='div_var_plots__group_x_zoom_controls',
                            checkboxInput(inputId='var_plots__scale_x_log_base_10',
                                          label='Scale X-Axis Log 10', value=FALSE, width='100%'),
                            bsTooltip(id='var_plots__scale_x_log_base_10',
                                      title="Adds a Log transformation to the values associated with the x-axis.",
                                      placement='top', trigger='hover'),
                            div(style="display:inline-block",     
                            
                                numericInput(inputId='var_plots__x_zoom_min',
                                             label='X-Axis Min',
                                             value=NULL,
                                             width=100)
                            ),
                            div(style="display:inline-block; margin-left: 10px",     
                            
                                numericInput(inputId='var_plots__x_zoom_max',
                                             label='X-Axis Max',
                                             value=NULL,
                                             width=100)
                            ),
                            bsTooltip(id='var_plots__x_zoom_min',
                                      title='"Zoom" into the graph, using this value as the minimum x-axis coordinate',
                                      placement='top', trigger='hover'),
                            bsTooltip(id='var_plots__x_zoom_max',
                                      title='"Zoom" into the graph, using this value as the maximum x-axis coordinate',
                                      placement='top', trigger='hover')
                        ),
                        checkboxInput(inputId='var_plots__include_zero_y_axis',
                                          label='Include 0 in Y-Axis', value=TRUE, width='100%'),
                        bsTooltip(id='var_plots__include_zero_y_axis',
                                      title="If selected, expand the lower bound of the y-axis to incldue 0 (which is the best practice).",
                                      placement='top', trigger='hover'),
                        tags$div(id='div_var_plots__group_y_zoom_controls',

                            checkboxInput(inputId='var_plots__scale_y_log_base_10',
                                          label='Scale Y-Axis Log 10', value=FALSE, width='100%'),
                            bsTooltip(id='var_plots__scale_y_log_base_10',
                                      title="Adds a Log transformation to the values associated with the y-axis.",
                                      placement='top', trigger='hover'),
                            div(style="display:inline-block",
                                numericInput(inputId='var_plots__y_zoom_min',
                                             label='Y-Axis Min',
                                             value=NULL,
                                             width=100)
                            ),
                            div(style="display:inline-block; margin-left: 10px",     
                            
                                numericInput(inputId='var_plots__y_zoom_max',
                                             label='Y-Axis Max',
                                             value=NULL,
                                             width=100)
                            ),
                            bsTooltip(id='var_plots__y_zoom_min',
                                      title='"Zoom" into the graph, using this value as the minimum y-axis coordinate',
                                      placement='bottom', trigger='hover'),
                            bsTooltip(id='var_plots__y_zoom_max',
                                      title='"Zoom" into the graph, using this value as the maximum y-axis coordinate',
                                      placement='bottom', trigger='hover')
                        ),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Other Options',
                        
                        fluidRow(
                            div(style="display:inline-block; float:left; margin-bottom:20px; margin-left: 15px",
                                actionButton(inputId='var_plots__custom_labels_apply', label='Apply Labels')),
                            div(style="display:inline-block; float:left; margin-bottom:20px; margin-left: 10px",
                                actionButton(inputId='var_plots__custom_labels_clear', label='Clear'))
                        ),
                        textInput(inputId='var_plots__custom_title',
                                  label="Title", width='100%', value=NULL),
                        textInput(inputId='var_plots__custom_subtitle',
                                  label="Subtitle", width='100%', value=NULL),
                        textInput(inputId='var_plots__custom_x_axis_label',
                                  label="X-Axis Label", width='100%', value=NULL),
                        textInput(inputId='var_plots__custom_y_axis_label',
                                  label="Y-Axis Label", width='100%', value=NULL),
                        textInput(inputId='var_plots__custom_caption',
                                  label="Caption", width='100%', value=NULL),
                        textInput(inputId='var_plots__custom_tag',
                                  label="Tag", width='100%', value=NULL),
                        bsTooltip(id='var_plots__custom_title',
                                  title="Adds or replaces a title to the graph.",
                                  placement='top', trigger='hover'),
                        bsTooltip(id='var_plots__custom_caption',
                                  title="Adds or replaces a caption on the graph (appears on the bottom-right of the graph).",
                                  placement='top', trigger='hover'),
                        bsTooltip(id='var_plots__custom_tag',
                                  title="Adds or replaces a tag on the graph (appears on the top-left of the graph).",
                                  placement='top', trigger='hover'),
                        sliderTextInput(inputId='var_plots__base_size',
                                        label='Text Size',
                                        choices=seq(6, 20, 1),
                                        selected=15,
                                        width='100%',
                                        grid=TRUE),
                        checkboxInput(inputId='var_plots__pretty_text',
                                      label='Pretty Text', value=FALSE, width='100%'),
                        bsTooltip(id='var_plots__pretty_text',
                                  title="For instance, changes `column_name` to `Column Name`. WARNING: can be slow for large datasets.",
                                  placement='top', trigger='hover'),
                        textAreaInput('var_plots__vertical_annotations',
                                      "Vertical Annotations", value = "", width = '100%', height = 150),
                        bsTooltip(id='var_plots__vertical_annotations',
                                  title="Add vertical lines and text to the graph. Each line\\/text in the graph should be referenced in this text-box on its own line. Each line should have two values seperated by a \\';\\'. The first value is the x-axis location. The second value is the text to display.",
                                  placement='top', trigger='hover'),
                        textAreaInput('var_plots__horizontal_annotations',
                                      "Horizontal Annotations", value = "", width = '100%', height = 150),
                        bsTooltip(id='var_plots__horizontal_annotations',
                                  title="Add horizontal lines and text to the graph. Each line\\/text in the graph should be referenced in this text-box on its own line. Each line should have two values seperated by a \\';\\'. The first value is the y-axis location. The second value is the text to display.",
                                  placement='bottom', trigger='hover'),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Map Options',
                        shinyjs::hidden(
                            checkboxInput(inputId='var_plots__map_format',
                                          label='Format as Map', value=FALSE, width='100%'),
                            textInput(inputId='var_plots___map_borders_database',
                                      label="Borders Database", width='100%', value = NULL),
                            textInput(inputId='var_plots___map_borders_regions',
                                      label="Regions", width='100%', value = NULL)
                        ),
                        bsTooltip(id='var_plots___map_borders_database',
                                  title="Possible values include `world`, `usa`, `state`, `county`, and more; see docs https://ggplot2.tidyverse.org/reference/borders.html",
                                  placement='top', trigger='hover'),
                        bsTooltip(id='var_plots___map_borders_regions',
                                  title="e.g. `WA`, or `WA, OR, CA`",
                                  placement='top', trigger='hover')
                    )
                )
            ),
            column(9,
                plotOutput(outputId='var_plots'),
                verbatimTextOutput(outputId='var_plots__filtering_messages'),
                verbatimTextOutput(outputId='var_plots__ggplot_messages'),
                shinyjs::hidden(actionButton(inputId='var_plots__generate_link', "Generate Link (Beta)"))
            )
        ),
        tabPanel(
            'Numeric Summary',
            column(2,
                   class='column-input-control-style',
                   tags$div(class='input-control-style', uiOutput('numeric_summary__options__UI'))
            ),
            column(10, tags$div(class='results-table', dataTableOutput(outputId='numeric_summary__table')))
        ),
        tabPanel(
            'Categoric Summary',
            tags$div(class='results-table', dataTableOutput(outputId='categoric_summary__table')),
            tags$br(),
            h4('Summary of Values'),
            tags$div(style='width: 800px', verbatimTextOutput(outputId='categoric_summary__text'))
        ),
        tabPanel(
            'Correlations',
            column(
                2,
                class='column-input-control-style',
                tags$div(
                    class='input-control-style',
                    sliderInput(inputId='correlation__max_missing_column_perc',
                                label='Max Perc of Missing Data', ## percent increase
                                min=0,
                                max=100,
                                step=5,
                                value=5,
                                post="%"),
                    bsTooltip(id='correlation__max_missing_column_perc',
                          title="Only includes columns that have less than x% missing data.",
                          placement='top', trigger='hover'),
                    sliderTextInput(inputId='correlation__corr_threshold',
                                    label='Min Correlation Threshold', ## percent increase
                                    choices=seq(0, 1, 0.05),
                                    selected=0,
                                    grid=TRUE),
                    bsTooltip(id='correlation__corr_threshold',
                          title="Only show pairs of correlations above the selected threshold.",
                          placement='top', trigger='hover'),
                    sliderTextInput(inputId='correlation__p_value_threshold',
                                    label='Max P-Value Treshold',
                                    choices=seq(0, 1, 0.05),
                                    selected=1,
                                    grid=TRUE),
                    bsTooltip(id='correlation__p_value_threshold',
                          title="Only show pairs of correlations that have a p-value below the selected threshold.",
                          placement='top', trigger='hover'),
                    sliderTextInput(inputId='correlation__base_size',
                                    label='Text Size',
                                    choices=seq(6, 20, 1),
                                    selected=15,
                                    grid=TRUE),
                    checkboxInput(inputId='correlation__pretty_text',
                                  label='Pretty Text', value=FALSE, width='100%'),
                    bsTooltip(id='correlation__pretty_text',
                          title="For instance, changes `column_name` to `Column Name`. WARNING: can be slow for large datasets.",
                          placement='top', trigger='hover')
                )
            ),
            column(10,
                   plotOutput(outputId='correlation__plot')
            )
        ),
        tabPanel(
            'Regression (Beta)',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='regression__collapse_controls', open='Variables', multiple=TRUE,
                    bsCollapsePanel(
                        'Variables',
                        tags$div(style='margin-bottom: 25px;',
                                 actionButton(inputId='regression__run_button', label='Run Regression')
                        ),
                        uiOutput('regression__dependent_variable__UI'),
                        uiOutput('regression__independent_variables__UI'),
                        actionButton(inputId='regression__toggle_all_ind_variables',
                                     label='Toggle All Variables'),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Interaction Effects',
                        uiOutput('regression__interaction_term1__UI'),
                        uiOutput('regression__interaction_term2__UI'),
                        style='default'
                    )
                )
            ),
            column(9,
                tabsetPanel(type="tabs",
                    tabPanel("Output",
                        hidden(tags$h4(id='regression__formula_header', 'Formula')),
                        verbatimTextOutput(outputId='regression__formula'),
                        tags$br(),
                        hidden(uiOutput('regression__summary_header__UI')),
                        verbatimTextOutput(outputId='regression__number_of_rows_missing_removed'),
                        tags$br(),
                        verbatimTextOutput(outputId='regression__summary_output')
                    ),
                    tabPanel("Plots",
                        tags$br(),
                        tabsetPanel(type="tabs",
                            tabPanel("Actual vs Predicted",
                                plotOutput(outputId='regression__diagnostic_actual_vs_predicted')
                            ),
                            tabPanel("Residuals vs Predicted",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_fitted')
                            ),
                            tabPanel("Residuals vs Predictors",
                                uiOutput('regression__residuals_vs_predictors_var__UI'),
                                plotOutput(outputId='regression__diagnostic_residuals_vs_predictors')
                            ),
                            tabPanel("Predicted vs Observed",
                                plotOutput(outputId='regression__diagnostic_actual_vs_observed')
                            ),
                            tabPanel("Normal Q-Q",
                                plotOutput(outputId='regression__diagnostic_normal_qq')
                            ),
                            tabPanel("Scale-Location",
                                plotOutput(outputId='regression__diagnostic_scale_location')
                            ),
                            tabPanel("Cooks Distance",
                                plotOutput(outputId='regression__diagnostic_cooks_distance')
                            ),
                            tabPanel("Residuals vs. Leverage",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_leverage')
                            ),
                            tabPanel("Cooks Distance vs Leverage",
                                plotOutput(outputId='regression__diagnostic_cooks_distance_vs_leverage')
                            )
                        )
                    ),
                    tabPanel("VIFs",
                        hidden(tags$h4(id='regression__vif_header', 'Variance Inflation Factors')),
                        verbatimTextOutput(outputId='regression__summary_vif')
                    )
                )   
            )
        )
    )
))
