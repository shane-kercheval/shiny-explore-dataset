library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('helper_scripts/definitions.R')
source('helper_scripts/generic_helpers.R')

shinyUI(fluidPage(theme="custom.css",

    useShinyjs(),
    div(
        id = 'div__loading_page',
        class='loading_page',
        tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
        div(class='loading_page_text', "Loading Application...")
    ),
    shinyjs::hidden(div(id = 'div__main_content',
    navbarPage(id='navbar_page_app',
               title = "Dataset Explorer",
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
                                    selected='Credit'))
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
                id='load_data__dataset_tabset_panel',
                tabPanel(
                    "First 500 Records of Dataset",
                    tags$div(class='results-table', DT::dataTableOutput(outputId='source_data__head_table'))
                ),
                tabPanel(
                    'Dataset Description',
                    tags$br(),
                    verbatimTextOutput(outputId='load_data__description')
                ),
                tabPanel(
                    'Variable Types',
                    tags$div(class='results-table', DT::dataTableOutput(outputId='source_data__types_table'))
                ),
                tabPanel(
                    'Update Dataset w/ R Code',
                    tags$div(class='code_text',
                        textAreaInput(inputId='load_data__r_code_text',
                                      label="",
                                      value = "# dataset = dataset %>% mutate(column = column / 10)\n",
                                      width = 800,
                                      height = 300,
                                      cols = NULL,
                                      rows = NULL,
                                      placeholder = NULL,
                                      resize = NULL)
                    ),
                    tags$div(class='error_output',
                        shinyjs::hidden(
                            verbatimTextOutput(outputId='load_data__r_code_error')
                        )
                    ),
                    bsTooltip(id='load_data__r_code_text',
                                  title="Write R code to manipulate the currently loaded dataset. The loaded dataset should be referred to explicitly as `dataset` in the code.",
                                  placement='top', trigger='hover'),
                    actionButton(inputId='load_data__r_code_apply', label='Run Code'),
                    tags$br(),tags$br(),tags$br()
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
                        selectInput(inputId='var_plots__variable',
                                    label='Variable',
                                    choices=var_plots__default_values[['var_plots__variable']],
                                    selected=var_plots__default_values[['var_plots__variable']],
                                    width='100%'),

                        shinyjs::hidden(
                            div(id='var_plots__ts_date_floor', style="display:inline-block",
                                selectInput(inputId='var_plots__ts_date_floor',
                                            label=NULL,
                                            choices=global__date_part_vector,
                                            selected=var_plots__default_values[['var_plots__ts_date_floor']],
                                            width=100)
                            )
                        ),
                        shinyjs::hidden(
                            div(id='var_plots__convert_primary_date_to_categoric',
                                style='display:inline-block; vertical-align: text-bottom; height:40px; margin-left: 10px',
                                checkboxInput(inputId='var_plots__convert_primary_date_to_categoric',
                                              label="Convert to Categoric",
                                              value=var_plots__default_values[['var_plots__convert_primary_date_to_categoric']],
                                              width=165)
                            )
                        ),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__comparison',
                                        label='Secondary Variable',
                                        choices=var_plots__default_values[['var_plots__comparison']],
                                        selected=var_plots__default_values[['var_plots__comparison']],
                                        width='100%')
                        ),
                        shinyjs::hidden(
                            div(id='var_plots__convert_numerics_to_categoric',
                                checkboxInput(inputId='var_plots__convert_numerics_to_categoric',
                                              label="Convert to Categoric",
                                              value=var_plots__default_values[['var_plots__convert_numerics_to_categoric']],
                                              width='100%')
                            )
                        ),
                        shinyjs::hidden(
                            div(id='var_plots__convert_numerics_to_categoric__num_groups',
                                #style="margin-left: 50px",
                                sliderInput(inputId='var_plots__convert_numerics_to_categoric__num_groups',
                                            label='Number of Groups',
                                            min=1,
                                            max=20,
                                            step=1,
                                            value=var_plots__default_values[['var_plots__convert_numerics_to_categoric__num_groups']],
                                            width='100%')
                            )
                        ),
                        shinyjs::hidden(
                            div(id='var_plots__convert_numerics_to_categoric__x_cut_sequence',
                                style="display:inline-block",
                            textInput(inputId='var_plots__convert_numerics_to_categoric__x_cut_sequence',
                                      label="X Cut Sequence",
                                      width=120,
                                      value=var_plots__default_values[['var_plots__convert_numerics_to_categoric__x_cut_sequence']]))
                        ),
                        shinyjs::hidden(
                            div(id='var_plots__convert_numerics_to_categoric__y_cut_sequence',
                                style="display:inline-block; margin-left: 10px",
                                textInput(inputId='var_plots__convert_numerics_to_categoric__y_cut_sequence',
                                          label="Y Cut Sequence",
                                          width=120,
                                          value=var_plots__default_values[['var_plots__convert_numerics_to_categoric__y_cut_sequence']])
                            )
                        ),
                        shinyjs::hidden(
                            div(id='var_plots__convert_numerics_to_categoric__cut_seq_apply',
                                style="display:inline-block; margin-left: 10px",
                                actionButton(inputId='var_plots__convert_numerics_to_categoric__cut_seq_apply',
                                                             label='Apply')
                            )
                        ),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__num_cat_aggregation_type',
                                        label='Aggregation Type',
                                        choices=global__num_cat_aggregation_type,
                                        selected=var_plots__default_values[['var_plots__num_cat_aggregation_type']],
                                        width='100%')
                        ),
                        shinyjs::hidden(
                            checkboxInput(inputId='var_plots__numeric_group_comp_variable',
                                          label='Group Secondary Variable',
                                          value=var_plots__default_values[['var_plots__numeric_group_comp_variable']],
                                          width='100%')
                        ),
                        bsTooltip(id='var_plots__numeric_group_comp_variable',
                                  title="Treats the `Secondary Variable` as discrete values, and aggregates the `Variable` based on the discrete groups of the `Secondary Variable`.",
                                  placement='top', trigger='hover'),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__numeric_aggregation_function',
                                        label='Aggregation Type',
                                        choices=c('Boxplot', 'Mean', 'Geometric Mean', 'Median', 'Total'),
                                        selected=var_plots__default_values[['var_plots__numeric_aggregation_function']],
                                        multiple=FALSE,
                                        selectize=TRUE,
                                        width=500)
                        ),
                        bsTooltip(id='var_plots__numeric_aggregation_function',
                                  title="Determines how the `Variable` is aggregated, after grouping the `Secondary Variable`.",
                                  placement='top',
                                  trigger='hover'),
                        # NOTE: the variables below can't be hidden initially (but rather are dynamically hidden)
                        # because the values need to load the first time this tab is clicked
                        # if not, when they become active (and therefore the values change to the default,
                        # they will trigger an unncessary plot refresh
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__numeric_aggregation',
                                        label='Aggregation Type',
                                        choices=c('Total', 'Mean', 'Geometric Mean', 'Median'),
                                        selected=var_plots__default_values[['var_plots__numeric_aggregation']],
                                        multiple=FALSE,
                                        selectize=TRUE,
                                        width=500)
                        ),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__sum_by_variable',
                                        label='Sum By Variable',
                                        choices=var_plots__default_values[['var_plots__sum_by_variable']],
                                        selected=var_plots__default_values[['var_plots__sum_by_variable']],
                                        width='100%')
                        ),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__count_distinct_variable',
                                        label='Count Unique Values',
                                        choices=var_plots__default_values[['var_plots__count_distinct_variable']],
                                        selected=var_plots__default_values[['var_plots__count_distinct_variable']],
                                        width='100%')
                        ),
                        bsTooltipResistant(id='var_plots__count_distinct_variable',
                                           title="For the selected variable, count the unique number of values.",
                                           placement='top', trigger='hover'),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__color_variable',
                                        label='Color Variable',
                                        choices=var_plots__default_values[['var_plots__color_variable']],
                                        selected=var_plots__default_values[['var_plots__color_variable']],
                                        width='100%')
                        ),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__facet_variable',
                                        label='Facet Variable',
                                        choices=var_plots__default_values[['var_plots__facet_variable']],
                                        selected=var_plots__default_values[['var_plots__facet_variable']],
                                        width='100%')
                        ),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__size_variable',
                                        label='Size Variable',
                                        choices=var_plots__default_values[['var_plots__size_variable']],
                                        selected=var_plots__default_values[['var_plots__size_variable']],
                                        width='100%')
                        ),
                        shinyjs::hidden(
                            textInput(inputId='var_plots__multi_value_delimiter',
                                      width=150,
                                      label="Multi-Value Delimiter",
                                      value=var_plots__default_values[['var_plots__multi_value_delimiter']])
                        ),
                        bsTooltip(id='var_plots__multi_value_delimiter',
                                  title="Regex text used for variables that have multi-value instances seperated by a delimiter (e.g. value_x;value_y), this feature seperates the multi-value instance into multiple instances and counts them individually.",
                                  placement='top', trigger='hover'),
                        ################
                        # CONVERSION RATE FIELDS
                        ################
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__date_conversion_variable',
                                        label='Conversion Variable',
                                        choices=var_plots__default_values[['var_plots__date_conversion_variable']],
                                        selected=var_plots__default_values[['var_plots__date_conversion_variable']],
                                        width='100%')
                        ),
                        bsTooltipResistant(id='var_plots__date_conversion_variable',
                                           title="Select a second date column to graph conversion rates. The primary variable is treated as the initial date and this variable is treated as the date of conversion. Assumes 1 record per entity.",
                                           placement='top', trigger='hover'),
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__date_cr__snapshots__group_variable',
                                        label='Segment By Variable',
                                        choices=var_plots__default_values[['var_plots__date_cr__snapshots__group_variable']],
                                        selected=var_plots__default_values[['var_plots__date_cr__snapshots__group_variable']],
                                        width='100%')
                        ),
                        bsTooltipResistant(id='var_plots__date_cr__snapshots__group_variable',
                                           title="Segment the conversion rates by a categoric variable.",
                                           placement='top', trigger='hover'),
                        fluidRow(
                            div(style="display:inline-block; float:left; margin-bottom:0px; margin-top:10px; margin-left: 15px",
                                shinyjs::hidden(actionButton(inputId='var_plots__variables_buttons_clear',
                                                             label='Clear'))
                            ),
                            div(style="display:inline-block; float:left; margin-bottom:0px; margin-top:10px; margin-left: 10px",
                                shinyjs::hidden(actionButton(inputId='var_plots__variables_buttons_swap',
                                                             label='Swap Variables'))
                            ),
                            div(style="display:inline-block; float:left; margin-bottom:0px; margin-top:10px; margin-left: 10px",
                                shinyjs::hidden(actionButton(inputId='var_plots__color_facet_buttons_swap',
                                                             label='Swap Color/Facet'))
                            ),
                            bsTooltip(id='var_plots__variables_buttons_clear',
                                      title="Clear all of the variables selected.",
                                      placement='bottom', trigger='hover'),
                            bsTooltip(id='var_plots__variables_buttons_swap',
                                      title="Swap the Primary and Secondary Variables.",
                                      placement='bottom', trigger='hover'),
                            bsTooltip(id='var_plots__color_facet_buttons_swap',
                                      title="Swap the Color and Facet Variables.",
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
                        # Selection that contains all of the variables
                        uiOutput('var_plots__filter_controls_selections__UI'),
                        # dynamic list of all the actual controls
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
                        ######################################################################################
                        # Date Conversion Rate Options
                        ######################################################################################
                        selectInput(inputId='var_plots__date_cr__plot_type',
                                    label="Graph Type",
                                    choices=global__date_cr_options,
                                    selected=var_plots__default_values[['var_plots__date_cr__plot_type']],
                                    width=500),
                        bsTooltip(id='var_plots__date_cr__plot_type',
                                  title="View \\'snapshots\\' of the conversion rate for each cohort over time, or view an Adoption Curve over time.",
                                  placement='top', trigger='hover'),
                        div(id='var_plots__date_cr__snapshots__values', style="display:inline-block; vertical-align:top",
                            textInput(inputId='var_plots__date_cr__snapshots__values',
                                      label="Snaphots",
                                      value=var_plots__default_values[['var_plots__date_cr__snapshots__values']],
                                      width=100)
                        ),
                        bsTooltip(id='var_plots__date_cr__snapshots__values',
                                  title="Comma seperated numbers. If \\'Units\\', right, is set to \\'Days\\', for example, then this defines the days the snapshots will be on. For example, \\'1, 7, 14\\' will generate snapshots of the conversion rates 1, 7, and 14 days after the initial date.",
                                  placement='top', trigger='hover'),
                        div(id='var_plots__date_cr__n_units_after_first_date', style="display:inline-block; vertical-align:top",
                            sliderInput(inputId='var_plots__date_cr__n_units_after_first_date',
                                        label='N Units',
                                        min=1,
                                        max=100,
                                        step=1,
                                        value=var_plots__default_values[['var_plots__date_cr__n_units_after_first_date']],
                                        width=150)
                        ),
                        bsTooltip(id='var_plots__date_cr__n_units_after_first_date',
                                  title="Number of days/week/etc. after the first event.",
                                  placement='top', trigger='hover'),
                        div(id='var_plots__date_cr__snapshots__units', style="display:inline-block; margin-left: 10px",
                            selectInput(inputId='var_plots__date_cr__snapshots__units',
                                        label="Units",
                                        choices=global__date_cr__unit_options,
                                        selected=var_plots__default_values[['var_plots__date_cr__snapshots__units']],
                                        width=100)
                        ),
                        radioButtons(inputId='var_plots__date_cr__snapshots__color_or_facet',
                                     label="Group Snapshots By:",
                                     choices=global__date_cr_color_or_facet,
                                     selected=var_plots__default_values[['var_plots__date_cr__snapshots__color_or_facet']],
                                     inline=TRUE,
                                     width='100%'),
                        bsTooltip(id='var_plots__date_cr__snapshots__color_or_facet',
                                  title="Differentiate each snapshot with colored lines, or faceted. Ignored if \\'Year-Over-Year\\' is selected.",
                                  placement='top', trigger='hover'),
                        sliderInput(inputId='var_plots__date_cr__last_n_cohorts',
                                    label='Last N Cohorts',
                                    min=1,
                                    max=20,
                                    step=1,
                                    value=var_plots__default_values[['var_plots__date_cr__last_n_cohorts']],
                                    width='100%'),
                        bsTooltip(id='var_plots__date_cr__last_n_cohorts',
                                  title="Only include the last N number of Cohorts",
                                  placement='top', trigger='hover'),
                        ######################################################################################
                        # 
                        ######################################################################################
                        selectInput(inputId='var_plots__numeric_graph_type',
                                    label='Type',
                                    choices=c('Boxplot', 'Histogram'),
                                    selected=var_plots__default_values[['var_plots__numeric_graph_type']],
                                    width=500),
                        numericInput(inputId='var_plots__histogram_bins',
                                      label='Number of Bins',
                                      value=var_plots__default_values[['var_plots__histogram_bins']]),
                        selectInput(inputId='var_plots__categoric_view_type',
                                    label='View Type',
                                    choices=c("Bar", "Confidence Interval"),
                                    selected=var_plots__default_values[['var_plots__categoric_view_type']],
                                    multiple=FALSE,
                                    selectize=TRUE,
                                    width='100%'),
                        sliderTextInput(inputId='var_plots__filter_factor_lump_number',
                                        label="Top N Categories",
                                        choices=as.character(c("Off", seq(1, 10), seq(15, 50, 5))),
                                        selected=var_plots__default_values[['var_plots__filter_factor_lump_number']],
                                        grid=TRUE,
                                        width=500),
                        bsTooltip(id='var_plots__filter_factor_lump_number',
                                  title="Only show the top N categories. Groups all other categories into `Other` category.",
                                  placement='top', trigger='hover'),
                        selectInput(inputId='var_plots__label_variables',
                                    label = 'Label Variables',
                                    choices = NULL,
                                    selected = var_plots__default_values[['var_plots__label_variables']],
                                    multiple = TRUE,
                                    selectize = TRUE,
                                    width='100%',
                                    size = NULL),
                        numericInput(inputId='var_plots__numeric_aggregation_count_minimum',
                                     label='Minimum number of samples in group:',
                                     width='100%',
                                     value=var_plots__default_values[['var_plots__numeric_aggregation_count_minimum']]),
                        bsTooltip(id='var_plots__numeric_aggregation_count_minimum',
                                  title="The minimum number of samples/instances required in order to include the group in the graph.",
                                  placement='top', trigger='hover'),
                        div(id='var_plots__show_points',
                            style="display:inline-block; vertical-align:top; height:40px",

                            checkboxInput(inputId='var_plots__show_points',
                                          label='Show Points',
                                          value=var_plots__default_values[['var_plots__show_points']],
                                          width=110)
                        ),
                        div(id='var_plots__annotate_points',
                            style="display:inline-block; margin-left: 10px; height:40px",

                            checkboxInput(inputId='var_plots__annotate_points',
                                      label='Show Values',
                                      value=var_plots__default_values[['var_plots__annotate_points']],
                                      width=110)
                        ),
                        checkboxInput(inputId='var_plots__date_cr__separate_colors',
                                      label="Use dynamic color scheme.",
                                      value=var_plots__default_values[['var_plots__date_cr__separate_colors']],
                                      width='100%'),
                        bsTooltip(id='var_plots__date_cr__separate_colors',
                                  title="Changes the color scheme of the cohorted lines.",
                                  placement='top', trigger='hover'),
                        
                        checkboxInput(inputId='var_plots__year_over_year',
                                      label='Year-Over-Year',
                                      value=var_plots__default_values[['var_plots__year_over_year']],
                                      width='100%'),
                        checkboxInput(inputId='var_plots__numeric_show_resampled_conf_int',
                                      label='Show Confidence Interval (Resampling):',
                                      value=var_plots__default_values[['var_plots__numeric_show_resampled_conf_int']],
                                      width='100%'),
                        selectInput(inputId='var_plots__order_by_variable',
                                    label = "Order By",
                                    choices = c("Default", "Frequency"),
                                    selected = var_plots__default_values[['var_plots__order_by_variable']],
                                    width='100%'),
                        # BARCHART CONTROLS
                        bsTooltip(id='div_var_plots__group_barchar_controls',
                                  title="Orders by total number of records. Otherwise, orders by character or factor-level ordering.",
                                  placement='top', trigger='hover'),
                        checkboxInput(inputId='var_plots__reverse_stack_order',
                                      label='Reverse Stack Order',
                                      value=var_plots__default_values[['var_plots__reverse_stack_order']],
                                      width='100%'),
                        checkboxInput(inputId='var_plots__show_variable_totals',
                                      label='Show Variable Values',
                                      value=var_plots__default_values[['var_plots__show_variable_totals']],
                                      width='100%'),
                        checkboxInput(inputId='var_plots__show_comparison_totals',
                                      label='Show Secondary Values',
                                      value=var_plots__default_values[['var_plots__show_comparison_totals']],
                                      width='100%'),
                        # SCATTER CONTROLS
                        sliderTextInput(inputId='var_plots__transparency',
                                        label='Point Transparency',
                                        choices=c(seq(0, 90, 10), 99),
                                        selected=var_plots__default_values[['var_plots__transparency']],
                                        post  = " %",
                                        width='100%',
                                        grid=TRUE),
                        checkboxInput(inputId='var_plots__jitter',
                                      label='Jitter',
                                      value=var_plots__default_values[['var_plots__jitter']],
                                      width='100%'),
                        bsTooltip(id='var_plots__jitter',
                                  title="Adds a small amount of random variation to the location of each point, and is a useful way of handling overplotting caused by discreteness in datasets.",
                                  placement='top', trigger='hover'),
                        checkboxInput(inputId='var_plots__scatter_add_histograms',
                                      label='Add Histograms',
                                      value=var_plots__default_values[['var_plots__scatter_add_histograms']],
                                      width='100%'),
                        # TREND CONTROLS
                        radioButtons(inputId='var_plots__trend_line',
                                     label="Trend Line:",
                                     choices=c("None", "Straight", "Smooth", "Projection"),
                                     selected=var_plots__default_values[['var_plots__trend_line']],
                                     inline=TRUE,
                                     width='100%'),
                        shinyjs::hidden(
                            dateInput(inputId='var_plots__trend_extend_date',
                                      label="Extend Trend To",
                                      value=var_plots__default_values[['var_plots__trend_extend_date']])
                        ),
                        radioButtons(inputId='var_plots__trend_line_se',
                                     label="Trend Confidence Interval:",
                                     choices=c("No", "Yes"),
                                     selected=var_plots__default_values[['var_plots__trend_line_se']],
                                     inline=TRUE,
                                     width='100%'),
                        # TIME SERIES
                        selectInput(inputId='var_plots__ts_date_break_format',
                                    label='Date Format:',
                                    choices=global__date_break_format_vector,
                                    selected=var_plots__default_values[['var_plots__ts_date_break_format']],
                                    width='100%'),
                        textInput(inputId='var_plots__ts_breaks_width',
                                  label="Date Breaks",
                                  value=var_plots__default_values[['var_plots__ts_breaks_width']],
                                  width='100%'),
                        bsTooltip(id='var_plots__ts_breaks_width',
                                  title="Control how many x-axis labels there are. For example, valid values include text such as: `4 weeks` or `2 months`.",
                                  placement='top', trigger='hover'),
                        # X AXIS
                        checkboxInput(inputId='var_plots__scale_x_log_base_10',
                                      label='Scale X-Axis Log 10',
                                      value=var_plots__default_values[['var_plots__scale_x_log_base_10']],
                                      width='100%'),
                        bsTooltip(id='var_plots__scale_x_log_base_10',
                                  title="Adds a Log transformation to the values associated with the x-axis.",
                                  placement='top', trigger='hover'),
                        div(id='var_plots__x_zoom_min', style="display:inline-block",
                            numericInput(inputId='var_plots__x_zoom_min',
                                         label='X-Axis Min',
                                         value=var_plots__default_values[['var_plots__x_zoom_min']],
                                         width=100)
                        ),
                        div(id='var_plots__x_zoom_max', style="display:inline-block; margin-left: 10px",     
                            numericInput(inputId='var_plots__x_zoom_max',
                                         label='X-Axis Max',
                                         value=var_plots__default_values[['var_plots__x_zoom_max']],
                                         width=100)
                        ),
                        bsTooltip(id='var_plots__x_zoom_min',
                                  title='"Zoom" into the graph, using this value as the minimum x-axis coordinate',
                                  placement='top', trigger='hover'),
                        bsTooltip(id='var_plots__x_zoom_max',
                                  title='"Zoom" into the graph, using this value as the maximum x-axis coordinate',
                                  placement='top', trigger='hover'),
                        checkboxInput(inputId='var_plots__include_zero_y_axis',
                                          label='Include 0 in Y-Axis',
                                          value=var_plots__default_values[['var_plots__include_zero_y_axis']],
                                          width='100%'),
                        bsTooltip(id='var_plots__include_zero_y_axis',
                                      title="If selected, expand the lower bound of the y-axis to incldue 0 (which is the best practice).",
                                      placement='top', trigger='hover'),
                        # Y AXIS
                        checkboxInput(inputId='var_plots__scale_y_log_base_10',
                                      label='Scale Y-Axis Log 10',
                                      value=var_plots__default_values[['var_plots__scale_y_log_base_10']],
                                      width='100%'),
                        bsTooltip(id='var_plots__scale_y_log_base_10',
                                  title="Adds a Log transformation to the values associated with the y-axis.",
                                  placement='top', trigger='hover'),
                        div(id='var_plots__y_zoom_min', style="display:inline-block",
                            numericInput(inputId='var_plots__y_zoom_min',
                                         label='Y-Axis Min',
                                         value=var_plots__default_values[['var_plots__y_zoom_min']],
                                         width=100)
                        ),
                        div(id='var_plots__y_zoom_max', style="display:inline-block; margin-left: 10px",     
                            numericInput(inputId='var_plots__y_zoom_max',
                                         label='Y-Axis Max',
                                         value=var_plots__default_values[['var_plots__y_zoom_max']],
                                         width=100)
                        ),
                        bsTooltip(id='var_plots__y_zoom_min',
                                  title='"Zoom" into the graph, using this value as the minimum y-axis coordinate',
                                  placement='bottom', trigger='hover'),
                        bsTooltip(id='var_plots__y_zoom_max',
                                  title='"Zoom" into the graph, using this value as the maximum y-axis coordinate',
                                  placement='bottom', trigger='hover'),
                        shinyjs::hidden(
                            textInput(inputId='var_plots__mock_input',
                                      label="",
                                      value="",
                                      width='100%')
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
                                  label="Title",
                                  width='100%',
                                  value=var_plots__default_values[['var_plots__custom_title']]),
                        textInput(inputId='var_plots__custom_subtitle',
                                  label="Subtitle",
                                  width='100%',
                                  value=var_plots__default_values[['var_plots__custom_subtitle']]),
                        textInput(inputId='var_plots__custom_x_axis_label',
                                  label="X-Axis Label",
                                  width='100%',
                                  value=var_plots__default_values[['var_plots__custom_x_axis_label']]),
                        textInput(inputId='var_plots__custom_y_axis_label',
                                  label="Y-Axis Label",
                                  width='100%',
                                  value=var_plots__default_values[['var_plots__custom_y_axis_label']]),
                        textInput(inputId='var_plots__custom_caption',
                                  label="Caption",
                                  width='100%',
                                  value=var_plots__default_values[['var_plots__custom_caption']]),
                        textInput(inputId='var_plots__custom_tag',
                                  label="Tag",
                                  width='100%',
                                  value=var_plots__default_values[['var_plots__custom_tag']]),
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
                                        selected=var_plots__default_values[['var_plots__base_size']],
                                        width='100%',
                                        grid=TRUE),
                        checkboxInput(inputId='var_plots__pretty_text',
                                      label='Pretty Text',
                                      value=var_plots__default_values[['var_plots__pretty_text']],
                                      width='100%'),
                        bsTooltip(id='var_plots__pretty_text',
                                  title="For instance, changes `column_name` to `Column Name`. WARNING: can be slow for large datasets.",
                                  placement='top', trigger='hover'),
                        tags$div(class='code_text',
                            textAreaInput(inputId='var_plots__vertical_annotations',
                                          label="Vertical Annotations",
                                          value=var_plots__default_values[['var_plots__vertical_annotations']],
                                          width='100%',
                                          height=150)
                        ),
                        bsTooltip(id='var_plots__vertical_annotations',
                                  title="Add vertical lines and text to the graph. Each line\\/text in the graph should be referenced in this text-box on its own line. Each line should have two values seperated by a \\';\\'. The first value is the x-axis location. The second value is the text to display.",
                                  placement='top', trigger='hover'),
                        tags$div(class='code_text',
                            textAreaInput(inputId='var_plots__horizontal_annotations',
                                          label="Horizontal Annotations",
                                          value=var_plots__default_values[['var_plots__horizontal_annotations']],
                                          width='100%',
                                          height=150)
                        ),
                        bsTooltip(id='var_plots__horizontal_annotations',
                                  title="Add horizontal lines and text to the graph. Each line\\/text in the graph should be referenced in this text-box on its own line. Each line should have two values seperated by a \\';\\'. The first value is the y-axis location. The second value is the text to display.",
                                  placement='bottom', trigger='hover'),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Map Options',
                        shinyjs::hidden(
                            checkboxInput(inputId='var_plots__map_format',
                                          label='Format as Map',
                                          value=var_plots__default_values[['var_plots__map_format']],
                                          width='100%'),
                            textInput(inputId='var_plots__map_borders_database',
                                      label="Borders Database",
                                      width='100%',
                                      value=var_plots__default_values[['var_plots__map_borders_database']]),
                            textInput(inputId='var_plots__map_borders_regions',
                                      label="Regions",
                                      width='100%',
                                      value=var_plots__default_values[['var_plots__map_borders_regions']])
                        ),
                        bsTooltip(id='var_plots__map_borders_database',
                                  title="Possible values include `world`, `usa`, `state`, `county`, and more; see docs https://ggplot2.tidyverse.org/reference/borders.html",
                                  placement='top', trigger='hover'),
                        bsTooltip(id='var_plots__map_borders_regions',
                                  title="e.g. `WA`, or `WA, OR, CA`",
                                  placement='top', trigger='hover')
                    )
                )
            ),
            column(9,
                plotOutput(outputId='var_plots'),
                verbatimTextOutput(outputId='var_plots__filtering_messages'),
                verbatimTextOutput(outputId='var_plots__ggplot_messages'),
                shinyjs::hidden(div(id='var_plots__div__buttons_below_graphs',
                    actionButton(inputId='var_plots__generate_link', "Generate Link"),
                    actionButton(inputId='var_plots__clear_all_settings', label='Clear All Settings')
                ))
            )
        ),
        tabPanel(
            'Numeric Summary',
            column(2,
                   class='column-input-control-style',
                   tags$div(class='input-control-style', uiOutput('numeric_summary__options__UI'))
            ),
            column(10, tags$div(class='results-table', DT::dataTableOutput(outputId='numeric_summary__table')))
        ),
        tabPanel(
            'Categoric Summary',
            tags$div(class='results-table', DT::dataTableOutput(outputId='categoric_summary__table')),
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
    )) # end of main_content & hidden()
))
