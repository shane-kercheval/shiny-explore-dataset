library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('helper_scripts/definitions.R')

shinyUI(fluidPage(theme="custom.css",

    useShinyjs(),

    titlePanel('Explore Dataset'),
    navlistPanel(
        tabPanel(
            'Load Dataset',
            tabsetPanel(type='tabs',
                tabPanel(
                    'Preloaded Dataset',
                    tags$br(),
                    tags$div(style='margin-bottom: 40px !important;',
                        selectInput(inputId='preloaded_dataset',
                                          label='Choose a Dataset:',
                                          choices=c('Diamonds',
                                                    'Credit',
                                                    'Housing',
                                                    'Insurance',
                                                    'Iris',
                                                    'Flights',
                                                    'Gapminder'),
                                          selected='Diamonds'))
                ),
                tabPanel(
                    'Load .csv/.RDS',
                    tags$br(),
                    fileInput(inputId='uploadFile', 'Choose a File:')
                ),
                tabPanel(
                    'Load .csv from URL',
                    tags$br(),
                    textInput(inputId='load_data__url_csv', label="URL", value = NULL, width = 600),
                    actionButton(inputId='load_data__url_csv_button', label='Load Data')
                )
            ),
            tags$br(),
            tags$br(),
            uiOutput('source_data__add_date_fields__UI'),
            tags$br(),
            tags$br(),
            tabsetPanel(type='tabs',
                tabPanel(
                    "First 500 Records of Dataset",
                    tags$div(class='results-table', dataTableOutput(outputId='source_data__head_table'))
                ),
                tabPanel(
                    'Variable Types',
                    tags$div(class='results-table', dataTableOutput(outputId='source_data__types_table'))
                )
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
                    sliderTextInput(inputId='correlation__corr_threshold',
                                    label='Min Correlation Threshold', ## percent increase
                                    choices=seq(0, 1, 0.05),
                                    selected=0,
                                    grid=TRUE),
                    sliderTextInput(inputId='correlation__p_value_threshold',
                                    label='Max P-Value Treshold',
                                    choices=seq(0, 1, 0.05),
                                    selected=1,
                                    grid=TRUE),
                    sliderTextInput(inputId='correlation__base_size',
                                    label='Text Size',
                                    choices=seq(6, 20, 1),
                                    selected=15,
                                    grid=TRUE),
                    checkboxInput(inputId='correlation__pretty_text',
                                  label='Pretty Text', value=FALSE, width=NULL)
                    )
            ),
            column(10,
                   plotOutput(outputId='correlation__plot')
            )
        ),
        tabPanel(
            'Variable Plots',
            column(3,
                class='column-input-control-style',

                bsCollapse(id='var_plots__bscollapse', open='Variables', multiple=TRUE,
                    bsCollapsePanel(
                        'Variables',
                        uiOutput('var_plots__variable__UI'),
                        uiOutput('var_plots__comparison__UI'),
                        # NOTE: the variables below can't be hidden initially (but rather are dynamically hidden)
                        # because the values need to load the first time this tab is clicked
                        # if not, when they become active (and therefore the values change to the default,
                        # they will trigger an unncessary plot refresh
                        uiOutput('var_plots__date_aggregation__UI'),
                        uiOutput('var_plots__sum_by_variable__UI'),
                        uiOutput('var_plots__color_variable__UI'),
                        uiOutput('var_plots__point_size__UI'),
                        textInput('var_plots__multi_value_delimiter',
                                  label="Multi-Value Delimiter"),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Filters',
                        fluidRow(
                            column(4,
                                checkboxInput(inputId='var_plots__filter_use',
                                              label='Use Filters', value=FALSE, width=NULL),
                                bsTooltip(id='var_plots__filter_use',
                                  title="Warning! Data that has NA/missing values for numeric variables will be removed.",
                                  placement='bottom', trigger='hover')
                            ),
                            column(8,
                                tags$div(style='margin-bottom: 10px;', actionButton(inputId='var_plots__filter_apply', label='Apply Filters')),
                                tags$div(style='margin-bottom: 20px;', actionButton(inputId='var_plots__filter_clear', label='Clear Filters'))
                            )
                        ),
                        uiOutput('var_plots__filter_bscollapse__UI')
 
                    ),
                    bsCollapsePanel(
                        'Plot Options',
                        shinyjs::hidden(
                            selectInput(inputId='var_plots__numeric_graph_type',
                                    label='Type',
                                    choices=c('Boxplot', 'Histogram'),
                                    selected='Boxplot')
                        ),
                        shinyjs::hidden(tags$div(id='div_var_plots__group_barchar_controls',
                            checkboxInput(inputId='var_plots__order_by_count',
                                          label='Order By Totals', value=TRUE, width=NULL),
                            checkboxInput(inputId='var_plots__show_variable_totals',
                                          label='Show Variable Totals', value=TRUE, width=NULL),
                            checkboxInput(inputId='var_plots__show_comparison_totals',
                                          label='Show Comparison Totals', value=TRUE, width=NULL)
                        )),
                        shinyjs::hidden(tags$div(id='div_var_plots__multi_barchar_controls',
                            checkboxInput(inputId='var_plots__stacked_comparison',
                                          label='Stack Comparison Variable', value=TRUE, width=NULL)
                        )),
                        shinyjs::hidden(
                             numericInput(inputId='var_plots__histogram_bins',
                                          label='Number of Bins',
                                          value=30)                 
                        ),
                        shinyjs::hidden(tags$div(id='div_var_plots__group_scatter_controls',
                            sliderTextInput(inputId='var_plots__transparency',
                                            label='Transparency',
                                            choices=c(seq(0, 90, 10), 99),
                                            selected=60,
                                            post  = " %",
                                            grid=TRUE),
                            checkboxInput(inputId='var_plots__jitter',
                                          label='Jitter', value=FALSE, width=NULL)
                        )),
                        shinyjs::hidden(tags$div(id='div_var_plots__group_trend_controls',
                            radioButtons(inputId='var_plots__trend_line',
                                         label='Trend Line:',
                                         choices=c('None', 'Straight', 'Smooth'),
                                         selected='None',
                                         inline=TRUE,
                                         width=NULL),
                            radioButtons(inputId='var_plots__trend_line_se',
                                         label='Trend Confidence Interval:',
                                         choices=c('No', 'Yes'),
                                         selected='Yes',
                                         inline=TRUE,
                                         width=NULL)
                        )),
                        shinyjs::hidden(tags$div(id='div_var_plots__group_x_zoom_controls',
                             checkboxInput(inputId='var_plots__scale_x_log_base_10',
                                           label='Scale X-Axis Log 10', value=FALSE, width=NULL),
                             numericInput(inputId='var_plots__x_zoom_min',
                                          label='X-Axis Zoom Min',
                                          value=NULL),
                             numericInput(inputId='var_plots__x_zoom_max',
                                          label='X-Axis Zoom Max',
                                          value=NULL)                         
                        )),
                        shinyjs::hidden(tags$div(id='div_var_plots__group_y_zoom_controls',
                             checkboxInput(inputId='var_plots__scale_y_log_base_10',
                                           label='Scale Y-Axis Log 10', value=FALSE, width=NULL),
                             numericInput(inputId='var_plots__y_zoom_min',
                                          label='Y-Axis Zoom Min',
                                          value=NULL),
                             numericInput(inputId='var_plots__y_zoom_max',
                                          label='Y-Axis Zoom Max',
                                          value=NULL)                         
                        )),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Pretty Options',
                        sliderTextInput(inputId='var_plots__base_size',
                                        label='Text Size',
                                        choices=seq(6, 20, 1),
                                        selected=15,
                                        grid=TRUE),
                        checkboxInput(inputId='var_plots__pretty_text',
                                      label='Pretty Text', value=FALSE, width=NULL),
                        shinyjs::hidden(
                           checkboxInput(inputId='var_plots__annotate_points',
                                         label='Annotate Points', value=FALSE, width=NULL)
                        ),
                        numericInput(inputId='var_plots__filter_factor_lump_number',
                                     label='Top N Categories',
                                     value=10),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Map Options',
                        shinyjs::hidden(
                            checkboxInput(inputId='var_plots__map_format',
                                          label='Format as Map', value=FALSE, width=NULL),
                            textInput(inputId='var_plots___map_borders_database', label="Borders Database", value = NULL),
                            textInput(inputId='var_plots___map_borders_regions', label="Regions", value = NULL)
                        ),
                        bsTooltip(id='var_plots___map_borders_database',
                                  title="Possible values include `world`, `usa`, `state`, `county`, and more; see docs https://ggplot2.tidyverse.org/reference/borders.html",
                                  placement='bottom', trigger='hover'),
                        bsTooltip(id='var_plots___map_borders_regions',
                                  title="e.g. `WA`, or `WA, OR, CA`",
                                  placement='bottom', trigger='hover')

                    )
                )
            ),
            column(9,
                plotOutput(outputId='var_plots'),
                verbatimTextOutput(outputId='var_plots__ggplot_messages')
            )
        ),
        tabPanel(
            'Regression',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='regression__collapse_controls', open='Variables', multiple=TRUE,
                    bsCollapsePanel(
                        'Variables',
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
                        tags$br(),
                        actionButton(inputId='regression__run_button', label='Run Regression'),
                        tags$br(),tags$br(),
                        hidden(tags$h4(id='regression__formula_header', 'Formula')),
                        verbatimTextOutput(outputId='regression__formula'),
                        tags$br(),
                        hidden(uiOutput('regression__summary_header__UI')),
                        verbatimTextOutput(outputId='regression__number_of_rows_missing_removed'),
                        tags$br(),
                        verbatimTextOutput(outputId='regression__summary_output')
                    ),
                    tabPanel("VIFs",
                        hidden(tags$h4(id='regression__vif_header', 'Variance Inflation Factors')),
                        verbatimTextOutput(outputId='regression__summary_vif')
                    ),
                    tabPanel("Diagnostic Plots",
                        tags$br(),
                        tabsetPanel(type="tabs",
                            tabPanel("Actual vs Predicted",
                                plotOutput(outputId='regression__diagnostic_actual_vs_predicted')
                            ),
                            tabPanel("Residuals vs Fittted",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_fitted')
                            ),
                            tabPanel("Actual vs Observed",
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
                    )
                )   
            )
        ),
        widths=c(2,10)
    )
))
