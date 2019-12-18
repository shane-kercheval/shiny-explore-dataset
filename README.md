# shiny-explore-dataset

Shiny Application for exploring new datasets.

## Hosted on shinyapps.io

The app is currently hosted at https://kercheval.shinyapps.io/shiny-explore-dataset/.

`shinyapps.io` gives a limited number of uptime hours per month at the free level, so it might be sporatically unavailable.

## Run Locally

This app utilitizes my [`rtools`](https://github.com/shane-kercheval/rtools) package which is a personal library of code that I typically re-use.

So, if you want to run locally, among the other libraries that are imported, you'll have to install `rtools` from github

```r
devtools::install_github('shane-kercheval/rtools')
```

## Notes On How to Use

The typical dataset to use with this app is a tidy dataset.

> In tidy data:
> 
> - Each variable forms a column.
> - Each observation forms a row.
> - Each type of observational unit forms a table.

https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html

### Loading datasets

![loading](./docs/loading_dataset.png)

- Loads either preloaded datasets, or either a .csv or .RDS
    - The downside of loading a CSV is that the app won't be able to use ordered factors, or anything that isn't automatically detected by `read.csv()`.
- `Add Date Fields based on Date Variable` adds various date fields to the dataset (e.g. year/month/week/day/is_weekend/etc.)

### Numeric Summary

![loading](./docs/numeric_summary.png)

### Categoric Summary

![loading](./docs/categoric_summary.png)

### Correlations

![loading](./docs/correlations.png)

### Variable Plots

![loading](./docs/variable_plots.png)

- Filtering Notes
    - when the filter is enabled, the header will turn green
    - when the filter is enabled and changes have been made but not applied, the header will turn red
    - sliders for filtering numeric data are inclusive 
- WARNING: enabling the filters removes all of the NA/missing values from each of the numberic variables even if the sliders are still set to the min/max values. Additionally, filtering numeric data removes the data from the dataset before it is graphed, changing the underlying distribution (unlike the x/y zoom min/max controls).

### Regression

![loading](./docs/regression.png)


# Version History (Major Changes)

* `1.0`: Stable 
* `1.1`: 
    * Added "Loading Application" screen 
    * refactored hide/show controls code to reset the variables back to the default value when hiding
    * updated logging
* `vCurrent`:
    * added conversion rate & adoption graphs
    * fixed/workaround for shinybs bug where toolstips don't work when controls are updated (e.g. via updateSelectInput)
    * cleaned up controls (converted some to inline/side-by-side)
    * added `Count Distinct` option to categoric variables; made `Multi-Value Delimiter` available for `Comparison Variable` & new `Count Distinct Variable`
    * added heatmaps for categoric/categoric graphs (with ability to convert numeric/numeric to categorics)
