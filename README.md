# electricity-browser
R Shiny web app for visualizing state-level electricity data with data from Data from the U.S. Energy Information Agency (www.eia.gov)

A working copy of the Shiny App can be found at [shinyapps.io](https://nuclearkatie.shinyapps.io/electricity-browser/)

This project was developed for a special topics course in Interactive Data Visualization, taught by the [Industrial & Systems Engineering department](https://www.engr.wisc.edu/department/industrial-systems-engineering/) at the [University of Wisconsin-Madison](https://www.wisc.edu)

## Requirements

To run the following R shiny app on your own machine, you need

- [R](https://cran.r-project.org/mirrors.html)
- [RStudio](https://www.rstudio.com)

And the following R libraries. They can be installed by using the code
```install.packages('PACKAGE_NAME')``` in an R console

- shiny
- shinythemes
- ggplot2
- dplyr
- RColorBrewer
- ggthemes
- readxl
- leaflet
- maps
- geojson
- geojsonio
- tigris
- DT
- openintro
- censusapi
- viridis
- shinyWidgets

All packages can be installed simultaneously with the following code

```install.packages(c('shiny', 'shinythemes', 'ggplot2, 'dplyr', 'RColorBrewer', 'ggthemes', 'readxl', 'leaflet', 'maps', 'geojson', 'geojsonio', 'tigris', 'DT', 'openintro', 'censusapi', 'viridis', 'shinyWidgets'))```
