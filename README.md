## cross_stitch_plots

R code for reading and plotting cross stitch art

## `plot_cactus_stitch.R`

### Process data
- Read in cactus color data as hex codes, from XLSX or CSV or RDS
- convert from "color matrix" to long data frame, remove white (unfilled) cells
- make rectangular grid of stitch holes, check if they fall within a circular polygon, and keep the ones that do

### Create nice, minimalist plot 
- use `geom_point(shape = '.')` for small holes 
- use pairs of diagonal `geom_segment`'s for stitches, with `scale_color_identity` to keep actual hex colors
- use `geom_sf` for outer ring

## `color_with_shiny.R`

Shiny app for collecting / drawing your own color data
- specify a palette of hex colors or DMC thread numbers
- use the `rhandsontable` package to make a fillable, interactive data frame with the color data
- click save button to export data to an RDS file

![Shiny app demo]('./shiny_app_demo.gif')

## Cactus data source

The cactus color data came from this cross stitch kit on Etsy: https://www.etsy.com/listing/586195752/desert-cacti-diy-cross-stitch-kit