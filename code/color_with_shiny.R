
library(tidyverse)
library(rhandsontable)
library(janitor)
library(shiny)

## awesome csv for getting RGB hexcodes from DMC thread #'s!
## source: https://github.com/adrianj/CrossStitchCreator/blob/master/CrossStitchCreator/Resources/DMC%20Cotton%20Floss%20converted%20to%20RGB%20Values.csv
dmc <- 
  read_csv('./data/DMC Cotton Floss converted to RGB Values.csv') %>% 
  clean_names()

## DMC thread numbers for cactus cross stitch
cactus_cols <- c(469, 471, 472, 677, 520, 522, 524, 502, 503, 927)

## filter thread colors to ones we need
hex_vec <- dmc %>% 
                filter(floss_number %in% cactus_cols) %>%
                mutate(rgb_code = paste0('#', rgb_code)) %>%
                pull(rgb_code)


## stitch dimensions of graphic
stitch_ht <- 50
stitch_wd <- 55

## create blank data frame for coloring in
color_df <- matrix('-', stitch_ht, stitch_wd) %>%
              data.frame %>%
              mutate_all(factor, levels=c('-', hex_vec))

rownames(color_df) <- stitch_ht:1
colnames(color_df) <- 1:stitch_wd


## shiny app for interactive data coloring, heavily borrowed from 
## (1) Rhandsontable vignette: https://rpubs.com/jrowen/intro_rhandsontable
## (2) this blog post: http://stla.github.io/stlapblog/posts/shiny_editTable.html

color_with_shiny <- function(DF, outdir=getwd(), outfilename="shiny_hexcol"){
 
 ui <- shinyUI(fluidPage(
    
    titlePanel("Cross Stitch Data Coloring"),
    
    sidebarLayout(
      sidebarPanel(
        
        
        h4('Use this Shiny app to color in cells with RGB hex codes. Some options:'),
        
        tags$ol(
          tags$li("Color an individual cell from a dropdown list"), 
          tags$li("Color a cell, copy it, and paste into another cell"), 
          tags$li("Color a cell, copy it, drag selection of new cells, and copy
               same color into all of them at once!")
        ),
         
          actionButton("save", "Save to RDS"),

          uiOutput("message", inline=TRUE)
        
      ),
      
      mainPanel(
        
        plotOutput('palette'),
        
        rHandsontableOutput("hot")

      )
    )
  ))
  
  server <- shinyServer(function(input, output) {
    
    ## show hex color options
    output$palette <- renderPlot({
      scales::show_col(hex_vec, ncol = 5)
    })
    
    ## initialize reactive value for current table snapshot
    cur_table <- reactiveValues()
    
    ## update rHandsontable on changes... carry over from source (2)
    observe({
      if (!is.null(input$hot)) {
        cur_table[["previous"]] <- isolate(cur_table[["DF"]])
        DF = hot_to_r(input$hot)
      } else {
        if (is.null(cur_table[["DF"]]))
          DF <- DF
        else
          DF <- cur_table[["DF"]]
      }
      cur_table[["DF"]] <- DF
    })
    
    ## plot updated table with cell background colors
    output$hot <- renderRHandsontable({
      DF <- cur_table[["DF"]]
      
      if (!is.null(DF)){
        
        ## show current data frame
        rhandsontable(DF) %>%
          
          ## change cell color to hex value it is filled with (color as you go)
          hot_cols(renderer = 
            "function (instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.TextRenderer.apply(this, arguments);
                    td.style.background = value;
                }") 
      }
        
    })
    
    ## Save to output RDS file on action button
    observeEvent(input$save, {
      
      fileType <- isolate(input$fileType)
      finalDF <- isolate(cur_table[["DF"]])
      
      saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
      
    })
    
    
    ## Message about output file
    output$message <- renderUI({
      if(input$save==0){
        
        helpText(sprintf("This data will be saved in folder \"%s\" once you press the Save button.", outdir))
      
      } else{
        
        helpText(sprintf("File saved: \"%s\".", file.path(outdir, outfile)))
      }
    })
    
  })
  
  ## run app 
  runApp(list(ui=ui, server=server))
  return(invisible())
  
}

## run shiny app! be sure to use specific output filepath
color_with_shiny(color_df, outdir='./data/', outfile='cactus_shiny_hexcol')
