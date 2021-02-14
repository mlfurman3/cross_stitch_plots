
library(tidyverse)
library(readxl)
library(sf)
library(sp)

## functions from sampSurf are needed but don't load bc there are 
## dependency conflicts with some tidyverse stuff! (e.g. dplyr::select)
# install.packages('sampSurf)

## read in cactus data, method 1 (excel spreadsheet)
cactus_sheet <- readxl::read_xlsx(path = './data/cactus_hexcol.xlsx', sheet=2)

## read in cactus data, method 2 (csv)
#cactus_csv <- read_csv('./data/cactus_hexcol.csv')
  
## read in cactus data, method 3 (rds)
## use this one if you collect and save color data with the shiny app 
# cactus_rds <- read_rds('./data/cactus_shiny_hexcol.rds') 
# cactus_rds <- cactus_rds %>%
#                  mutate_all(funs(str_replace(., '-','#FFFFFF'))) %>%
#                  add_column(rownum = nrow(cactus_rds):1, .before='1')

cactus_df <- cactus_sheet %>% 
  ## convert from wide to long format
  pivot_longer(cols = -rownum, names_to = 'colnum', values_to = 'hexcol') %>%
  mutate(colnum = as.numeric(colnum)) %>%
  ## remove white (uncolored) cells
  filter(hexcol != '#FFFFFF')

## plot cross stitch points as x's, using hex codes as colors
## geom_segment is used later, this is just to preview the graphic
cactus_df %>% 
  ggplot(aes(colnum, rownum, color = hexcol)) + 
  geom_point(shape = 4, size = 3) + 
  ## keep hex colors 
  scale_color_identity() + 
  theme_void()


## dimensions of graphic, aka number of stitch holes to make
stitch_ht <- 50
stitch_wd <- 55
#stitch_ht <- cactus_df %>% summarise(max(rownum)) %>% pull
#stitch_wd <- cactus_df %>% summarise(max(colnum)) %>% pull

## extra stitches to add to grid (to fill out rest of circle)
stitch_extra <- 20

## create rectangular grid of stitch holes 
fabric_df <- expand.grid(x = (0 - stitch_extra) : (stitch_wd + stitch_extra), 
                         y = (0 - stitch_extra) : (stitch_ht + stitch_extra))

## center point of cross stitch circle 
## (can tweak this if pattern is asymmetric)
circ_pt <- c(x = 28, y = 25.5)
# circ_pt <- c(x = median(1:stitch_wd), y = median(1:stitch_ht))

## radius of inner circle (for filtering points), in # of stitches
circ_rad_inner <- 32.5

## radius of outer circle (for drawing ring), in # of stitches
## (if you want holes very close to edges, keep these values close together!)
circ_rad_outer <- 34

## plot just the fabric
fabric_df %>% 
  ggplot(aes(x, y)) +
  # '.' shape is much smaller than default points
  geom_point(shape='.') + 
  theme_bw()


## create inner around center point
circ_inner <- sampSurf::spCircle(radius=circ_rad_outer,
                                  centerPoint = circ_pt)

## create outer circle around center point, for geom_sf layer in final plot
circ_outer <- sampSurf::spCircle(radius=circ_rad_outer,
                                 centerPoint = circ_pt) %>%
              purrr::pluck('spCircle') %>%
              sf::st_as_sf()


## create spatial polygon of fabric pts to keep
fabric_sp <- fabric_df %>% 
                as.matrix %>% 
                sp::SpatialPoints()

## check if fabric points are inside inner circle (point in polygon)
## returns 1 if true, NA if false
fabric_df <- fabric_df %>% 
                mutate(inside_circ = sp::over(fabric_sp, circ_inner$spCircle))

## filter fabric to set that falls within the inner circle
fabric_df_filt <- fabric_df %>% 
                    filter(inside_circ == 1)

## plot just points that fall inside circle
fabric_df_filt %>%
  ggplot(aes(x, y)) +
  # '.' shape is much smaller than default points
  geom_point(shape='.') + 
  theme_bw()

## plot everything together!
nice_plot <- ggplot() + 
  
  ## add outer ring
  geom_sf(data = circ_outer, fill = NA, col = 'tan', size = 4) + 
  
  ## add filtered fabric points 
  geom_point(data = fabric_df_filt, aes(x = x, y = y), shape = '.') + 
  
  ## add stitches as 2 diagonal segments, so they reach fabric holes
  
  ## bottom left to top right "/"
  geom_segment(data = cactus_df, 
               aes(x=colnum - 1, y = rownum - 1, xend = colnum, 
                   yend = rownum, color = hexcol), lwd = 1.1) +
  
  ## top left to bottom right "\"
  geom_segment(data = cactus_df, 
               aes(x = colnum - 1, y = rownum, xend = colnum, 
                   yend = rownum-1, color = hexcol), lwd = 1.1) + 
  
  scale_color_identity() + 
  theme_void()

## show plot
nice_plot

## not super savvy with ggsave, so I just used the export button

### small graphic - 560 x 630 pixels
### larger graphic - 937 x 852 pixels
