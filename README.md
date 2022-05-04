    library(here)

    ## here() starts at C:/Users/gp63dyte/Documents/projects/gptools

    library(tidyverse)

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.9
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## Warning: package 'tibble' was built under R version 4.1.3

    ## Warning: package 'tidyr' was built under R version 4.1.3

    ## Warning: package 'readr' was built under R version 4.1.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    file <- here("R/misc.R")
    file.exists(file)

    ## [1] TRUE

## Scripts

    scripts <- list.files(here("R"))
    scripts %>% writeLines

    ## 00-imports.R
    ## archd_fct.R
    ## comp_sim_df.R
    ## extract_nearest.R
    ## flags.R
    ## ggplot.R
    ## mahadist_map.R
    ## mapping.R
    ## markdown.R
    ## metadata.R
    ## misc.R
    ## notes_extract_nearest.R
    ## save_metadata.R
    ## sem_graphviz.R
    ## timestamps.R
    ## utils-pipe.R

## Packages used

    pkgs <- map(scripts, ~readr::read_lines(here("R", .x)) %>% str_extract("[a-zA-Z_-]*(?=::)") %>% na.omit %>% as.character) %>% unlist %>% 
      unique %>% sort %>% .[-1]

    pkgs %>% writeLines

    ## checkmate
    ## DiagrammeR
    ## dplyr
    ## gsubfn
    ## here
    ## leaflet
    ## lubridate
    ## magrittr
    ## mapview
    ## purrr
    ## raster
    ## readr
    ## rio
    ## rmarkdown
    ## rnaturalearth
    ## rstudioapi
    ## sf
    ## stringi
    ## stringr
    ## tibble
    ## tidyr
    ## tools
    ## utils
    ## xfun

## Functions

    fcts <- map(scripts, ~readr::read_lines(here("R", .x)) %>% str_extract("^.*(?= <- function\\()") %>% na.omit %>% as.character) %>% unlist %>% 
      .[str_detect(., "^[a-zA-Z]")]

    fcts %>% writeLines

    ## ad_init
    ## ad_archive
    ## ad_save
    ## ad_to_date
    ## ad_lsv
    ## default_fold
    ## mk_timestamp
    ## same_file
    ## create_log
    ## update_log
    ## comp_dfs
    ## list_cols_to_csv
    ## comp_sim_df
    ## comp_for
    ## comp_btw
    ## extract_nearest
    ## extract_nearest_layer
    ## extract_nearest_value
    ## make_flag_col
    ## add_flag
    ## get_flag
    ## one1_line
    ## plot_corr
    ## make_grid
    ## make_sf_wgs84
    ## qwmap
    ## gp_pointmap
    ## gp_point_ras
    ## gp_open_gmaps
    ## gp_gplot
    ## knit_w_tmst
    ## coldesc
    ## which.nonnum
    ## nonumdf
    ## gp_dputran
    ## gp_file_opened
    ## gp_jobinfo
    ## gp_scale2
    ## gp_ggaes
    ## wrnam
    ## mk_hdr
    ## up_date
    ## paste_na
    ## gp_unwrap
    ## prinf
    ## saveme
    ## ggsaveme
    ## record_meta
    ## create_project_file_tracking
    ## plot_psem
    ## plot_lavaan
    ## psemtolav
    ## tmst
    ## ptmst
    ## hptmst
    ## last_tmst
    ## list_tmst

### Spatial analysis

extract\_nearest  
extract\_nearest\_layer  
extract\_nearest\_value  
splibs  
make\_grid

### Mapping

gp\_pointmap  
gp\_point\_neighbor  
gp\_open\_gmaps  
gp\_gplot

### ggplot helpers

one1\_line  
plot\_corr  
gp\_ggaes

### Compare datasets

comp\_sim\_df  
comp\_for  
comp\_btw

### Flag entries in list-column

make\_flag\_col  
add\_flag  
get\_flag

### Misc

knit\_w\_tmst  
coldesc  
which.nonnum  
nonumdf  
gp\_dputran  
gp\_file\_opened  
gp\_jobinfo  
gp\_scale2  
wrnam  
mk\_hdr  
up\_date  
paste\_na  
gp\_unwrap  
prinf

### Data archiving (archd\_fct.R)

#### ArchD

This project is mostly deprecated. It provided a form of version control
for dataset that were modified within and without R, with saving
intermediate versions. It worked quite well, but was possibly overkill
for most tasks. It was abandoned in favor of a simpler approach
implemented with file tracking.

Functions: ad\_init, ad\_archive, ad\_save, ad\_to\_date, ad\_lsv,
default\_fold, mk\_timestamp, same\_file, create\_log, update\_log,
comp\_dfs, list\_cols\_to\_csv

#### File tracking

saveme  
ggsaveme  
record\_meta  
create\_project\_file\_tracking  
tmst  
ptmst  
hptmst  
last\_tmst  
list\_tmst

### Data Viz

plot\_psem  
plot\_lavaan  
psemtolav
