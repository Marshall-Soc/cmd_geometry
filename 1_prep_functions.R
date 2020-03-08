# ---------------------------------------------- #
## Project Title: Concept Movers' Distance with Cultural Dimensions
## 
## Date: 2020-02-12 21:13:37
## Author: Marshall A. Taylor and Dustin S. Stoltz
# ---------------------------------------------- #

# -----------------------------------------------------------------------------
# PACKAGES
# -----------------------------------------------------------------------------

    # devtools::install_github("dustinstoltz/CMDist", force=TRUE)

    pacman::p_load(
        tidyverse, dplyr,
        gutenbergr, tidytext, 
        text2vec, CMDist, MASS,
        extrafont, ggplot2, 
        textclean, tm, googledrive,
        ggpubr, ggrepel,
        install = TRUE)

    urbnthemes::set_urbn_defaults()

    # load fastText word embeddings prepared from this file: 
    # https://dl.fbaipublicfiles.com/fasttext/vectors-english/wiki-news-300d-1M.vec.zip
    ft.wv <- readRDS("data/ft_en.Rds")

    # You can download the fastText Word Vectors hosted on Google Drive:
    # see https://googledrive.tidyverse.org/
    temp <- tempfile()
    drive_download(as_id("1Z0W9tXF459b6R_bS4zvOhbDNI_nLVpHr"), path = temp, overwrite = TRUE)
    ft.wv <- readRDS(temp)

# -----------------------------------------------------------------------------
# THE END
# -----------------------------------------------------------------------------

library(googledrive)
temp <- tempfile(fileext = ".zip")
dl <- drive_download(
  as_id("1AiZda_1-2nwrxI8fLD0Y6e5rTg7aocv0"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())
bank <- read.csv(out[14], sep = ";")


library(googledrive)
temp <- tempfile(fileext = ".zip")
dl <- drive_download(
  as_id("1AiZda_1-2nwrxI8fLD0Y6e5rTg7aocv0"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())
bank <- read.csv(out[14], sep = ";")