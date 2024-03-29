# ---------------------------------------------- #
## Project Title: Concept Mover's Distance with Semantic Directions
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
        textclean, tm,
        ggpubr, ggrepel,
        install = TRUE)

    urbnthemes::set_urbn_defaults()

    # load your fastText word embeddings prepared from this file: 
    # https://dl.fbaipublicfiles.com/fasttext/vectors-english/wiki-news-300d-1M.vec.zip
    # ft.wv <- readRDS("")

    # You can also download our Rds of the fastText Word Vectors we have
    # hosted on Google Drive (see https://googledrive.tidyverse.org/)
    temp <- tempfile()
    googledrive::drive_download(as_id("17H4GOGedeGo0urQdDC-4e5qWQMeWLpGG"), 
                                path = temp, overwrite = TRUE)
    ft.wv <- readRDS(temp)

# -----------------------------------------------------------------------------
# THE END
# -----------------------------------------------------------------------------
