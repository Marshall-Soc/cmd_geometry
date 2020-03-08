#------------------------------------------------------------------------------
# DATA
#------------------------------------------------------------------------------

    sotu.texts <- quanteda.corpora::data_corpus_sotu$documents
    sotu.texts$docname <- sotu.texts$`_document`
    sotu.texts$`_document` <- NULL

#------------------------------------------------------------------------------
# Create DTM
#------------------------------------------------------------------------------

    sotu.dtm <- sotu.texts %>% 
        mutate(documents = str_replace_all(texts, "[^[:ascii:]]", " ")) %>%
        mutate(documents = str_replace_all(texts, "[[:punct:]]", " ")) %>%
        unnest_tokens(word, texts, to_lower = TRUE) %>%
        anti_join(stop_words) %>%
        filter(!str_detect(word, "[0-9]+") ) %>%
        dplyr::count(docname, word) %>%
        cast_dtm(term = word, document = docname, 
                value = n, weighting = tm::weightTf)

    sotu.dtm <- removeSparseTerms(sotu.dtm, .99) 

    # check dims
    dim(sotu.dtm) # 240 by 13303

##-----------------------------------------------------------------------------
#  BUILD Cultural Dimensions
##-----------------------------------------------------------------------------
   
    additions.1  <- c("liberal")
    substracts.1 <- c("conservative")

    additions.2  <- c("progressive")
    substracts.2 <- c("traditional")

    additions.3  <- c("democrat")
    substracts.3 <- c("republican")

    additions.4  <- c("progressive")
    substracts.4 <- c("conservative")

    additions.5  <- c("unconventional")
    substracts.5 <- c("conventional")

    additions.6  <- c("democratic")
    substracts.6 <- c("republican")

    additions  <- c("liberal", "progressive", "democratic", "progressive", "democrat", "unconventional")
    substracts <- c("conservative", "traditional", "republican", "conservative", "republican","conventional")

    antonyms.1 <- cbind(additions.1, substracts.1)
    antonyms.2 <- cbind(additions.2, substracts.2)
    antonyms.3 <- cbind(additions.3, substracts.3)
    antonyms.4 <- cbind(additions.4, substracts.4)
    antonyms.5 <- cbind(additions.5, substracts.5)
    antonyms.6 <- cbind(additions.6, substracts.6)
    antonyms   <- cbind(additions, substracts)

    cd.1 <- get_antodim(antonyms.1, ft.wv)
    cd.2 <- get_antodim(antonyms.2, ft.wv)
    cd.3 <- get_antodim(antonyms.3, ft.wv)
    cd.4 <- get_antodim(antonyms.4, ft.wv)
    cd.5 <- get_antodim(antonyms.5, ft.wv)
    cd.6 <- get_antodim(antonyms.6, ft.wv)
    cd.a <- get_antodim(antonyms, ft.wv)

    cd <- rbind(cd.1, cd.2, cd.3, cd.4, cd.5, cd.6, cd.a)

# -----------------------------------------------------------------------------
#  Get CMDs
# -----------------------------------------------------------------------------

    # run all concept words and cultural dimensions at the same time
    sotu.cmd <- CMDist(sotu.dtm, 
                        cw = c("nurture parent", "strict father"), 
                        cd = cd, 
                        wv = ft.wv, 
                        method = "cosine", scale = TRUE,
                        parallel = TRUE)
    
    # get correlation between "nurture parent" and "liberal" pole of CDim
    cor(sotu.cmd$cmd.nurture, sotu.cmd$cmd.liberal.pole.7)

    # merge with meta data
    sotu.texts <- left_join( sotu.texts, sotu.cmd, by = c("docname" = "docs") )


# -----------------------------------------------------------------------------
#  PLOTS
# -----------------------------------------------------------------------------


    # Over time for liberal pole of cultural dimension
    sotu.plot.cd <- sotu.texts %>% 
                mutate(liberal_dim = cmd.liberal.pole.7) %>%
                ggplot(aes(x = Date, y = as.numeric(liberal_dim))) +
    geom_smooth(aes(color = as.numeric(liberal_dim)), method = "loess", color="#000000",
                formula = y ~ x, span = .1) +
    geom_point(aes(color = as.numeric(liberal_dim)), size = 2) +
    ylim(-2.5,2.5) +
    xlab("Date") +
    ylab('Engagement with "Liberal" Pole') +
    scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                            name = "", 
                            limits = c(-2.5,2.5),
                            breaks = c(-2.5,2.5), labels = c("Conservative", "Liberal")) +
    theme(legend.position = "bottom",
            legend.key.size = unit(5, "mm")) 


    # Over time of "nurture + parent"
    sotu.plot.cw <- sotu.texts %>% 
            mutate(nurture_parent = cmd.nurture) %>%
            ggplot(aes(x = Date, y = as.numeric(nurture_parent))) +
    geom_smooth(aes(color = as.numeric(nurture_parent)), method = "loess", color="#000000",
                formula = y ~ x, span = .1) +
    geom_point(aes(color = as.numeric(nurture_parent)), size = 2) +
    ylim(-2.5,2.5) +
    xlab("Date") +
    ylab('Engagement with "Nurture + Parent"') +
    scale_color_gradient2(low = "#fdbf11", mid="#B2BECC", high = "#1696d2",
                            name = "", 
                            limits = c(-2.5,2.5),
                            breaks = c(-2.5,2.5), labels = c("", "Nurture + Parent")) +
    theme(legend.position = "bottom",
            legend.key.size = unit(5, "mm")) 


    png("figures/figure_cmd_sotu_cw_cd.png", 
        width = 9, height = 5, units = 'in', res = 400)
    ggarrange(sotu.plot.cd, sotu.plot.cw)
    dev.off()

    # Individual antonym pairs compared to full cultural dimension

      cmd.scatter.df <- sotu.texts %>% 
                    mutate(cmd.np   = as.numeric(cmd.nurture),
                           cmd.full = as.numeric(cmd.liberal.pole.7),
                           cmd.1    = as.numeric(cmd.liberal.pole.1),
                           cmd.2    = as.numeric(cmd.progressive.pole.2),
                           cmd.3    = as.numeric(cmd.democrat.pole.3),
                           cmd.4    = as.numeric(cmd.progressive.pole.4),
                           cmd.5    = as.numeric(cmd.unconventional.pole.5),
                           cmd.6    = as.numeric(cmd.democratic.pole.6)) %>%
                    dplyr::select(cmd.np, cmd.full, cmd.1, cmd.2, cmd.3, cmd.4, cmd.5, cmd.6)

    p.sc.np <- ggscatter(cmd.scatter.df, x = "cmd.full", y = "cmd.np",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = '"Liberal" pole of cultural dimension', 
          ylab = '"Nurture + Parent"',
          color = "#1696d2") +
          xlim(-3,3) +
          ylim(-3,3)
      
    p.sc.1 <- ggscatter(cmd.scatter.df, x = "cmd.full", y = "cmd.1",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = '"Liberal" pole of cultural dimension', 
          ylab = '"Liberal-Conservative"',
          color = "#1696d2") +
          xlim(-3,3) +
          ylim(-3,3)


    p.sc.2 <- ggscatter(cmd.scatter.df, x = "cmd.full", y = "cmd.2",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = '"Liberal" pole of cultural dimension', 
          ylab = '"Progressive-Traditional"',
          color = "#1696d2") +
          xlim(-3,3) +
          ylim(-3,3)

    p.sc.3 <- ggscatter(cmd.scatter.df, x = "cmd.full", y = "cmd.3",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = '"Liberal" pole of cultural dimension', 
          ylab = '"Democrat-Republican"',
          color = "#1696d2") +
          xlim(-3,3) +
          ylim(-3,3)

    p.sc.4 <- ggscatter(cmd.scatter.df, x = "cmd.full", y = "cmd.4",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = '"Liberal" pole of cultural dimension', 
          ylab = '"Progressive-Conservative"',
          color = "#1696d2") +
          xlim(-3,3) +
          ylim(-3,3)

    p.sc.5 <- ggscatter(cmd.scatter.df, x = "cmd.full", y = "cmd.5",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = '"Liberal" pole of cultural dimension', 
          ylab = '"Unconventional-Conventional"',
          color = "#1696d2") +
          xlim(-3,3) +
          ylim(-3,3)

    p.sc.6 <- ggscatter(cmd.scatter.df, x = "cmd.full", y = "cmd.6",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = '"Liberal" pole of cultural dimension', 
          ylab = '"Democratic-Republican"',
          color = "#1696d2") +
          xlim(-3,3) +
          ylim(-3,3)



    png("figures/figure_cmd_sotu_scatters.png", 
        width = 12, height = 7, units = 'in', res = 350)
    ggarrange(p.sc.1, p.sc.2, 
              p.sc.3, p.sc.4, p.sc.5, 
              p.sc.6)
    dev.off()

# -----------------------------------------------------------------------------
# THE END
# -----------------------------------------------------------------------------