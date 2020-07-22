# ---------------------------------------------- #
## Project Title: Concept Mover's Distance with Semantic Directions
## 
## Date: 2020-02-12 21:13:37
## Author: Marshall A. Taylor and Dustin S. Stoltz
# ---------------------------------------------- #

##-----------------------------------------------------------------------------
# DATA
##-----------------------------------------------------------------------------

play.meta <- read_csv("data/2_shakes_meta.csv")

# Grab the text from Project GUTENBERG
plays <- play.meta %>% 
    dplyr::select(gutenberg_id) %>%
    gutenberg_download() %>%
    group_by(gutenberg_id) %>%
    summarise(text = paste(text, collapse=", ") )

# Clean PLAY Text and UNNEST words
play.text <- plays %>% 
    mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
    mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
    mutate(text = replace_white(text)) %>%
    mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
    mutate(text = replace_number(text)) %>%
    unnest_tokens(word, text, to_lower = TRUE) %>%
    anti_join(stop_words) %>%
    filter(!str_detect(word, "[0-9]+") ) %>%
    count(gutenberg_id, word) %>%
    bind_tf_idf(word, gutenberg_id, n)

#------------------------------------------------------------------------------
# Create DTM
#------------------------------------------------------------------------------

  play.dtm <- play.text %>%
    cast_dtm(term = word, document = gutenberg_id, 
            value = n, weighting = tm::weightTf) %>%
    removeSparseTerms(0.99) # 37 by 22782 (99%)
  
  # check dims
  dim(play.dtm)

##-----------------------------------------------------------------------------
#  Get CMDs
##-----------------------------------------------------------------------------

  # For scatter plots and regressions
  # Build cultural dimension of Death/Life
    additions.sp  <- c("death", "casualty", "demise", "dying", "fatality")
    substracts.sp <- c("life", "survivor", "birth", "living", "endure")

    antonyms.sp <- cbind(additions.sp, substracts.sp)
    cd.sp <- get_direction(antonyms.sp, ft.wv)


  # Run CMD for concept words and cultural dimension
  play.cmd <- play.dtm %>%
    CMDist(cw=c("life", "death", "love", "hate"), cd = cd.sp, 
            wv = ft.wv, 
            method = "cosine", scale = T) %>%
    mutate(gutenberg_id= as.numeric(as.character(docs) ) ) %>%
    left_join(play.meta )

##-----------------------------------------------------------------------------
# PLOT
##-----------------------------------------------------------------------------

  #Correlation between "life" and "death"
  cor.1 <- ggscatter(play.cmd, x = "cmd.life", y = "cmd.death", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = 'Engagement with "Life"', 
            ylab = 'Engagement with "Death"',
            color = "#1696d2") +
            xlim(-2.5,2.5) +
            ylim(-2.5,2.5)

  #Correlation between "life" and "death"
  cor.2 <- ggscatter(play.cmd, x = "cmd.love", y = "cmd.hate", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = 'Engagement with "Love"', 
            ylab = 'Engagement with "Hate"',
            color = "#1696d2") +
            xlim(-2.5,2.5) +
            ylim(-2.5,2.5)

  #Combine them
  cor.1.2 <- ggarrange(cor.1, cor.2, align = "hv")

  png("figures/figure_cmd_shakespeare.png", 
      width = 6, height = 4, units = 'in', res = 700)
  cor.1.2
  dev.off()

##-----------------------------------------------------------------------------
# REGRESSIONS
##-----------------------------------------------------------------------------

  # Decide on estimator
  ggplot(play.cmd, aes(body_count) ) + 
    geom_histogram(binwidth = 1) # Should do a count model

    m1 <- glm.nb(body_count ~ cmd.death.pole.1, 
              data = play.cmd)

    m2 <- glm(body_count ~ cmd.death.pole.1, 
              data = play.cmd, family = "poisson")

    m3 <- glm.nb(body_count ~ cmd.life, 
              data = play.cmd)

    m4 <- glm(body_count ~ cmd.life, 
              data = play.cmd, family = "poisson")

    m5 <- glm.nb(body_count ~ cmd.death, 
              data = play.cmd)

    m6 <- glm(body_count ~ cmd.death, 
              data = play.cmd, family = "poisson")

    pchisq(2 * (logLik(m1) - logLik(m2)), df = 1, lower.tail = F) # Specifically an nbreg
    pchisq(2 * (logLik(m3) - logLik(m4)), df = 1, lower.tail = F)
    pchisq(2 * (logLik(m5) - logLik(m6)), df = 1, lower.tail = F)

  # Models
  glm.nb(body_count ~ cmd.death.pole.1, data = play.cmd) %>% summary()
  glm.nb(body_count ~ poly(cmd.death.pole.1, 2), data = play.cmd) %>% summary()
  glm.nb(body_count ~ cmd.life, data = play.cmd) %>% summary()
  glm.nb(body_count ~ cmd.death, data = play.cmd) %>% summary()

  # Predicted Body Counts
  new.data <- data.frame(cmd.death.pole.1 = -1:1) # death pole
  new.data$phat <- predict(m1, new.data, type = "response")
  new.data

  new.data <- data.frame(cmd.death = -1:1) # death
  new.data$phat <- predict(m5, new.data, type = "response")
  new.data

  # lm(cmd.death.pole.1 ~ body_count, data = play.cmd) %>% summary()
  # lm(cmd.life ~ body_count, data = play.cmd) %>% summary()
  # lm(cmd.death ~ body_count, data = play.cmd) %>% summary()

