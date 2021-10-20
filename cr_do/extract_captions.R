# Dads For Life Captions Youtube videos "Voices of a Father" Series
## https://www.youtube.com/playlist?list=PLfK3JF5hCnygVtUaZXbpwc7nLrbMcszr3 


rm(list = ls())
library(tidyverse)
library(dplyr)
library(janitor)
library(quanteda)
library(stringr)
library(tidytext)
library(ggplot2)

# Extract captions for six videos 
#install.packages('youtubecaption')

library(youtubecaption)

# Jamal ####
url_jamal <- "https://www.youtube.com/watch?v=GbTdobt0yX0&list=PLfK3JF5hCnygVtUaZXbpwc7nLrbMcszr3&index=4&ab_channel=DadsforLife"
caption_jamal <- get_caption(url_jamal)
caption_jamal


# Collapse the text into one cell 
data1 <- as_tibble(paste(unlist(caption_jamal$text), collapse =" ")) 
data1 <- 
    data1 %>% 
    rename(text = value) %>% 
    mutate(id = 1)



# Eric Kwek ####
url_eric <- "https://www.youtube.com/watch?v=07G-Bzf5URQ&list=PLfK3JF5hCnygVtUaZXbpwc7nLrbMcszr3&index=6&ab_channel=DadsforLife"
caption_eric <- get_caption(url_eric)
caption_eric

# Collapse the text into one cell 
data2 <- as_tibble(paste(unlist(caption_eric$text), collapse =" ")) 
data2 <- 
    data2 %>% 
    rename(text = value) %>% 
    mutate(id = 2)



# Desmond Tin ### 

url_desmond <- "https://www.youtube.com/watch?v=zNDIFHrgDEQ&list=PLfK3JF5hCnygVtUaZXbpwc7nLrbMcszr3&index=8&ab_channel=DadsforLife"
caption_desmond <- get_caption(url_desmond)
caption_desmond


# Collapse the text into one cell 
data3 <- as_tibble(paste(unlist(caption_desmond$text), collapse =" ")) 
data3 <- 
    data3 %>% 
    rename(text = value) %>% 
    mutate(id = 3)


# Dan Ong ### 

url_dan <- "https://www.youtube.com/watch?v=vghebSc2CPg&list=PLfK3JF5hCnygVtUaZXbpwc7nLrbMcszr3&index=10&ab_channel=DadsforLife"
caption_dan <- get_caption(url_dan)
caption_dan


# Collapse the text into one cell 
data4 <- as_tibble(paste(unlist(caption_dan$text), collapse =" ")) 
data4 <- 
    data4 %>% 
    rename(text = value) %>% 
    mutate(id = 4)

# Desmond Koh ### 

url_koh <- "https://www.youtube.com/watch?v=Z9AZtNEK7aA&list=PLfK3JF5hCnygVtUaZXbpwc7nLrbMcszr3&index=12&ab_channel=DadsforLife"
caption_koh <- get_caption(url_koh)
caption_koh


# Collapse the text into one cell 
data5 <- as_tibble(paste(unlist(caption_koh$text), collapse =" ")) 
data5 <- 
    data5 %>% 
    rename(text = value) %>% 
    mutate(id = 5)



# Combine all the dataframes ####
data <- rbind(data1, data2, data3, data4, data5)
getwd()
save(data, file = "data/data.RData" )

# Names for the id
data$person <- factor(data$id, labels = c("Jamal", "Eric", "Desmond Tin", "Dan", "Desmond Koh"))


# Tokenize
corp <- corpus(data, 
               text_field = "text",
               docid_field = "id",
               )

tok <- 
    corp %>% 
    tokens(remove_numbers = T,
           remove_punct = T,
           remove_separators = T,
           remove_symbols = T,
           remove_url = T,
           include_docvars = T) %>% 
    tokens_tolower() %>% 
    tokens_select(pattern = stopwords("en"),
                  selection = "remove") %>% 
    tokens_wordstem()

dfm <-
    tok %>% dfm()

library(quanteda.textstats)

topfeatures(dfm, n = 100)

# Comparison cloud
library(showtext)
# You will need to have internet connection
# If you restart R you will need to execute this code again to use the font
font_add_google(name = "Roboto Condensed",   # Name of the font on the Google Fonts site
                family = "Roboto Condensed") # Name you want to use to call the font

#font_add_google(name = "Noto Sans",   # Name of the font on the Google Fonts site
#                family = "Noto Sans") # Name you want to use to call the font
#

library("quanteda.textplots")

# Cast to tidy
tidy <- tidy(dfm) 

person_names <- 
    data %>% 
    select(id, person) %>% 
    rename(document = id) 

tidy <-
    tidy %>% 
    mutate(document = as.integer(document)) %>% 
    left_join(person_names)


# TFIDF 
tfidf <- 
    tidy %>% 
    bind_tf_idf(term, document, count) %>% 
    arrange(desc(tf_idf))

tfidf_smallest <-
    tfidf %>% 
    slice_min(tf_idf, n = 800) %>% 
    select(document, term)

tidy_reduced <- 
    anti_join(tidy, tfidf_smallest) 

tidy_reduced <-  left_join(tidy_reduced, person_names)


reduced_dfm  <- 
    tidy_reduced %>% 
    arrange(document) %>%
    cast_dfm(document, term, count)

reduced_dfm@docvars <- dfm@docvars 


    # get meta data in too
    # https://github.com/juliasilge/tidytext/issues/114


set.seed(123)

reduced_dfm %>% 
    #dfm_group(groups = docid(dfm)) %>%
    dfm_group(groups = person) %>%
    dfm_trim(min_termfreq = 3, verbose = FALSE) %>%
    textplot_wordcloud(comparison = TRUE, 
                       family = "Roboto Condensed")

## Cast to tidy
#tidy <- tidy(dfm)
##install.packages("ggwordcloud")
#library(ggwordcloud)
#
#set.seed(42)
#tidy %>% 
#    filter(count>5) %>% 
#    ggplot(aes(label = word)) + 
#    geom_text_wordcloud(aes(label = term, size = count)) +
#    theme_minimal()
#
#
#    scale_size_area(max_size = 7)
#
#