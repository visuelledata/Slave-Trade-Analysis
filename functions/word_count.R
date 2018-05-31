require(tidyverse)
require(stringr)
word_count <- function(data, char_col){
        possibly(
           function(data, char_col) {
              char_col <- enquo(char_col)
              char_col_i <- quo_name(char_col)
              
              data %>% 
                select(!!char_col) %>% 
                select_if(is.character) %>% 
                mutate(!!char_col_i := str_remove_all(!!char_col, '[[:punct:]]')) %>% 
                mutate(!!char_col_i := str_split(!!char_col, ' ')) %>% 
                separate(char_col_i, into = paste0('col', 1:30), fill = 'right') %>% 
                select(-col1) %>% 
                gather(value = word) %>% 
                select(word) %>% 
                janitor::remove_empty(c('rows')) %>%
                filter(word != '') %>% 
                mutate(word = str_to_lower(word)) %>% 
                group_by(word) %>% 
                summarize(freq = n()) %>% 
                arrange(desc(freq))
            },
         otherwise = 'char_col must be the name of a character vector.',
         quiet = FALSE)
}
