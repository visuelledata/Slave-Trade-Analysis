require(tidyverse)
require(stringr)

#param data is a tibble
#param char_col is a name of a character column within data
#param len_longest_str is the maximum number of words in any string within char_col

word_count <- function(data, char_col) {
  ##  Provides word count from character column of a tibble.
  if(!is.tibble(data)) {stop("data is not a tibble, errors may occur")}
  
  char_col <- enquo(char_col)
  
  data %>% 
    select(!!char_col) %>% 
    select_if(function(x) is.factor(x) || is.character(x)) %>% # Gives error when column type isn't chr of fct
    mutate(word = str_remove_all(!!char_col, '[[:punct:]]')) %>% # Removes punctuation
    mutate(word = str_split(word, ' ')) %>% #Separate words into a vec of strings
    unnest() %>%                           # Expands the list column into a char column
    filter(word != "") %>%                 # Remove all empty strings
    mutate(word = str_to_lower(word)) %>%  # Puts everything in lowercarse
    group_by(word) %>%                     # Groups by word
    summarize(freq = n()) %>%              # Calculates the number of each word
    arrange(desc(freq))                    # Sorts the tibble by descending frequency
}

# Catches the error thrown by select_if and gives an error message
word_count <- possibly(.f = word_count, 
                        otherwise = 'char_col should be a character column (or factor)', 
                        quiet = FALSE)
