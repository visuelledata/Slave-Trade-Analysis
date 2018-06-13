#' Gives word frequencies from a character or factor column of a tibble
#' 
#' Takes a tibble and character column name as input, then returns the 
#' frequencies of that column in a new tibble.  
#' 
#' @param .data A tibble.
#' @param .char_col The name of a character or factor column in the tibble.
#' 
#' @return tibble with a column for word and a column for the frequency
#' 
#' @example 
#' iris %>% 
#'   as.tibble() %>% 
#'   word_count(Species)
#'   

require(tidyverse)
require(stringr)

word_count <- function(.data, .char_col) {

  if(!is.tibble(.data)) {warning(".data is not a tibble, errors may occur")}
  
  .char_col <- enquo(.char_col)
  
  .data %>% 
    select(!!.char_col) %>% 
    select_if(function(x) is.factor(x) || is.character(x)) %>% # Gives error when col type isn't chr or fct
    mutate(word = str_remove_all(!!.char_col, '[[:punct:]]')) %>% # Removes punctuation
    mutate(word = str_split(word, ' ')) %>% #Separate words into a vec of strings
    unnest() %>%                           # Expands the list column into a char column
    filter(word != "") %>%                 # Remove all empty strings
    mutate(word = str_to_lower(word)) %>%  # Puts everything in lowercarse
    group_by(word) %>%                     # Groups by word
    summarize(freq = n()) %>%              # Calculates the number of each word
    arrange(desc(freq))                    # Sorts the tibble by descending frequency
}
# Catches the error thrown by select_if and gives an error message
word_count <- purrr::possibly(
                        .f = word_count, 
                        otherwise = 'char_col should be a character column (or factor)', 
                        quiet = FALSE)
