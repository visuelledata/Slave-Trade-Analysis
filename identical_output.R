library(tidyverse)

identical_output <- function(function_list, param_list){
  param_list <- list(param_list) 
  
  output <- invoke_map(.f = function_list, .x = param_list) 
  
  if(reduce(function_list, identical)) print('The functions are identical.')
  else {
    ifelse(reduce(output, identical), TRUE, FALSE) 
    }
}

identical_output(function_list = list(cumprod, cumsum),  # This works
             param_list = list(x = iris$Sepal.Length))


identical_output(function_list = list(cumsum, cumsum), # This works
             param_list = list(x = iris$Sepal.Length))

m <- function(data, col_name){ # Enquo test function
  col_name <- enquo(col_name)
  data %>% 
    select(!!col_name)
}
n <- function(data, col_name){ # Enquo test function
  identical(1,1)
  col_name <- enquo(col_name)
  data %>% 
    select(!!col_name)
}


n(iris, Species) # Seeing if the functions work

identical_output(function_list = list(m, n),     # The call doesn't work
             param_list = list(data = iris, col_name = quo(Species)))
