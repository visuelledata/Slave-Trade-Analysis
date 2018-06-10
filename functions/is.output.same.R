# To do: add support for mutate(iris, col1 + col2) style functions

require(purrr)
require(magrittr) 
require(rlang) # add_quo uses this package
source('functions/add_quo.R') # Helper function that wraps certain arguments in quo()

#param function_list is a list of functions
#param ... are the arguments for those functions

is.output.same<- function(.f_list, ...) {
  ## Checks to see if functions produce identical output
  
  param_list <- add_quo(...) # Enables function with dplyr syntax to work
  
  # Error checking
  if(!all(map_lgl(.f_list, is.function)))
    stop('Element in function_list is not a function.')
  if(length(param_list) == 0)
    stop('Example call: is.output.same(list(cumsum, cumsum), x = c(1,2,3))
         Argument must be given. See example above.')
  if(identical(reduce(map(.f_list, formalArgs), intersect), character(0))) 
    stop('Functions must have a common argument.') 
  
  invoke_map(.f = .f_list, .x = param_list) %>%  # Pass args to functions
    reduce(identical) # Sees if all outputs are identical
}



# ctrl + shift + c while all code is highlighted to test is.output.same

# m <- function(data, col_name) {
#   col_name <- enquo(col_name)
#   select(data, !!col_name)
# }
# n <- m
# 
# head(iris)
# 
# # Works with 'dplyr syntax' functions
# m(data = iris, col_name = Species)
# is.output.same(list(m, n), data = iris, col_name = Species)
#   #[1] TRUE
# 
# # Works with unnamed arguments
# select(.data = iris, Species, Sepal.Width)
# is.output.same(list(select, select), .data = iris, Species, Sepal.Width)
#   #[1] TRUE
# 
# # Does not work with 'dplyr syntax' functions that perform on operation such as the one in mutate
# mutate(.data = iris, sum_numeric = Sepal.Length + Sepal.Width)
# is.output.same(list(mutate, mutate), .data = iris, sum_numeric = Sepal.Length + Sepal.Width)
#   #[1] TRUE
# 
# #Supports unnamed arguments
# is.output.same(list(cumsum, cumsum), 1:4)
#   #[1] TRUE
# is.output.same(list(cumprod, cumsum), 1:4)
#   #[1] FALSE
# is.output.same(list(select, select), iris, Species, Sepal.Width)
#   #[1] TRUE