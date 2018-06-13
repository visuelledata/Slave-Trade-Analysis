#' Checks if the arguments in .call will produce identical output in other functions
#' 
#' The use for this is to evaluate your code, while you're cleaning your functions. You 
#' can have a function call as the first argument and the name of the cleaned function
#' as the second argument, then it will evaluate the functions with identical arguments
#' and see if the output is identical. 
#' 
#' @param .call Function call, such as '.call = my_function(x = 1:3)
#' @param ...   One or more function names
#' @param quiet Optional warning suppression
#' 
#' @return TRUE or FALSE
#' 
#' @seealso [find_call_piped()]
#' 
#' @examples 
#' 
#' is.output.same(map(1:3, cumsum), lapply) # TRUE
#' is.output.same(cumsum(1:3), cumprod)     # FALSE
#' is.output.same(sum(1:3), prod)           # FALSE for outputs of different class
#' subset(iris, Sepal.Width < 5) %>%        # Pipeable
#'   is.output.same(filter)
#' mean(1:3) %>%                            # FALSE
#'   is.output.same(median)
#' 
require(magrittr)
require(purrr)

is.output.same <- function(.call, ..., quiet = FALSE) {

  # Optional warning 
  if(quiet == TRUE) suppressWarnings(.call <- find_call_piped(.call))
    else .call <- find_call_piped(.call)
  
  # Error checking
  eval(.call) # To make sure .call is valid before proceeding
  if (!all(map_lgl(list(...), is.function))) stop('An optional argument is not a function.')
  if (missing(...)) stop('Need at least 1 optional argument.')
  
  # Makes list of function names
  f_names <- eval(substitute(alist(...)))  
  
  # Creates new function calls, then evaluates them against .call and checks output
  map2(rep(list(.call), length(f_names)),  
       f_names,
       function(.x, .y, i) {
         .x[[1]] <- .y 
         return(.x)
        }) %>%
    map(eval) %>%                           
    map_lgl(identical, x = eval(.call)) %>% 
    all()                                   
}
