#Need to add warning for functions that produce different types of output

## Checks if args in .call will produce identical output in other functions

#param .call A function call such as '.call = map(1:3, cumsum)'
#param ...   Functions, for example, lapply() in 'is.out.same(map(1:3, cumsum), lapply)'

require(magrittr)
require(purrr)

is.output.same <- function(.call, ...) {

  #Error checking
  .call # To make sure the call is valid before proceeding
  if (!all(map_lgl(list(...), is.function))) stop('An optional argument is not a function.')
  
  call <- substitute(.call)               # Captures function call
  f_names <- eval(substitute(alist(...))) # Makes list of f_names
  
  
  map2(rep(list(call), length(f_names)),  # Creates list of new function calls
       f_names,
       function(.x, .y, i) {.x[[1]] <- .y; return(.x)} 
    ) %>%
    map(eval) %>%                         # Evaluates function calls
    map_lgl(identical, x = .call) %>%     # Checks output of new calls against output of original call 
    all()                                 # Returns TRUE if calls produce identical outputs
}


## Examples

# is.out.same(map(1:3, cumsum), lapply) #TRUE
# is.out.same(cumsum(1:3), cumprod) #FALSE
# is.out.same(sum(1:3), prod, iris) # Intended error
# is.out.same(subset(iris, Sepal.Width < 5), filter) #TRUE
# is.out.same(sum(1:3), prod) #Why is the output wrong?

# Doesn't work unfortunately 
# map(1:3, cumsum) %>% 
#   is.out.same(cumprod)