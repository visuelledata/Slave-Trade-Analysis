require(purrr)
require(magrittr)
require(rlang)

#param ... is any list of arguments

add_quo <- function(...) {
  ## Formats optional arguments for use in invoke_map()
  
  # Create a logical vector for use as an index below
  index <- exprs(...) %>%                       # Captures the arguments
    unlist() %>%                                # Makes them mappable
    map_if(negate(is.name), 
           .f = function(x) {return(TRUE)}) %>% # Replaces unnecessary args with TRUE
    map_if(is.name, as.character) %>%           # Converts named arguments to characters
    map_if(is.character, exists) %>%            # Replaces named args that aren't objects as FALSE
    unlist()                                    # To make index mappable
  
  # Wraps named args that aren't objects in new_quosure()
  exprs(...) %>%                    # Captures arguments
    unlist() %>%                    # Makes them mappable
    map_if(!index, new_quosure,     # Wraps non-object named arg in new_quosure()
           env = global_env()) %>%  # new_quosure() used instead of quo() as env must be global
    map_if(index, eval) %>%         # Removes expr() formatting
    list()                          # Makes arguments usable in invoke_map()
}



# dotdotdot <- function(...) return(list(list(...))) # ... args are passed into invoke_map() like this
# 
# dotdotdot(data = iris, min = 5, x = 1:4, g = quo(aaa), mtcars, quo(neio))
# add_quo(data = iris, min = 5, x = 1:4, g = aaa, mtcars, neio)
# is.output.same(list(add_quo, dotdotdot))
# 
# a <- add_quo(data = iris, min = 5, x = 1:4, aesrtion) %>% # Can't use is.output.same as args are different
#   unlist() %>%
#   map(class)
# b <- dotdotdot(data = iris, min = 5, x = 1:4, g = quo(aesrtion)) %>%
#   unlist() %>%
#   map(class)
# identical(a,b)


