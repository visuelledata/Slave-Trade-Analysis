require(purrr)

#param function_list is a list of functions
#param ... are the arguments for those functions

is.output.same <- function(function_list, ...) {
  ## Checks to see if functions produce identical output
  # Error checking
  if(!all(map_lgl(function_list, is.function)))
    stop('Element in function_list is not a function.')
  if(length(list(...)) == 0) 
    stop('Example call: is.output.same(list(cumsum, cumsum), x = c(1,2,3)) 
          Argument must be given. See example above.')
  if(identical(reduce(map(function_list, formalArgs), intersect), character(0))) 
    stop('Functions must have a common argument.') 
  
  output <- invoke_map(.f = function_list, .x = list(list(...))) # Pass args to functions
  reduce(output, identical) # Sees if all outputs are identical
}