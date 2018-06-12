quote_pipe <- function()
{
  function(lhs, rhs)
  {
    # the parent environment
    parent <- parent.frame()
    
    # the environment in which to evaluate pipeline
    env    <- new.env(parent = parent)
    
    # split the pipeline/chain into its parts.
    chain_parts <- split_chain(match.call(), env = env)
    
    pipes <- chain_parts[["pipes"]] # the pipe operators.
    rhss  <- chain_parts[["rhss" ]] # the right-hand sides.
    lhs   <- chain_parts[["lhs"  ]] # the left-hand side.
    
    # Create the list of functions defined by the right-hand sides.
    env[["_function_list"]] <- 
      lapply(1:length(rhss), 
             function(i) wrap_function(rhss[[i]], pipes[[i]], parent))
    
    # Create a function which applies each of the above functions in turn.
    env[["_fseq"]] <-
      `class<-`(eval(quote(function(value) freduce(value, `_function_list`)), 
                     env, env), c("fseq", "function"))
    
    # make freduce available to the resulting function 
    # even if magrittr is not loaded.
    env[["freduce"]] <- freduce 
    
    # Result depends on the left-hand side.
    if (identical(lhs, quote(.))) {
      # return the function itself.
      env[["_fseq"]]
    } else {
      # evaluate the LHS
      env[["_lhs"]] <- lhs
      
      # compute the result by applying the function to the LHS
      result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      
      # If compound assignment pipe operator is used, assign result
      result[["value"]]
    }
  }
}
`%''%`  <- quote_pipe()

split_chain <- function(expr, env) 
{
  # lists for holding the right-hand sides and the pipe operators.
  rhss  <- list()
  pipes <- list()
  
  # Process the call, splitting it at each valid magrittr pipe operator.
  i <- 1L 
  while(is.call(expr) && identical(expr[[1L]], quote(`%''%`))) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]
    
    if (is_parenthesized(rhs))
      rhs <- eval(rhs, env, env)
    
    rhss[[i]] <- 
      if (is_function(rhs) || is_colexpr(rhs))
        prepare_function(rhs)
    else if (is_first(rhs))
      prepare_first(rhs)
    else
      rhs
    
    # Make sure no anonymous functions without parentheses are used.
    if (is.call(rhss[[i]]) && identical(rhss[[i]][[1L]], quote(`function`)))
      stop("Anonymous functions must be parenthesized", call. = FALSE)
    
    expr <- expr[[2L]]
    i <- i + 1L
  }
  # return the components; expr will now hold the left-most left-hand side.
  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}


wrap_function <- function(body, pipe, env) {
  eval(call("function", as.pairlist(alist(.=)), body), env, env)
}


is_parenthesized <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`(`))
}


is_first <- function(expr) {
  !any(vapply(expr[-1L], identical, logical(1L), quote(.)))
}

is_colexpr <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`(`))
}


prepare_first <- function(expr) {
  as.call(c(expr[[1L]], quote(.), as.list(expr[-1L])))
}

