#' Finds the call that generated the value that was passed through a pipe. 
#' 
#' This is a function that receives a value through a pipe, then traces it through the stack to 
#' find the original pipe call and extract it. The use case for this is when you want to write a 
#' function that receives code rather than a value, you can put it as the first line of code in 
#' your function. However, this defeats the purpose of the pipe and can cause confusion for
#' someone looking at your code.
#'
#' @param .piped A value passed through a pipe.
#'
#' @return call
#'
#' @example
#' sum(1:3) %>%
#'   identity() %>% 
#'   is.na() %>%
#'   find_call_piped() 
#'   
   
find_call_piped <- function(.piped) {
  pipe_env <- purrr::compose(parent.frame, 
                             purrr::partial(tail, n = 3), 
                             purrr::head_while)
  
  env <- pipe_env(1:sys.nframe(), 
                  pryr::f(!('chain_parts' %in% ls(envir=parent.frame(.n)))))
  
  if (exists('chain_parts', env)) {
    warning("You have undone the evaluation of the pipe")
    return(env$chain_parts$lhs)
  } else {
    return(do.call("substitute", list(substitute(.piped), parent.frame())))
  }
}



