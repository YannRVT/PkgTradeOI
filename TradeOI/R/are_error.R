#' Function to check if there is an error in dataset 1
#'
#' @param log1_dataset dataset with error info
#'
#' @import compiler
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export
#'

are_error_in_DS <- compiler::cmpfun(are_error_in_DS <- function(log1_dataset){
  print("in are_error_in_DS")
  if(length(which(log1_dataset$type=="error"))>0){
    error_input <- 1
  }
  else{
    error_input <- 0
  }
  return(error_input)
})