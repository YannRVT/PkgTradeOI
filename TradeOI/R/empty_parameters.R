#' Function to check if there is no empty parameters
#'
#' @param input_dataset dataset 
#'
#' @import compiler
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export
#'

is_empty_parameter1 <- compiler::cmpfun(is_empty_parameter <- function(input_dataset){
  print("in is_empty_parameter")
  
  if(nrow(input_dataset)<2){
    TK_emplty <- 1
  }
  else{
    TK_emplty <- 0
  }
  return(TK_emplty)
}
)