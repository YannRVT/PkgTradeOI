#' Function to check if there is no empty table during all the features
#'
#' @param log_dataset dataset with error info
#' @param note_txt info
#' @param IEx 
#' @param IEx_1 
#' @param error_input error info
#'
#' @import compiler
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export
#'

is_empty_output1 <- compiler::cmpfun(is_empty_output <- function(
  log_dataset,
  note_txt,
  IEx,
  IEx_1,
  error_input){
  
  print("in is_empty_output")
  if(IEx==1){
    if(error_input == 1 | IEx_1 == 1){
      log1 <- data.frame(step = "Error", note = paste("The table",note_txt,"is empty because of previous errors."), type="error" , stringsAsFactors = F)
      log <- rbind(log_dataset,log1)
      
    }else{
      log1 <- data.frame(step = "Error", note = paste("After",note_txt,"verification, all data have been deleted from the dataset."), type="error" , stringsAsFactors = F)
      log <- rbind(log_dataset,log1)
    }
  }
  else{
    log <- log_dataset
  }
  
  return(log)
})