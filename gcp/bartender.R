library(keras)

values <- readRDS("Drunk_Params.RDS")
cocktail_model <- load_model_hdf5("gcp/name_and_contents_multiple_widths_trained_saved_cpu.h5")

whats_in_my_cocktail <- function(cocktail_name = "Gin Sling", diversity = .8, guessmax = 300){
  
  sample_mod <- function(preds, temperature = 1){
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp(preds))
    which.max(as.integer(rmultinom(1, 1, preds)))
  }
  
  tokenized_drink <-
    c(strsplit(trimws(tolower(cocktail_name)), split = "")[[1]], ":")
  
  splitSeed <-
    c(rep("<pad>", values$npad_drink - length(tokenized_drink)),
      tokenized_drink)
  
  generated <- paste0(cocktail_name,":")
  next_char <- ""
  
  reviewChars<-0 
  
  x <- as.matrix(data.frame(
      sapply(values$Inputs, function(x){as.integer(x == splitSeed)})
    ))
  
  
  
  while(next_char!="ENDOFNAME"){
    
    x_array <- array(x, c(1, dim(x)))
    
    preds <-
      try(predict(cocktail_model, list(x_array,
                                       x_array[, seq(values$npad_drink - 39, values$npad_drink), ,drop=FALSE],
                                       x_array[, seq(values$npad_drink - 19, values$npad_drink), ,drop=FALSE]))
      )
    
    next_index <- sample_mod(preds, diversity)
    next_char <- values$Outputs[next_index]
    
    if(next_char != "ENDOFNAME"){
      
      generated <- paste0(generated, next_char)

      reviewChars<-reviewChars+1
      
      x <- x[-1, ,drop=FALSE]
      x <- rbind(x,matrix(as.numeric(values$Inputs == next_char),nrow = 1))
      
    }
    
    if(reviewChars > guessmax){
      break
    }
  }
  
  return(generated)
}


# Testing function
# whats_in_my_cocktail("Bahama mama")
