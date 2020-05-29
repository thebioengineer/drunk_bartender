library(keras)
library(tidyverse)
library(abind)
library(stringi)
library(tokenizers)

values <- readRDS("Drunk_Params.RDS")
cocktail_model <- load_model_hdf5("name_and_contents_trained_saved_cpu.h5")

whats_in_my_cocktail <- function(cocktail_name = "Gin Sling", diversity = .8, guessmax = 300){
  
  sample_mod <- function(preds, temperature = 1){
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp(preds))
    
    rmultinom(1, 1, preds) %>% 
      as.integer() %>%
      which.max()
  }
  
  tokenized_drink <- tokenize_characters(
    trimws(tolower(cocktail_name)),
    strip_non_alphanum = FALSE,
    simplify = FALSE)[[1]] %>% 
    c(":")
  
  splitSeed <- c(rep("<pad>",values$npad_drink-length(tokenized_drink)),
                 tokenized_drink)
  
  generated <- paste0(cocktail_name,":")
  next_char <- ""
  
  reviewChars<-0
  
  while(next_char!="ENDOFNAME"){
    
    x <- data.frame(
      sapply(values$Inputs, function(x){as.integer(x == splitSeed)})
    ) %>% 
      as.matrix
    
    x <- tensorflow::array_reshape(x, c(1, dim(x)))
    
    preds <-
      try(predict(cocktail_model, x))
    
    next_index <- sample_mod(preds, diversity)
    next_char <- values$Outputs[next_index]
    
    if(next_char != "ENDOFNAME"){
      
      generated <- str_c(generated, next_char, collapse = "")
      splitSeed <- c(splitSeed[-1], next_char)
      
      reviewChars<-reviewChars+1
    }
    
    if(reviewChars > guessmax){
      break
    }
  }
  
  return(generated)
}