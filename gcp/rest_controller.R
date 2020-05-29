source("bartender.R")

# Set an endpoint to return a pet name
#* @get /cocktail
get_cocktail <- function(input_name = ""){
  whats_in_my_cocktail(input_name)
}