source("bartender.R")

# Set an endpoint to return a pet name
#* @param name cocktail name to generate recipe for
#* @get /cocktail
get_cocktail <- function(name = ""){
  whats_in_my_cocktail(name)
}
