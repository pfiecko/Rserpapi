# Rserpapi.utils.storeCredentials.r

#' Store your SERP API credentials in global environment.
#'
#' @return Rserpapi_credentials object in your global environment holding your SERP API API key.
#'
Rserpapi.utils.storeCredentials <- function() {

  print("Welcome to Rserpapi, an R interface for serpapi.com web service. To proceed, please copy your SERP API API key and paste it below.")
  apikey <- readline()

  Rserpapi_credentials <- list(apikey = apikey)

  assign("Rserpapi_credentials", Rserpapi_credentials, envir = globalenv())

  return(print("Your credentials were successfully saved to global environment."))

}
