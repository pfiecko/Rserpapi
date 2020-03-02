# Rserpapi.get.SearchSingle.tidy.dev.r

#' Get single Serp API request output as tidy data frame
#'
#' @param q (Required) Parameter is the query you want to search. You can use anything that you would use in a regular Google search. (e.g., inurl:, site:, intitle:, etc.)
#' @param location (Optional) Parameter defines from where you want the search to originate. If several locations match the location requested, we'll pick the most popular one. Head to the /locations.json API if you need a more precise control. location and uule parameters can't be used together.
#' @param uule (Optional) Parameter is the Google encoded location you want to use for the search. uule and location parameters can't be used together.
#' @param google_domain (Optional) Parameter defines the Google domain to use. It defaults to google.com. Head to the Google domains for a full list of supported Google domains.
#' @param gl (Optional) Parameter defines the country to use for the Google search. It's a two-letter country code. (e.g., us for the United States, uk for United Kingdom, or fr for France) Head to the Google countries for a full list of supported Google countries.
#' @param hl (Optional) Parameter defines the language to use for the Google search. It's a two-letter language code. (e.g., en for English, es for Spanish, or fr for French) Head to the Google languages for a full list of supported Google languages.
#' @param lr (Optional) Parameter defines one or multiple languages to limit the search to. It uses lang_{two-letter language code} to specify languages and | as a delimiter. (e.g., lang_fr|lang_de will only search French and German pages)
#' @param start (Optional) Parameter defines the result offset. It skips the given number of results. It's used for pagination. (e.g., 0 (default) is the first page of results, 10 is the 2nd page of results, 20 is the 3rd page of results, etc.)
#' @param num (Optional) Parameter defines the maximum number of results to return. (e.g., 10 (default) returns 10 results, 40 returns 40 results, and 100 returns 100 results)
#' @param ijn (Optional) Parameter defines the page number for Google Images. There are 100 images per page. This parameter is equivalent to start (offset) = ijn * 100. This parameter works only for Google Images.
#' @param tbm (Optional) (to be matched) parameter defines the type of search you want to do. It can be set to: (no tbm parameter): regular Google Search, isch: Google Images API, vid: Google Videos API, nws: Google News API, shop: Google Shopping API, or any other Google service.
#' @param safe (Optional) Parameter defines the level of filtering for adult content. It can be set to active, or off (default).
#' @param nfpr (Optional) Parameter defines the exclusion of results from an auto-corrected query that is spelled wrong. It can be set to 1 to exclude these results, or 0 to include them (default).
#' @param tbs (Optional) (to be searched) parameter defines advanced search parameters that aren't possible in the regular query field. (e.g., advanced search for patents, dates, news, videos, images, apps, or text contents).
#' @param filter (Optional) Parameter defines if the filters for "Similar Results" and "Omitted Results" are on or off. It can be set to 1 (default) to enable these filters, or 0 to disable these filters.
#' @param ludocid (Optional) Parameter defines the id (CID) of the Google Google My Business listing you want to scrape.
#' @param device (Optional) Parameter defines the device to use to get the results. It can be set to desktop (default) to use a regular browser, tablet to use a tablet browser (currently using iPads), or mobile to use a mobile browser (currently using iPhones).
#' @param async (Optional) Parameter defines the way you want to submit your search to SerpApi. It can be set to false (default) to open an HTTP connection and keep it open until you got your search results, or true to just submit your search to SerpApi and retrieve them later. In this case, you'll need to use our Searches Archive API to retrieve your results. async and no_cache parameters should not be used together.
#' @param no_cache (Optional) Parameter will force SerpApi to fetch the Google results even if a cached version is already present. A cache is served only if the query and all parameters are exactly the same. Cache expires after 1h. Cached searches are free, and are not counted towards your searches per month. It can be set to false (default) to allow results from the cache, or true to disallow results from the cache. no_cache and async parameters should not be used together.
#' @param api_key (Optional) Parameter defines the SerpApi private key to use.
#' @param output (Optional) Parameter defines the final output you want. It can be set to json (default) to get a structured JSON of the results, or html to get the raw html retrieved.

Rserpapi.get.SearchSingle.tidy.dev <- function(q, location = "", uule = "", google_domain = "", gl = "", hl = "", lr ="", start = "", num = "", jin = "", tbm = "", safe = "", nfpr = "", tbs = "", filter = "", ludocid = "", device = "", async = "", no_cache = "", api_key = Rserpapi_credentials$apikey, output = "json") {

  # Get function parameter values

  urlPartParams <- c("?q", "&location", "&uule", "&google_domain", "&gl", "&hl", "&lr", "&start", "&num", "&jin", "&tbm", "&safe", "&nfpr", "&tbs", "&filter", "&ludocid", "&device", "&async", "&no_cache", "&api_key", "&output")
  usedParams <- c(urltools::url_encode(q), urltools::url_encode(location), urltools::url_encode(uule), urltools::url_encode(google_domain), urltools::url_encode(gl), urltools::url_encode(hl), urltools::url_encode(lr), urltools::url_encode(start), urltools::url_encode(num), urltools::url_encode(jin), urltools::url_encode(tbm), urltools::url_encode(safe), urltools::url_encode(nfpr), urltools::url_encode(tbs), urltools::url_encode(filter), urltools::url_encode(ludocid), urltools::url_encode(device), urltools::url_encode(async), urltools::url_encode(no_cache), urltools::url_encode(api_key), urltools::url_encode(output))

  # Make tibble and filter only non empty parameters

  params <- tibble::tibble(urlPart = urlPartParams,
                           value = usedParams) %>%
    dplyr::filter(nchar(value) > 0) %>%
    dplyr::mutate(requestPart = paste(urlPart, value, sep = "="))

  # Make request url

  requestString <- paste(params$requestPart, collapse = "", sep = "")
  request <- paste("https://serpapi.com/search.json", requestString, sep = "")

  # Get response

  response <- jsonlite::fromJSON(request)

  # Elements of serpapi response

  elements <- c("search_metadata", "search_parameters", "search_information", "related_questions", "related_searches", "organic_results", "ads")

  # Schemas of serpapi elements response

  schemas <- list("organic_results" = c("position", "title", "link", "displayed_link", "snippet", "sitelinks", "cached_page_link", "related_pages_link", "rich_snippet", "date"),
                  "related_questions" = c("question", "snippet", "title", "link", "displayed_link"),
                  "related_searches" = c("query", "link"),
                  "ads" = c("position", "block_position", "title", "link", "displayed_link", "tracking_link", "description",  "sitelinks"),
                  "shopping_results" = c("position", "title", "link", "source", "price", "rating", "reviews", "snippet", "extensions", "thumbnail"),
                  "local_results" = c("position", "title", "reviews", "price", "type", "address", "description", "extensions", "thumbnail"),
                  "search_metadata" = c("id", "status", "created_at", "processed_at", "google_url", "total_time_taken"),
                  "search_parameters" = c("q", "google_domain", "device"),
                  "search_information" = c("total_results", "time_taken_displayed", "query_displayed"))

  # 1. Function to parse the response element. If there is no element returns empty data frame based on element schema.

  asDataFrameResult <- function(x) {

    ## Inner functions ##

    ## 1. Function to prepare empty data frame based on given schema. Uses "schemas" definitions from above.
    ## x - name of element

    emptyDataFrameFromSchemaVector <- function(x) {
      noCols <- length(schemas[[x]])
      firstRow <- replicate(noCols, NA, simplify = "vector")
      outputDf <- as.data.frame(t(firstRow))
      names(outputDf) <- schemas[[x]]
      return(outputDf)
    }

    ## 2. Function to check if response data frame has proper schema and fix it if something is missing.
    ## x - name of element

    hasDataFrameProperSchema <- function(x) {
      dfCols <- names(response[[x]])
      scCols <- schemas[[x]]

      if (dfCols == scCols) {
        return(list(status = TRUE, missingCols = NA))
      } else {
        missingColumns <- setdiff(dfCols, scCols)
        return(list(status = FALSE, missingCols = missingColumns))
      }
    }

    ## 3. Fix data frame if schema is wrong

    fixDfIfSchemaWrong <- function(x) {
      checkupResult <- hasDataFrameProperSchema(x)
      if (checkupResult[["status"]] == FALSE) {
        response[[x]] %>%
          tibble::add_column(checkupResult[["missing_cols"]]) %>%
          return()
      } else {
        return(response[[x]])
      }
    }

    # Main function body

    if (is.null(response[[x]])) {
      return(emptyDataFrameFromSchemaVector(response[[x]]))
    } else {
      return(fixDfIfSchemaWrong(as.data.frame(response[[x]])))
    }

  }

  # 2. Function to add results type column to element response based on element name.

  addResultsTypeToDf <- function(x) {
    x$results_type <- deparse(substitute(x))
    return(x)
  }

  # 3. Function to convert all columns in response to strings. If column has nested data frames it converts them to JSON and saves as text string.

  allColsToString <- function(x) {
    x %>%
      dplyr::mutate_if(is.data.frame, jsonlite::toJSON) %>%
      dplyr::mutate_all(as.character) %>%
      return()
  }

  ## MAIN FUNCTION BODY ##

  # Prepare search metadata row

  search_metadata <- asDataFrameResult("search_metadata")
  search_parameters <- asDataFrameResult("search_parameters")
  search_information <- asDataFrameResult("search_information")

  meta <- search_metadata %>%
    dplyr::bind_cols(search_parameters) %>%
    dplyr::bind_cols(search_information)

  # Related questions

  related_questions <- asDataFrameResult("related_questions")
  related_questions <- addResultsTypeToDf(related_questions) %>%
    allColsToString()

  # Related searches

  related_searches <- asDataFrameResult("related_searches")
  related_searches <- addResultsTypeToDf(related_searches) %>%
    allColsToString()

  # Organic results

  organic_results <- asDataFrameResult("organic_results")
  organic_results <- addResultsTypeToDf(organic_results) %>%
    allColsToString()

  # Ads results

  ads <- asDataFrameResult("ads")
  ads <- addResultsTypeToDf(ads) %>%
    allColsToString()

  # Shopping results

  shopping_results <- asDataFrameResult("shopping_results")
  shopping_results <- addResultsTypeToDf(shopping_results) %>%
    allColsToString()

  # Local results

  local_results <- asDataFrameResult("local_results")
  local_results <- addResultsTypeToDf(local_results) %>%
    allColsToString()

  # Compile all results into one data frame

  results <- related_questions %>%
    dplyr::full_join(related_searches) %>%
    dplyr::full_join(organic_results) %>%
    dplyr::full_join(ads) %>%
    dplyr::full_join(shopping_results) %>%
    dplyr::full_join(local_results)

  # Merge meta row with results table

  output <- meta %>%
    merge(results)

  # Set proper names to output table

  #  names(output) <- colNamesDictionary$prettyNames

  return(output)

}
