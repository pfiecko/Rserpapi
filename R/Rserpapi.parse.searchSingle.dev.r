# Rserpapi.parse.searchSingle.dev.r
## Parse single JSON ####

#' Parse output from srpSearchSingle.json
#'
#' @param data Output from srpSearchSingle.json

Rserpapi.parse.searchSingle.dev <- function(data) {

  # Elements of serpapi response

  elements <- c("search_metadata", "search_parameters", "search_information", "related_questions", "related_searches", "organic_results", "ads")

  # Schemas of serpapi elements response

  schemas <- list("organic_results" = c("position", "title", "link", "displayed_link", "snippet", "sitelinks", "cached_page_link", "related_pages_link", "rich_snippet", "date"),
                  "related_questions" = c("question", "snippet", "title", "link", "displayed_link"),
                  "related_searches" = c("query", "link"),
                  "ads" = c("position", "block_position", "title", "link", "displayed_link", "tracking_link", "description",  "sitelinks"),
                  "shopping_results" = c("position", "title", "price", "link", "source", "thumbnail"),
                  "local_results" = c("position", "title", "reviews", "price", "type", "address", "phone", "hours", "thumbnail"),
                  "search_metadata" = c("id", "status", "created_at", "processed_at", "google_url", "total_time_taken"),
                  "search_parameters" = c("q", "google_domain", "device"),
                  "search_information" = c("total_results", "time_taken_displayed", "query_displayed"))

  # Function to parse the response element. If there is no element returns empty data frame based on element schema.

  asDataFrameResult <- function(x) {

    # Function to prepare empty data frame based on given schema. Uses "schemas" definitions from above".

    emptyDataFrameFromSchemaVector <- function(x) {
      noCols <- length(schemas[[x]])
      firstRow <- replicate(noCols, NA, simplify = "vector")
      outputDf <- as.data.frame(t(firstRow))
      names(outputDf) <- schemas[[x]]
      return(outputDf)
    }

    if (is.null(data[[x]])) {
      return(emptyDataFrameFromSchemaVector(x))
    } else {
      return(as.data.frame(data[[x]]))
    }
  }

  # Function to add results type column to element response based on element name.

  addResultsTypeToDf <- function(x) {
    x$results_type <- deparse(substitute(x))
    return(x)
  }

  # Function to convert all columns in response to strings. If column has nested data frames it converts them to JSON and saves as text string.

  allColsToString <- function(x) {
    x %>%
      dplyr::mutate_if(is.data.frame, jsonlite::toJSON) %>%
      dplyr::mutate_all(as.character) %>%
      return()
  }

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

##  names(output) <- colNamesDictionary$prettyNames

  return(output)

}
