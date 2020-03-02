# tests.r


test_sss.j.p <- srpSearchSingle.json.parse(test_sss.j)

test_sss.j <- srpSearchSingle.json.get("bookah")
test_sss.j.p <- srpSearchSingle.json.parse(test_sss.j)

colNamesDictionary <- tibble::tibble(dirtyNames = names(test_sss.t),
                                     prettyNames = outputColumnNames)
outputColumnNames <- c("serpMetaId", "serpMetaStatus", "serpMetaCreatedAt", "serpMetaProcessedAt", "serpMetaGoogleUrl", "serpMetaTotalTimeTaken", "serpQueryKeyword", "serpQueryGoogleDomain", "serpQueryDevice", "serpQueryTotalResults", "serpQueryTotalTimeTakenDisplayed", "serpQueryQueryDisplayed", "serpResultsQuestion", "serpResultsSnippet", "serpResultsTitle", "serpResultsLink", "serpResultsDisplayedLink", "serpResultsResultsType", "serpResultsRelatedQuery", "serpResultsPosition", "serpResultsSitelinks", "serpResultsCachedPageLink", "serpResultsRelatedPagesLink", "serpResultsRichSnippet", "serpResultsDate", "serpResultsAdsBlockPosition", "serpResultsAdsTrackingLink", "serpResultsAdsDescription", "serpResultsPrice", "serpResultsLocalSource", "serpResultsLocalThumbnail", "serpResultsLocalReviews", "serpResultsLocalType", "serpResultsLocalAddress", "serpResultsLocalPhone", "serpResultsLocalHours")

names1 <- names(test_sss.t)
names2 <- names(test_sss.t2)
namesDiff <- setdiff(names1, names2)
