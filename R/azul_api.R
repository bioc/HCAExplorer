## functions to build requests to HCA azul backend for HCAExplorer class



.project_content <- function(project, res)
{
    con <- httr::content(res)
    .parse_project_get(project, con)
}

projectGet <- function(project, filter, per_page=15)
{
    browser()
    url <- project@url
    project@per_page=per_page
    url <- paste0(url, '/projects?', filter, '&size=', per_page, '&sort=projectTitle&order=asc')
    res <- httr::GET(url)
    project <- .project_content(project, res)
    project
}

.nextResults_HCAExplorer <- function(result)
{
    url <- result@url
    url <- paste0(url, '/projects?', result@current_filter, '&size=', result@per_page,
        '&sort=projectTitle&order=asc&search_after=', result@search_after,
        '&search_after_uid=', result@search_after_uid)
    res <- httr::GET(url)
    project <- .project_content(result, res)
    project
}

#' Next Results
#'
#' Fetch the next set of bundles from a HCAExplorer Object
#'
#' @return A HCAExplorer object that displays the next results
#'
#' @author Daniel Van Twisk
#'
#' @name nextResults
#' @aliases nextResults,HCAExplorer-method
#' @docType methods
#' 
#' @examples
#'
#' hca <- HCAExplorer()
#' hca <- nextResults(hca)
#' hca
#'
#' @export
setMethod("nextResults", "HCAExplorer", .nextResults_HCAExplorer)
