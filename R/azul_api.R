## functions to build requests to HCA azul backend for HCAExplorer class



.project_content <- function(project, res)
{
    con <- httr::content(res)
    .parse_project_get(project, con)
}

.projectGet <- function(project, filter, per_page=15)
{
    url <- project@url
    project@per_page <- per_page
    project_url <- paste0(url, '/repository/projects?', filter, '&size=', per_page, '&sort=projectTitle&order=asc')
    project_res <- httr::GET(project_url)
    project <- .project_content(project, project_res)
    summary_url <- paste0(url, '/repository/summary?', filter)
    summary_res <- httr::GET(summary_url)
    summary_res <- httr::content(summary_res)
    project@totalFileSize <- summary_res$totalFileSize
    project@fileCount <- summary_res$fileCount
    project@totalCellCount <- summary_res$totalCellCount
    project@donorCount <- summary_res$donorCount
    project@specimenCount <- summary_res$specimenCount
    project@projectCount <- summary_res$projectCount
    project
}

.nextResults_HCAExplorer <- function(hca)
{
    url <- hca@url
    url <- paste0(url, '/repository/projects?', hca@current_filter, '&size=', hca@per_page,
        '&sort=projectTitle&order=asc&search_after=', hca@search_after,
        '&search_after_uid=', hca@search_after_uid)
    res <- httr::GET(url)
    project <- .project_content(hca, res)
    project
}

#' Next Results
#'
#' @description Fetch the next set of entries from a HCAExplorer Object
#'
#' @param hca An HCAExplorer object
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
#' hca <- hca %>% nextResults
#' hca
#'
#' @export
setMethod("nextResults", "HCAExplorer", .nextResults_HCAExplorer)
