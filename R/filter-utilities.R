
.range_ops = list(
    '<' = "lt",
    '<=' = "lte",
    '>' = 'gt',
    '>=' = 'gte'
)

.regexp_ops = c('contains', 'startsWith', 'endsWith')

.range <- c('<', '<=', '>', '>=')

.match_ops = list(
    '==' = '='
)

.project_fields <- function(hca)
{
    names(hca@terms)
}

#' List supported fields of an HCAExplorer object
#'
#' @return A tibble indicating fields that can be queried upon.
#'
#' @name fields
#' @aliases fields,HCAExplorer-method
#' @docType methods
#'
#' @examples
#' hca <- HCAExplorer()
#' hca %>% fields
#'
#' @export
setMethod("fields", "HCAExplorer", .project_fields)

.project_values <- function(x, fields)
{
    term <- x@terms
    field <- term[[fields]]
    field <- unlist(field)
    field <- head(field, -3)
    uu <- matrix(field, nrow = 2)
    uu <- t(uu)
    uu <- as.data.frame(uu)
    names(uu) <- c('value', 'hits')
    as_tibble(uu)
}

#' List all values for certain fields in a HCAExplorer Object
#'
#' @param x A HCAExplorer Object.
#' @param fields a character vector of fields to display avaiable values for.
#' @param ... Other arguments.
#'
#' @return a list of possible values for a filter
#'
#' @examples
#' hca <- HCAExplorer()
#' vals <- hca %>% values('organ')
#' vals
#'
#' @importFrom S4Vectors values
#' @export
setMethod("values", "HCAExplorer", .project_values)

.binary_op_project <- function(sep)
{
    force(sep)
    function(e1, e2) {
        field <- as.character(substitute(e1))
        value <- as.character(substitute(e2))
        
        fun <- "is"

        leaf <- list(value)

        names(leaf) <- fun
        leaf <- list(leaf)
        names(leaf) <- field
        leaf
    }
}

.combine_op_project <- function(sep)
{
    force(sep)
    function(e1, e2) {
        
        con <- list(e1, e2)
        con
    }
}

.project_filter_loop <- function(li, expr)
{
    res <- rlang::eval_tidy(expr, data = .LOG_OP_REG_PROJECT)
    res
}

.project_temp <- function(dots)
{
    res <- Reduce(.project_filter_loop, dots, init = list())
    res
}

#' @importFrom curl curl_escape
#' @export
filter.HCAExplorer <- function(.data, ..., .preserve)
{
    dots = quos(...)
    if (length(dots) == 0) {
        project <- .data
        ret <- paste0('filter=', curl::curl_escape('{}'))
        project@current_filter <- ret
        projectGet(project, ret)
    }
    else {
        project <- .data
        es_query <- c(project@es_query, dots)
        search_term <- Reduce(.project_filter_loop, es_query, init = list())
        project@es_query <- es_query
        project@search_term <- search_term
        ret <- paste0('filters=', curl::curl_escape(jsonlite::toJSON(search_term)))
        project@current_filter <- ret
        projectGet(project, ret)
    }
}

.LOG_OP_REG_PROJECT <- list()
.LOG_OP_REG_PROJECT$`==` <- .binary_op_project("==")
.LOG_OP_REG_PROJECT$`%in%` <- .binary_op_project("==")
.LOG_OP_REG_PROJECT$`&` <- .combine_op_project("&")

`%startsWith%` <- function(e1, e2){}
`%endsWith%` <- function(e1, e2){}
`%contains%` <- function(e1, e2){}

