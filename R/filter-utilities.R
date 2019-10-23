
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
#' @param hca An HCAExplorer object
#'
#' @name fields
#' @aliases fields,HCAExplorer-method
#' @docType methods
#'
#' @examples
#' hca <- HCAExplorer()
#' hca %>% fields
#'
#' @importFrom utils head
#' @importFrom tidygraph as_tibble
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
#' hca %>% values('organ')
#'
#' @importFrom S4Vectors values
#' @export
setMethod("values", "HCAExplorer", .project_values)

.binary_op_project <- function(sep)
{
    force(sep)
    function(e1, e2) {
        field <- as.character(substitute(e1))
        value <- try({
            e2
        }, silent = TRUE)
        if (inherits(value, "try-error")) {
            value <- as.character(substitute(e2))
            if(value[1] == 'c')
                value <- value[-1]
            value
        }
        
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
    if (length(res) > 1)
        res <- unlist(res, recursive = FALSE)
    c(li, res)
}

#' Filter an HCAExplorer object using fields and values
#' 
#' @description Given some amount of fields and values associated with them,
#'  modify the search performed by the HCAExplorer object. This function adds
#'  terms to the query that is ultimately performed.
#' 
#' @param .data An HCAExplorer object to query.
#' @param ... Any number of fields and values to queried against. The binary
#'  operators '==' and '%in%' are allowed while only the combination operator
#'  '&' is allowed. See examples.
#' @param .preserve Unused argument
#'
#' @return An HCAExplorer object with the desired query performed.
#'
#' @examples
#'  hca <- HCAExplorer()
#'  hca %>% fields
#'  hca %>% values('organ')
#'  hca %>% values('disease')
#'  ## Perform query (organ is 'blood' OR 'brain') AND disease is 'normal'.
#'  hca %>% filter(organ == c('blood', 'brain') & disease == 'normal')
#'  ## Also
#'  hca %>% filter(organ == c('blood', 'brain'), disease == 'normal')
#'
#' @importFrom curl curl_escape
#' @importFrom dplyr filter
#' @importFrom jsonlite toJSON
#' @export
filter.HCAExplorer <- function(.data, ..., .preserve)
{
    dots = quos(...)
    project <- .data
    if (length(dots) == 0 && length(project@query) == 0) {
        ret <- paste0('filters=', curl::curl_escape('{}'))
        project@current_filter <- ret
        .projectGet(project, ret)
    }
    else {
        query <- c(project@query, dots)
        search_term <- Reduce(.project_filter_loop, query, init = list())
#        if (length(search_term) > 1)
#            search_term <- unlist(search_term, recursive = FALSE)
        ret <- paste0('filters=', curl::curl_escape(jsonlite::toJSON(search_term)))
        project@query <- query
        project@search_term <- search_term
        project@current_filter <- ret
        .projectGet(project, ret)
    }
}

#' Filter entries by position or name
#'
#' @description This function works differently from other usages of select.
#'  This function allows the user to filter entries based on position or name.
#'  This function creates a filter targeting the entryId.
#'
#' @param .data An HCAObject to filter
#' @param ... Unused argument
#' @param projects numeric or character. Either the positions in the output show
#'  method of the HCAExplorer object or the entries' names.
#'
#' @return An HCAExplorer object with the applied filter.
#'
#' @examples
#'  hca <- HCAExplorer()
#'  hca
#'  hca %>% select(projects = c(1, 2))
#'
#' @importFrom dplyr select
#' @importFrom tidygraph %>%
#' @export
select.HCAExplorer <- function(.data, ..., projects)
{
    hca <- .data
    res <- hca@results
    if (is.character(projects)) {
        ids <- which(res[['projects.projectTitle']] == projects)
    }
    ids <- res[projects,]$entryId
    hca %>% filter(projectId == ids)
}

.LOG_OP_REG_PROJECT <- list()
.LOG_OP_REG_PROJECT$`==` <- .binary_op_project("==")
.LOG_OP_REG_PROJECT$`%in%` <- .binary_op_project("==")
.LOG_OP_REG_PROJECT$`&` <- .combine_op_project("&")

`%startsWith%` <- function(e1, e2){}
`%endsWith%` <- function(e1, e2){}
`%contains%` <- function(e1, e2){}

