
.init_HCAExplorer <- function(project)
{
    res <- filter(project)
    res
}

.results <- function(object)
{
    res <- object@results
    if(object@activated == "projects")
    sel <- c('projects.projectTitle', 'samples.sampleEntityType', 'samples.organ', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'samples.disease')
    if(object@activated == "samples")
    sel <- c('samples.id', 'projects.projectTitle', 'samples.sampleEntityType', 'samples.organ', 'samples.organPart', 'cellSuspensions.selectedCellType', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'donorOrganisms.organismAge', 'donorOrganisms.biologicalSex', 'samples.disease')
    if(object@activated == "files")
    sel <- c('samples.id', 'samples.sampleEntityType', 'samples.organ', 'samples.organPart', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'donorOrganisms.organismAge', 'samples.disease')
    hca <- select(res, sel)
}

#' Obtain search results from a HCAExplorer Object
#'
#' @description
#'  Returns a tibble either showing bundles or files based on whichever is
#'  activated.
#'
#' @param object An HCAExplorer object
#'
#' @return a tibble
#'
#' @examples
#'  hca <- HCAExplorer()
#'  hca %>% results
#'
#' @name results
#' @aliases results,HCAExplorer-method
#' @docType methods
#'
#' @importFrom dplyr distinct
#' @importFrom httr GET content
#' @export
setMethod('results', 'HCAExplorer', .results)

.viewProjects <-
function(hca)
{
    ids <- hca@results$entryId
    ret <- lapply(ids, function(id) {
        url <- paste0(hca@url, '/repository/projects/', id)
        res <- httr::GET(url)
        res <- httr::content(res)
        res
    })
    
    #    cat('\nProject Title\t', as.character(res[,"projects.projectTitle"]), '\n')
    #    cat('\n')
    #    cat('Project Details\n')
    #    cat('Project Label\t\t\t', as.character(res[,"projects.projectShortname"]), '\n')
    #    cat('Species\t\t\t\t', as.character(res[,"donorOrganisms.genusSpecies"]), '\n')
    #    cat('Organ\t\t\t\t', as.character(res[,"specimens.organ"]), '\n')
    #    cat('Organ Part\t\t\t', as.character(res[,"specimens.organPart"]), '\n')
    #    cat('Known Diseases (Specimens)\t', as.character(res[,"specimens.disease"]), "\n")
    #    cat('Library Construction Approach\t', as.character(res[,"protocols.libraryConstructionApproach"]), "\n")
    #    cat('Paired End\t\t\t', as.character(res[, "protocols.pairedEnd"]), "\n")
    #    cat('File Type\t\t\t', as.character(res[,"fileTypeSummaries.fileType"]), "\n")
    #    cat('Cell Count Estimate\t\t', as.character(res[,"fileTypeSummaries.count"]), "\n")
    
    #    cat("\nDescription\n")
    #    cat(as.character(hca[,"project_json.project_core.project_description"]), "\n")
    
    #    cat("\n")
    
    #    cat('Publications\t\t', as.character(hca[,'publication.publication_title']), "\n")
    #    cat('Laboratory\t\t\t', as.character(res[,'projects.laboratory']), "\n")
    ret
}

#' View all metadata about a selection of projects
#'
#' @description Returns a list of all the metadata from the current selection
#'  of entries in the HCAExplorer object.
#'
#' @param hca An HCAExplorer object
#'
#' @return A list of all metadata in the selected entries
#'
#' @examples
#'  hca <- HCAExplorer()
#'  view <- hca %>% viewProjects()
#'  view
#'
#' @name viewProjects
#' @aliases viewProjects,HCAExplorer-method
#' @docType methods
#'
#' @export

setMethod('viewProjects', 'HCAExplorer', .viewProjects)

.summary_filter <- function(.data, ...)
{
    dots <- quos(...)
    project <- .data
    search_term <- Reduce(.project_filter_loop, dots, init = list())
    paste0('filters=', curl::curl_escape(jsonlite::toJSON(search_term)))
}

.getManifestFileFormats <- function(hca)
{
    url <- hca@url
    res <- hca@results
    ids <- res$entryId
    query <- .summary_filter(hca, projectId == ids)
    url <- paste0(url, '/repository/summary?', query)
    res <- httr::GET(url)
    res <- httr::content(res)
    res <- as.data.frame(do.call(rbind, res$fileTypeSummaries))
    unlist(res$fileType)
}

#' Show all possible manifest file formats for current selection
#' 
#' @description Show all possible manifest file formats for the
#'  current selection of projects in the HCAExplorer object. To be
#'  used in conjunction with 'getManifest()'.
#'
#' @param hca An HCAExplorer object
#'
#' @return A character vecotr of information about possible file formats.
#'
#' @examples
#'  hca <- HCAExplorer()
#'  hca %>% getManifestFileFormats
#'
#' @name getManifestFileFormats
#' @aliases getManifestFileFormats,HCAExplorer-method
#' @docType methods
#'
#' @export
setMethod('getManifestFileFormats', 'HCAExplorer', .getManifestFileFormats)

.getManifest <- function(hca, fileFormat)
{
    url <- hca@url
    res <- hca@results
    ids <- res$entryId
    filters <- .summary_filter(hca, projectId == ids & fileFormat == fileFormat)
    url <- paste0(hca@url, '/fetch/manifest/files?', filters, '&format=tsv')
    res <- httr::GET(url)
    res <- httr::content(res)
    loc <- res$Location
    loc <- httr::content(httr::GET(loc))$Location
    httr::GET(loc, httr::timeout(60))
}

#' Obtain metadata information from an HCAExplorer object
#'
#' @description Obtain metadata infromation from an HCAExplorer object.
#'  This metadata can then be passed on to download files from other services.
#'
#' @param hca An HCAExplorer object
#' @param fileFormat character. A character vector of file formats of metadata
#'  to obtain.
#'
#' @return a tibble of metadata information
#'
#' @examples
#'  hca <- HCAExplorer()
#'  hca <- hca %>% select(projects = c(1))
#'  formats <- hca %>% getManifestFileFormats
#'  hca %>% getManifest(fileFormat = formats[1])
#'
#' @name getManifest
#' @aliases getManifest,HCAExplorer-method
#' @docType methods
#'
#' @importFrom httr timeout
#' @export
setMethod('getManifest', 'HCAExplorer', .getManifest)

.activate.HCAExplorer <-
function(hca, what = c('projects', 'samples', 'files'))
{
    type <- match.arg(what)
    if(type == 'projects') {
        #hca@url <- 'https://service.explore.data.humancellatlas.org/repository/projects'
        hca@activated <- 'projects'
    }
    else if (type == 'samples') {
        #hca@url <- 'https://service.explore.data.humancellatlas.org/repository/samples'
        hca@activated <- 'samples'
    }
    else if (type == 'files') {
        #hca@url <- 'https://service.explore.data.humancellatlas.org/repository/files'
        hca@activated <- 'files'
    }
    filter(hca)
}

#' Activate projects, samples, or files to display in the HCAExplorer Object
#'
#' @name activate
#' @aliases activate,HCAExplorer-method
#' @docType methods
#'
#' @description Choose whether to display entries by project, samples, or files.
#'  The HCAExplorer class always defaults to projects.
#'
#' @param hca An HCAExplorer object
#' @param what character(1). Either 'projects', 'samples', or 'files'.
#'
#' @return An HCAExplorer object with medified activation.
#'
#' @examples
#'  hca <- HCAExplorer()
#'  hca %>% activate('samples')
#'  hca %>% activate('files')
#'
#' @importFrom tidygraph activate
#' @export
setMethod('activate', 'HCAExplorer', .activate.HCAExplorer)

.reset_query <-
function(hca)
{
    hca@search_term <- list()
    hca@query <- quos()
    hca %>% filter()
}

#' Reset all queries performed on an object
#'
#' @description Reset all queries performed on an HCAExplorer
#'  object then return the result.
#'
#' @param hca An HCAExplorer object
#'
#' @return An HCAExplorer object with the changes applied to it.
#'
#' @examples
#'  hca <- HCAExplorer()
#'  hca <- hca %>% filter(organ == 'blood')
#'  hca <- hca %>% select(projects = c(1, 2))
#'  hca %>% resetQuery
#'
#' @name resetQuery
#' @aliases resetQuery,HCAExplorer-method
#' @docType methods
#'
#' @importFrom rlang quos
#' @export
setMethod('resetQuery', 'HCAExplorer', .reset_query)

.undo_query <-
function(hca, n = 1L)
{
    check <- length(hca@query) - n
    hca@search_term <- list()
    if (check < 1)
    resetQuery(hca)
    else{
        hca@query <- head(hca@query, -c(n))
        hca@es_source <- head(hca@es_source, -c(n))
        filter(hca)
    }
}

#' Undo one or multiple queries performed on an object
#'
#' @description Undo one or multiple queries performed on an HCAExplorer
#'  object then return the result.
#'
#' @param hca An HCAExplorer object
#' @param n integer(1). The number of queries to step back from.
#'
#' @return An HCAExplorer object with the changes applied to it.
#'
#' @examples
#'  hca <- HCAExplorer()
#'  hca <- hca %>% filter(organ == 'blood')
#'  hca <- hca %>% select(projects = c(1, 2))
#'  hca %>% undoQuery(n = 1L)
#'
#' @name undoQuery
#' @aliases undoQuery,HCAExplorer-method
#' @docType methods
#'
#' @export
setMethod('undoQuery', 'HCAExplorer', .undo_query)

.show_HCAExplorer <- function(object)
{
    cat('class:', class(object), '\n')
    cat('Using azul backend at:\n ', object@url, '\n')
    cat('\n')
    cat('Donor count:', object@donorCount, '\n')
    cat('Specimens:', object@specimenCount, '\n')
    cat('Estimated Cells:', object@totalCellCount, '\n')
    cat('Files:', object@fileCount, '\n')
    cat('File Size:', utils:::format.object_size(object@totalFileSize, "auto"), '\n')
    cat('\n')
    cat('Showing', object@activated, 'with', object@per_page ,'results per page.')
    print(results(object))
}

#' Show HCAExplorer
#'
#' @param object a HCAExplorer object to show
#'
#' @return outputs a text represntation of the object
#'
#' @importFrom methods show
#' @export
setMethod('show', 'HCAExplorer', .show_HCAExplorer)

