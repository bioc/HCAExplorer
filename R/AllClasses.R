
.init_HCAExplorer <- function(project)
{
    res <- filter(project)
    res
}

setOldClass('quosure')
setOldClass('quosures')

#' The Project Browser Class
#'
#' @description A still tentative class that displays Human Cell Atlas
#'  information by projects.
#'
#' @param url character(1) The url of the Human Cell Atlas.
#'
#' @author Daniel Van Twisk
#'
#' @exportClass HCAExplorer
.HCAExplorer <- setClass("HCAExplorer",
    slots = c(
        url = "character",
        results = "tbl_df",
        activated = 'character',
        es_query = "quosures",
        es_source = "quosures",
        search_term = "list",
        per_page = "numeric",
        current_filter = "character",
        terms = "list",
        search_after = "character",
        search_after_uid = "character"
    )
)

#' The Project Browser Class
#'
#' @description A still tentative class that displays Human Cell Atlas
#'  information by projects.
#'
#' @param per_page the number of results to display per request.
#' @param url character(1) The url of the Human Cell Atlas.
#'
#' @author Daniel Van Twisk
#'
#' @return a HCAExplorer object
#'
#' @examples
#' pb <- HCAExplorer()
#' pb
#' @export
HCAExplorer <-
    function(url='https://service.explore.data.humancellatlas.org/repository/projects',
             per_page = 15)
{
    project <- .HCAExplorer(url=url, per_page = per_page, results=tibble(),
                               es_query = quos(), es_source=quos(), activated = 'projects')
    .init_HCAExplorer(project)
}
             
.priv_results <- function(object) object@results

setGeneric('results', function(object, ...) standardGeneric('results'))

.retrieve_results <-
    function(object)
{
    res <- object@results@results
    if (nrow(res) > 0 && object@activated == 'bundles')
        res <- res %>% distinct(bundle_fqid, .keep_all = TRUE)
    res
}

.project_results <- function(object)
{
    res <- object@results
    if(object@activated == "projects")
        sel <- c('projects.projectTitle', 'samples.sampleEntityType', 'samples.organ', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'samples.disease')
    if(object@activated == "samples")
        sel <- c('samples.id', 'projects.projectTitle', 'samples.sampleEntityType', 'samples.organ', 'samples.organPart', 'cellSuspensions.selectedCellType', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'donorOrganisms.organismAge', 'donorOrganisms.biologicalSex', 'samples.disease')
    if(object@activated == "files")
        sel <- c('samples.id', 'samples.sampleEntityType', 'samples.organ', 'samples.organPart', 'cellSuspensions.selectCellType', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'donorOrganisms.organismAge', 'donorOrganism.biologicalSex', 'samples.disease')
    select(res, sel)
}

#' Obtain search results from a HCAExplorer Object
#'
#' @description
#'  Returns a tibble either showing bundles or files based on whichever is
#'  activated.
#'
#' @return a tibble
#'
#' @name results
#' @aliases results,HCAExplorer-method
#' @docType methods
#'
#' @export
#' @importFrom dplyr distinct
setMethod('results', 'HCAExplorer', .project_results)

#' @export
setGeneric('undoEsQuery', function(hca, ...) standardGeneric('undoEsQuery'))
setGeneric('resetEsQuery', function(hca, ...) standardGeneric('resetEsQuery'))

setGeneric('per_page', function(hca, ...) standardGeneric('per_page'))

setGeneric('pullBundles', function(hca, ...) standardGeneric('pullBundles'))
setGeneric('pullFiles', function(hca, ...) standardGeneric('pullFiles'))
setGeneric('showBundles', function(hca, bundle_fqids, ...) standardGeneric('showBundles'))
setGeneric('activate', function(hca, ...) standardGeneric('activate'))
setGeneric('getProjects', function(hca, ...) standardGeneric('getProjects'))
setGeneric('showProject', function(hca, ...) standardGeneric('showProject'))
setGeneric('pullProject', function(hca, ...) standardGeneric('pullProject'))

.project_selections <-
    c('project_json.project_core.project_title',
      'project_json.project_core.project_short_name',
      'specimen_from_organism_json.organ.text',
      'library_preparation_protocol.library_construction_approach.text',
      'specimen_from_organism_json.genus_species.text',
      'disease.text'
    )

.showProject <-
    function(hca, project)
{
    browser()
    res <- hca@results
    entryId <- as.character(res[['entryId']][[project]])
    url <- paste0(hca@url, '/', entryId)
    res <- httr::GET(url)
    res <- httr::content(res)
    
    cat('\nProject Title\t', as.character(res[,"projects.projectTitle"]), '\n')
    cat('\n')
    cat('Project Details\n')
    cat('Project Label\t\t\t', as.character(res[,"projects.projectShortname"]), '\n')
    cat('Species\t\t\t\t', as.character(res[,"donorOrganisms.genusSpecies"]), '\n')
    cat('Organ\t\t\t\t', as.character(res[,"specimens.organ"]), '\n')
    cat('Organ Part\t\t\t', as.character(res[,"specimens.organPart"]), '\n')
    cat('Known Diseases (Specimens)\t', as.character(res[,"specimens.disease"]), "\n")
    cat('Library Construction Approach\t', as.character(res[,"protocols.libraryConstructionApproach"]), "\n")
    cat('Paired End\t\t\t', as.character(res[, "protocols.pairedEnd"]), "\n")
    cat('File Type\t\t\t', as.character(res[,"fileTypeSummaries.fileType"]), "\n")
    cat('Cell Count Estimate\t\t', as.character(res[,"fileTypeSummaries.count"]), "\n")
    
    cat("\nDescription\n")
    cat(as.character(hca[,"project_json.project_core.project_description"]), "\n")

    cat("\n")

#    cat('Publications\t\t', as.character(hca[,'publication.publication_title']), "\n")
    cat('Laboratory\t\t\t', as.character(res[,'projects.laboratory']), "\n")
    
}

setMethod('showProject', 'HCAExplorer', .showProject)

.pullProject_project <- function(hca, project, n)
{
    hca <- as.data.frame(hca@results[project,])
    pj <- as.character(hca[,"projects.projectTitle"])
    hh <- HCABrowser()
    hh %>% filter("project_title" == pj) %>% pullBundles(n)
}

setMethod('pullProject', 'HCAExplorer', .pullProject_project)

.activate.HCAExplorer <-
    function(hca, what = c('projects', 'samples', 'files'))
{
    type <- match.arg(what)
    if(type == 'projects') {
        hca@url <- 'https://service.explore.data.humancellatlas.org/repository/projects'
        hca@activated <- 'projects'
    }
    else if (type == 'samples') {
        hca@url <- 'https://service.explore.data.humancellatlas.org/repository/samples'
        hca@activated <- 'samples'
    }
    else if (type == 'files') {
        hca@url <- 'https://service.explore.data.humancellatlas.org/repository/files'
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
#' @importFrom tidygraph activate
#' @export
setMethod('activate', 'HCAExplorer', .activate.HCAExplorer)

.undo_esquery <-
    function(hca, n = 1L)
{
    check <- length(hca@es_query) - n
    hca@search_term <- list()
    if (check < 1)
        resetEsQuery(hca)
    else{
        hca@es_query <- head(hca@es_query, -c(n))
        hca@es_source <- head(hca@es_source, -c(n))
        filter(hca)
    }
}

.show_HCAExplorer <- function(object)
{
    cat('class:', class(object), '\n')
    cat('Using azul backend at:\n ', object@url, '\n\n')
    cat('Showing', object@activated, 'with', object@per_page ,'results per page\n')
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
