#' Convert a list to a data.table
#' 
#' data.table version of ldply
#' @param .data a list
#' @return a data.table
#' @noRd 
.ldply <- function (.data,  ..., .id = NA) 
{
  index <- names(.data)
  if(is.null(index)){
    index <- seq_along(.data)
    .id <- NULL
  }

  res <- .do_loop(index = index, .data = .data, ..., .id = .id)
  res <- rbindlist(res, use.names = TRUE, fill = TRUE)
  setkeyv(res, .id)
  res
}

#' @param index the index of the list, can be either character or numeric
#' @param  .fun the function to apply to each element of .data
#' @param ... other arguments passed to .fun
#' @param .id see help(ldply)
#' @noRd 
.do_loop <- function(index, .data, .fun = NULL, ..., .id = NA){

  lapply(index, function(i){
    
        dt <- .fun(.data[[i]], ...)
        dt <- as.data.table(dt)
        #append id
        if(!is.null(.id)){
          if (is.na(.id)) {
            .id <- ".id"
          }
          eval(substitute(dt[, newCol := i], list(newCol = .id)))
        }
        dt
      })
}
#' convert a flowSet to a data.table
#' @param .data flowSet
#' @noRd 
.fsdply <- function (.data, ..., .id = NA) 
{

  index <- sampleNames(.data)
  res <- .do_loop(index = index, .data = .data, ..., .id = .id)
  res <- rbindlist(res, use.names = TRUE, fill = TRUE)
  setkeyv(res, .id)
  res
}

#' Generate a marginal gate.
#' 
#' It simply constructs an boundaryFilter that removes the marginal events.
#' It can be passed directly to ggcyto constructor. See the examples for details.
#' 
#' @param fs flowSet (not used.)
#' @param dims the channels involved
#' @param ... arguments passed to \link[flowCore]{boundaryFilter}
#' @return  an boundaryFilter
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1]
#' chnls <- c("FSC-H", "SSC-H")
#' #before removign marginal events
#' summary(fs[, chnls])
#' 
#' # create merginal filter
#' g <- marginalFilter(fs, chnls)
#' g
#' 
#' #after remove marginal events
#' fs.clean <- Subset(fs, g)
#' summary(fs.clean[, chnls])
#' 
#' #pass the function directly to ggcyto
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' # with marginal events
#' ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") + geom_hex(bins = 64)
#'
#' # using marginalFilter to remove these events
#' ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+", filter = marginalFilter) + geom_hex(bins = 64)
#' 
#' @export
marginalFilter <- function(fs, dims, ...){
  boundaryFilter(x = dims, ...)
}

#' Set plotting order for pData columns
#' 
#' This function sets custom ordering for specified pData columns, allowing users
#' to control the order in which samples appear in faceted plots. By default, pData
#' variables are stored as strings and are ordered alphabetically. This function
#' stores ordering information that is applied during plot generation.
#' 
#' @param data A flowSet, ncdfFlowSet, or GatingSet object
#' @param ... Named arguments where the name is the pData column and the value is 
#'   a character vector specifying the desired order of levels. If the value is NULL,
#'   the ordering will use unique values in the order they appear in the data.
#' 
#' @return The input object with a "pdata_order" attribute containing the ordering information
#' 
#' @details
#' The function stores ordering information as an attribute on the flowSet/GatingSet object.
#' This ordering is automatically applied when the data is fortified for plotting, converting
#' the specified columns to factors with the given level ordering. This affects how the data
#' will be ordered in faceted plots (e.g., when using \code{facet_grid} or \code{facet_wrap}).
#' 
#' The original pData is not modified - only the ordering information is stored as an attribute.
#' The factor conversion happens during the fortify step when preparing data for plotting.
#' 
#' If a column value is provided as NULL, the function will store the unique values
#' in the order they appear in the data as the desired ordering.
#' 
#' @examples
#' \dontrun{
#' library(ggcyto)
#' data(GvHD)
#' fs <- GvHD[subset(pData(GvHD), Patient %in% 5:7 & Visit %in% c(5:6))[["name"]]]
#' 
#' # Set custom order for Patient column
#' fs <- set_pdata_order(fs, Patient = c("7", "6", "5"))
#' 
#' # Now plots will use this order
#' ggcyto(fs, aes(x = `FSC-H`)) + geom_histogram() + facet_grid(Patient~Visit)
#' 
#' # Set order for multiple columns
#' fs <- set_pdata_order(fs, 
#'                       Patient = c("7", "6", "5"),
#'                       Visit = c("6", "5"))
#' 
#' # Use automatic ordering (order of appearance)
#' fs <- set_pdata_order(fs, Patient = NULL)
#' }
#' 
#' @export
set_pdata_order <- function(data, ...) {
  # Get the arguments
  args <- list(...)
  
  if (length(args) == 0) {
    stop("At least one column must be specified")
  }
  
  # Validate that columns exist in pData
  pd <- pData(data)
  for (col_name in names(args)) {
    if (!col_name %in% colnames(pd)) {
      stop("Column '", col_name, "' not found in pData")
    }
    
    levels_order <- args[[col_name]]
    
    # If NULL, use unique values in order of appearance
    if (is.null(levels_order)) {
      args[[col_name]] <- unique(as.character(pd[[col_name]]))
    } else {
      # Check if all values in the data are in the provided levels
      missing_levels <- setdiff(unique(as.character(pd[[col_name]])), levels_order)
      if (length(missing_levels) > 0) {
        warning("Some values in '", col_name, "' are not in the provided levels: ",
                paste(missing_levels, collapse = ", "),
                ". These will be converted to NA.")
      }
    }
  }
  
  # Store ordering information as an attribute instead of modifying pData
  attr(data, "pdata_order") <- args
  
  return(data)
}

gg_add <- function(e1, e2, ...) {
  f <- get0("add_gg",envir = asNamespace("ggplot2"))
  if (is.function(f)) {
    f(e1, e2, ...)
  } else {
    ggplot2:::`+.gg`(e1, e2, ...)
  }
}
