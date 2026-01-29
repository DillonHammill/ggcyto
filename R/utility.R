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
#' This function converts specified pData columns to factors with custom level ordering,
#' allowing users to control the order in which samples appear in faceted plots.
#' By default, pData variables are stored as strings and are ordered alphabetically.
#' This function provides a clean way to set a custom plotting order.
#' 
#' @param data A flowSet, ncdfFlowSet, or GatingSet object
#' @param ... Named arguments where the name is the pData column and the value is 
#'   a character vector specifying the desired order of levels. If the value is NULL,
#'   the column will be converted to a factor with levels in the order they appear.
#' 
#' @return The input object with modified pData where specified columns are converted to factors
#' 
#' @details
#' The function modifies the pData slot of the input object by converting specified
#' columns to factors with custom level ordering. This affects how the data will be
#' ordered in faceted plots (e.g., when using \code{facet_grid} or \code{facet_wrap}).
#' 
#' If a column value is provided as NULL, the function will use the unique values
#' in the order they appear in the data.
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
#' # Convert to factor with automatic ordering (order of appearance)
#' fs <- set_pdata_order(fs, Patient = NULL)
#' }
#' 
#' @export
set_pdata_order <- function(data, ...) {
  # Get the pData
  pd <- pData(data)
  
  # Get the arguments
  args <- list(...)
  
  if (length(args) == 0) {
    stop("At least one column must be specified")
  }
  
  # Process each argument
  for (col_name in names(args)) {
    if (!col_name %in% colnames(pd)) {
      stop("Column '", col_name, "' not found in pData")
    }
    
    levels_order <- args[[col_name]]
    
    if (is.null(levels_order)) {
      # Use unique values in order of appearance
      pd[[col_name]] <- factor(pd[[col_name]], levels = unique(pd[[col_name]]))
    } else {
      # Use provided order
      # Check if all values in the data are in the provided levels
      missing_levels <- setdiff(unique(pd[[col_name]]), levels_order)
      if (length(missing_levels) > 0) {
        warning("Some values in '", col_name, "' are not in the provided levels: ",
                paste(missing_levels, collapse = ", "),
                ". These will be converted to NA.")
      }
      
      pd[[col_name]] <- factor(pd[[col_name]], levels = levels_order)
    }
  }
  
  # Set the modified pData back
  pData(data) <- pd
  
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
