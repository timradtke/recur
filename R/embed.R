#' Embed the recurrence table via UMAP
#' 
#' This function takes the recurrence table version of a set of time series
#' to embed the time series via [Uniform Manifold Approximation and Projection](https://umap-learn.readthedocs.io/en/latest/#)
#' (UMAP). 
#' This only provides access to the naive settings of the [umap::umap] package.
#' 
#' @param object The recurrence object returned by [measure()]. If not provided,
#'     `data` will be used to first compute the recurrence object before it is
#'     embedded.
#' @param data A dataframe with columns "id", "date", "value". See [measure()]
#'     for more information. If provided while `object` is left empty, the
#'     recurrence table is computed internally via [measure()] first.
#' @param ... Further parameters provided to [measure()].
#' @param umap_object If `TRUE` (default), the object returned by [umap::umap()] is
#'     returned as well.
#' 
#' @return 
#' @seealso [measure()], [umap::umap()], [cluster()]
#' @references
#' L. McInnes, J. Healy, J. Melville (2018). 
#' [*UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction*](https://arxiv.org/abs/1802.03426), 
#' ArXiv e-prints 1802.03426.
#' @examples 
#' times <- recur::synthetic
#' measured <- measure(data = times, size = "triangle", shape = "wide")
#' embedded <- embed(object = measured)
#' 
#' # this generates the same result as
#' embedded <- embed(data = times, size = "triangle")
#' @export
embed <- function(object = NULL, data = NULL, ...,
                  umap_object = TRUE) {
  
  if(is.null(object) & is.null(data)) 
    stop("Either `object` or `data` has to be provided.")
  
  if(!is.null(data) & is.null(object)) {
    object <- measure(data = data, ...)
  }
  
  if(object$shape != "wide") {
    object$recurrence_table <- tidyr::spread(object$recurrence_table[c("id", 
                                                                       "index", 
                                                                       "recurrence")],
                                             index, recurrence)
    object$shape <- "wide"
  }
  
  umap_fit <- umap::umap(d = as.matrix(object$recurrence_table[,-1]),
                         method = "naive")
  
  id_layout <- data.frame(id = object$recurrence_table$id,
                          x = umap_fit$layout[,1], 
                          y = umap_fit$layout[,2],
                          stringsAsFactors = FALSE)
  
  return_list <- list(embedding = id_layout)
  if(umap_object) return_list$umap <- umap_fit
  
  return(return_list)
}
