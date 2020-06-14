#' Cluster the recurrence embedding via HDBSCAN
#' 
#' This function extends the embedding by an HDBSCAN clustering. Note that the
#' `embedding` item is extended by two columns `cluster` and `membership_prob`
#' of the as optimal identified cut in the hierarchical clustering. The full
#' HDBSCAN object returned by [dbscan::hdbscan()] is returned as well.
#' 
#' @param object The recurrence embedding object returned by [embed()].
#' @param min_points The `min_points` argument of [dbscan::hdbscan()].
#' 
#' @references 
#' Leland McInnes, John Healy, Steve Astels. [The hdbscan Clustering Library](https://hdbscan.readthedocs.io). 
#' 
#' Ricardo J.G.B. Campello, Davoud Moulavi, Joerg Sander (2013).
#' [Density-Based Clustering Based on Hierarchical Density Estimates](https://link.springer.com/chapter/10.1007/978-3-642-37456-2_14),
#' PAKDD 2013, Part II, LNAI 7819, pp. 160-172.
#' 
#' @seealso [measure()], [embed()], [dbscan::hdbscan()]
#' 
#' @examples 
#' times <- recur::synthetic
#' measured <- measure(data = times, size = "triangle", shape = "wide")
#' embedded <- embed(object = measured)
#' embedded_and_clustered <- cluster(embedded, min_points = 25)
#' 
#' plot(embedded_and_clustered$embedding$x, 
#'      embedded_and_clustered$embedding$y,
#'      col = 1 + embedded_and_clustered$embedding$cluster)
#'      
#' plot(embedded_and_clustered$hdbscan)
#' 
#' @export
cluster <- function(object, min_points = 10) {
  
  hdbscan_fit <- dbscan::hdbscan(object$embedding[c("x", "y")], 
                                 minPts = min_points,
                                 gen_hdbscan_tree = TRUE)
  
  object$embedding$cluster <- hdbscan_fit$cluster
  object$embedding$membership_prob <- hdbscan_fit$membership_prob
  object$hdbscan <- hdbscan_fit
  
  return(object)
}