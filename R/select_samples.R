#' Select Samples for Population Genomics Studies
#'
#' This function selects samples from ranked metadata based on cluster constraints,
#' budget limitations, and optional priority clusters. It ensures sequenced samples
#' are always included and applies cluster-specific maximums when specified.
#'
#' @param ranked_meta A data frame containing ranked metadata (output from \code{rank_samples()}).
#'   Must contain columns: cluster ID column (specified by \code{pop_col}), \code{is_sequenced} (logical),
#'   \code{cluster_rank} (numeric), and \code{SampleID} (unique identifier).
#' @param max_samples_per_cluster Integer. Default maximum number of samples to select
#'   per cluster. Default is 8.
#' @param min_samples_per_cluster Integer. Minimum number of samples required to keep
#'   a cluster in the final selection. Clusters below this threshold are removed.
#'   Default is 4.
#' @param total_budget Integer or NULL. Total number of samples to select across all
#'   clusters. If NULL, no budget constraint is applied. Default is NULL.
#' @param cluster_specific_max Named list or NULL. Specify higher (or lower) maximums
#'   for specific clusters. Names should match cluster IDs in the cluster column.
#'   Example: \code{list("Cluster_5" = 10, "Cluster_12" = 10)}. Default is NULL.
#' @param protected_clusters Character vector or NULL. Cluster IDs that should not
#'   have samples removed when enforcin budget constraint. Useful when there are specific target populations with higher desired sample sizes. Default is NULL.
#' @param pop_col Character string specifying the column name for population/cluster
#'   identification. Default is "Pop".
#'
#' @return A data frame containing the selected samples with all original metadata columns.
#'
#' @details
#' The function follows this workflow:
#' \enumerate{
#'   \item Always includes all sequenced samples (these are never removed)
#'   \item Selects unsequenced samples up to cluster-specific or default maximums
#'   \item Removes clusters that fall below the minimum sample threshold
#'   \item If total_budget is specified, removes samples from non-protected clusters
#'         starting with the largest clusters first, removing worst-ranked samples
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage (all clusters treated equally)
#' selected <- select_samples(ranked_meta,
#'                           max_samples_per_cluster = 8,
#'                           min_samples_per_cluster = 4,
#'                           total_budget = 200,
#'                           pop_col = "Pop")
#'
#' # With priority clusters
#' selected <- select_samples(ranked_meta,
#'                           max_samples_per_cluster = 8,
#'                           cluster_specific_max = list("Cluster_5" = 10,
#'                                                      "Cluster_12" = 10),
#'                           protected_clusters = c("Cluster_5", "Cluster_12"),
#'                           pop_col = "Pop")
#' }
#'
#' @importFrom dplyr filter bind_rows count left_join mutate group_by row_number
#'   ungroup arrange slice_head pull select
#' @importFrom tidyr replace_na
#' @export
select_samples <- function(ranked_meta,
                           max_samples_per_cluster = 8,
                           min_samples_per_cluster = 4,
                           total_budget = NULL,
                           cluster_specific_max = NULL,
                           protected_clusters = NULL,
                           pop_col = "Pop") {

  # always select ALL sequenced samples first (these will always be included as they have already been sequenced)
  sequenced_samples <- ranked_meta %>%
    filter(is_sequenced == TRUE)

  cat("Selected", nrow(sequenced_samples), "sequenced samples (always included)\n")

  # calculate remaining budget for unsequenced samples
  remaining_budget <- if (!is.null(total_budget)) {
    max(0, total_budget - nrow(sequenced_samples))
  } else {
    NULL
  }

  if (!is.null(total_budget)) {
    cat("Remaining budget for unsequenced samples:", remaining_budget, "\n")
  }

  # select unsequenced samples between min and max allowed
  unsequenced_candidates <- ranked_meta %>%
    filter(is_sequenced == FALSE)

  if (nrow(unsequenced_candidates) == 0) {
    cat("No unsequenced samples available\n")
    return(sequenced_samples)
  }

  # initial selection of unsequenced samples by cluster
  # apply cluster-specific maximums if provided
  if (!is.null(cluster_specific_max)) {
    # split by cluster and apply limits
    cluster_list <- split(unsequenced_candidates, unsequenced_candidates[[pop_col]])
    
    unsequenced_selected <- lapply(names(cluster_list), function(cluster_id) {
      cluster_data <- cluster_list[[cluster_id]]
      n_to_select <- if (cluster_id %in% names(cluster_specific_max)) {
        cluster_specific_max[[cluster_id]]
      } else {
        max_samples_per_cluster
      }
      cluster_data %>%
        arrange(cluster_rank) %>%
        slice_head(n = n_to_select)
    }) %>%
      bind_rows()
  } else {
    unsequenced_selected <- unsequenced_candidates %>%
      group_by(.data[[pop_col]]) %>%
      arrange(cluster_rank) %>%
      slice_head(n = max_samples_per_cluster) %>%
      ungroup()
  }

  # account for sequenced samples in max_samples_per_cluster limit
  sequenced_per_cluster <- sequenced_samples %>%
    count(.data[[pop_col]], name = "sequenced_count")

  # split by cluster and apply adjusted limits
  cluster_list <- split(unsequenced_selected, unsequenced_selected[[pop_col]])

  unsequenced_selected <- lapply(names(cluster_list), function(cluster_id) {
    cluster_data <- cluster_list[[cluster_id]]
    
    # get sequenced count for this cluster
    seq_count <- sequenced_per_cluster %>%
      filter(.data[[pop_col]] == cluster_id) %>%
      pull(sequenced_count)
    
    if (length(seq_count) == 0) seq_count <- 0
    
    # determine max for this cluster
    cluster_max <- if (!is.null(cluster_specific_max) && cluster_id %in% names(cluster_specific_max)) {
      cluster_specific_max[[cluster_id]]
    } else {
      max_samples_per_cluster
    }
    
    # calculate how many unsequenced we can keep
    n_unsequenced_allowed <- max(0, cluster_max - seq_count)
    
    cluster_data %>%
      slice_head(n = n_unsequenced_allowed)
  }) %>%
    bind_rows()

  # combine sequenced and unsequenced before checking min cluster sizes
  all_selected <- bind_rows(sequenced_samples, unsequenced_selected)

  # check if min cluster size is met
  cluster_sizes <- all_selected %>%
    count(.data[[pop_col]], name = "selected_count")

  clusters_below_min <- cluster_sizes %>%
    filter(selected_count < min_samples_per_cluster) %>%
    pull(.data[[pop_col]])

  if (length(clusters_below_min) > 0) {
    cat("Removing", length(clusters_below_min), "clusters with fewer than",
        min_samples_per_cluster, "samples:\n")
    print(clusters_below_min)

    # remove unsequenced samples from clusters below the min
    # sequenced samples are always kept
    unsequenced_selected <- unsequenced_selected %>%
      filter(!.data[[pop_col]] %in% clusters_below_min)

    # update combined selection
    all_selected <- bind_rows(sequenced_samples, unsequenced_selected)
  }

  # apply budget constraint to unsequenced samples only
  if (!is.null(remaining_budget) && nrow(unsequenced_selected) > remaining_budget) {

    samples_to_remove <- nrow(unsequenced_selected) - remaining_budget
    cat("Need to remove", samples_to_remove, "unsequenced samples\n")

    removed_count <- 0

    while (removed_count < samples_to_remove && nrow(unsequenced_selected) > 0) {

      # Ccunt current samples per cluster
      current_cluster_sizes <- bind_rows(sequenced_samples, unsequenced_selected) %>%
        count(.data[[pop_col]], name = "total_count")

      # rind cluster with most total samples that can lose unsequenced samples
      # and exclude protected clusters from removal consideration
      clusters_with_unsequenced <- unsequenced_selected %>%
        count(.data[[pop_col]], name = "unsequenced_count") %>%
        left_join(current_cluster_sizes, by = pop_col) %>%
        # only remove if cluster will still meet minimum after removal
        filter(total_count > min_samples_per_cluster) %>%
        # exclude protected clusters
        filter(if (!is.null(protected_clusters)) !.data[[pop_col]] %in% protected_clusters else TRUE) %>%
        arrange(desc(total_count)) %>%
        slice_head(n = 1) %>%
        pull(.data[[pop_col]])

      if (length(clusters_with_unsequenced) == 0) {
        cat("Unable to remove more unsequenced samples while maintaining minimum cluster size")
        if (!is.null(protected_clusters)) {
          cat(" (protected clusters excluded from removal)")
        }
        cat("\n")
        break
      }

      target_cluster <- clusters_with_unsequenced[1]

      # remove the worst ranked unsequenced sample
      sample_to_remove <- unsequenced_selected %>%
        filter(.data[[pop_col]] == target_cluster) %>%
        arrange(desc(cluster_rank)) %>%
        slice_head(n = 1)

      unsequenced_selected <- unsequenced_selected %>%
        filter(SampleID != sample_to_remove$SampleID)

      removed_count <- removed_count + 1
    }

    cat("Removed", removed_count, "unsequenced samples\n")
  }

  # combine sequenced + selected unsequenced
  selected_samples <- bind_rows(sequenced_samples, unsequenced_selected)

  return(selected_samples)
}
