#' Rank Samples Based on Type, Age, and Sequencing Status
#'
#' Ranks samples within each spatial cluster based on sample quality (type),
#' age, and whether they have already been sequenced. Already-sequenced samples
#' receive the highest priority, followed by samples ranked by quality and age.
#'
#' @param metadata A data frame containing sample metadata with spatial clusters.
#'   Must contain a column for cluster IDs (specified by \code{pop_col}).
#' @param sample_type_col Character. Name of the column containing sample type information.
#'   Sample types are ranked using the hierarchy defined in sample_type_ranking arg.
#'   Default is "SampleType".
#' @param date_col Character. Name of the column containing collection dates.
#'   Can handle multiple date formats (M/D/YYYY, M/D/YY, or standard R date formats).
#'   Default is "CollectionDate".
#' @param sequenced_col Character. Name of the column indicating if a sample has been
#'   sequenced. Should contain "Y" or "y" for sequenced samples. Default is "Sequenced".
#' @param pop_col Character. Name of the column containing cluster/population IDs.
#'   Default is "Pop".
#' @param prefer_recent Logical. If TRUE, more recent samples are ranked higher.
#'   If FALSE, older samples are ranked higher. Default is TRUE.
#' @param sample_type_ranking A data frame (optional) specifying the hierarchy of sample types.
#'   Must contain two columns:
#'     SampleType: Character vector of sample types matching the metadata column.
#'     type_rank: Numeric rank where 1 = highest priority
#'   If not provided, unsequenced samples will be ranked by age only.
#'
#' @return A data frame with the same structure as input, with additional columns:
#'   \describe{
#'     \item{type_rank}{Numeric rank based on sample type (1 = highest quality)}
#'     \item{age_years}{Age of sample in years from collection date}
#'     \item{age_rank}{Rank based on sample age}
#'     \item{is_sequenced}{Logical indicating if sample has been sequenced}
#'     \item{composite_rank}{Combined rank based on sequencing status, type, and age}
#'     \item{cluster_rank}{Final rank within each cluster (1 = highest priority)}
#'   }
#'
#' @details
#' The ranking hierarchy prioritizes samples as follows:
#' \enumerate{
#'   \item Already sequenced samples (always ranked highest & always selected)...
#'   \item Sample type e.g. Blood > Feather > Tissue > Toe pad (Bird example)
#'   \item Sample age (recent or old, depending on \code{prefer_recent})
#' }
#'
#' @examples
#' \dontrun{
#' # Basic ranking
#' ranked_data <- rank_samples(metadata,
#'                            date_col = "CollectionDate",
#'                            sequenced_col = "Sequenced",
#'                            pop_col = "Pop")
#'
#' # Define a sample type ranking table
#' sample_type_ranking <- data.frame(
#'   SampleType = c("Blood", "Feather", "Tissue", "Toe pad"),
#'   type_rank = c(1, 2, 3, 4), # 1 = highest priority
#'   stringsAsFactors = FALSE # if using R <4.0.0
#' )
#'
#' # Basic ranking using the ranking table
#' ranked_data <- rank_samples(
#'   metadata,
#'   sample_type_col = "SampleType",
#'   date_col = "CollectionDate",
#'   sequenced_col = "Sequenced",
#'   pop_col = "Pop",
#'   sample_type_ranking = sample_type_ranking
#' )
#'
#' # Prefer older samples
#' ranked_data <- rank_samples(meta, prefer_recent = FALSE)
#' }
#'
#' @importFrom dplyr left_join mutate group_by ungroup
#' @importFrom stats setNames
#' @export
rank_samples <- function(
  metadata,
  sample_type_col = "SampleType",
  date_col = "CollectionDate",
  sequenced_col = "Sequenced",
  pop_col = "Pop",
  sample_type_ranking = NULL,  # optional
  prefer_recent = TRUE
) {

    # Warning if sample type ranking is not provided
    if (is.null(sample_type_ranking) || !(sample_type_col %in% colnames(metadata))) {
      warning(
        "No sample type ranking provided or sample type column '",
        sample_type_col,
        "' not found in metadata. ",
        "Unsequenced samples will be ranked by age only, and sample type priority will be ignored. ",
        "See the `@examples` section in the documentation for how to provide a ranking table."
      )
    }

    
  # add sample type ranking only if provided
  if (!is.null(sample_type_ranking)) {
    metadata <- metadata %>%
      left_join(sample_type_ranking, by = setNames("SampleType", sample_type_col)) %>%
      mutate(
        # if sample type not found in ranking, assign lowest priority...
        # if sample type information is missing for some samples, they WILL be ranked low...
        type_rank = ifelse(is.na(type_rank), max(sample_type_ranking$type_rank) + 1, type_rank)
      )
  }

  # rank by age
  metadata <- metadata %>%
    mutate(
      age_years = sapply(get(date_col), calculate_age),
      age_rank = if (prefer_recent) {
        base::rank(age_years, ties.method = "min", na.last = TRUE)
      } else {
        base::rank(-age_years, ties.method = "min", na.last = FALSE)
      }
    )

  # within each cluster, create composite rank
  metadata <- metadata %>%
    group_by(!!as.name(pop_col)) %>%
    mutate(
      is_sequenced = ifelse(is.na(get(sequenced_col)), FALSE,
                             toupper(get(sequenced_col)) == "Y"),
      # composite rank, use type_rank only if it exists
      composite_rank = ifelse(
        is_sequenced,
        age_rank,
        if ("type_rank" %in% colnames(metadata)) {
          type_rank * 1000 + age_rank
        } else {
          age_rank
        }
      ),
      cluster_rank = base::rank(composite_rank, ties.method = "min")
    ) %>%
    ungroup()

  return(metadata)
}
