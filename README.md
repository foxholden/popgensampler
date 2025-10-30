# popgensampler

Tools for ranking and selecting genomic samples based on geographic distance, sample quality, and age. Designed to optimize sample selection for population genomics projects with budget constraints and spatial clustering requirements.

## Installation

You can install the development version of popgensampler from your local directory:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install popgensampler from local directory
devtools::install_local("~/Desktop/packages/popgensampler")
```

## Overview

`popgensampler` helps you make informed decisions about which samples to sequence when:
- You have more samples than you can afford to sequence
- Samples come from different sources with varying quality
- You need to maintain geographic representation
- Some samples have already been sequenced

The package provides three main functions:
1. **`calculate_age()`** - Calculate sample age from collection dates
2. **`rank_samples()`** - Rank samples based on quality, age, and sequencing status
3. **`select_samples()`** - Select optimal samples given constraints

## Quick Start

```r
library(popgensampler)
library(dplyr)
library(geosphere)

# 1. Create spatial clusters from your metadata
coords <- metadata %>% select(Long, Lat)
dist_matrix <- distm(as.matrix(coords), fun = distHaversine)
hclust_groups <- hclust(as.dist(dist_matrix), method = "complete")
metadata$Pop <- paste0("Cluster_", cutree(hclust_groups, h = 111111))

# 2. Rank samples within each cluster
ranked_meta <- rank_samples(metadata,
                           sample_type_col = "SampleType",
                           date_col = "CollectionDate",
                           sequenced_col = "Sequenced",
                           sample_type_ranking = as.data.frame(SampleType = c(Best, Worst), type_rank = c(1, 2))
                           prefer_recent = TRUE)

# 3. Select samples (basic usage)
selected <- select_samples(ranked_meta,
                          max_samples_per_cluster = 8,
                          min_samples_per_cluster = 4,
                          total_budget = 200)
```

## Advanced Usage

### Priority Clusters

Increase sample counts for specific clusters of interest:

```r
selected <- select_samples(ranked_meta,
                          max_samples_per_cluster = 8,
                          cluster_specific_max = list(
                            "Cluster_5" = 10,
                            "Cluster_12" = 10,
                            "Cluster_23" = 12
                          ))
```

### Protected Clusters

Prevent sample removal from priority clusters during budget enforcement:

```r
selected <- select_samples(ranked_meta,
                          max_samples_per_cluster = 8,
                          total_budget = 200,
                          cluster_specific_max = list(
                            "Cluster_5" = 10,
                            "Cluster_12" = 10
                          ),
                          protected_clusters = c("Cluster_5", "Cluster_12"))
```

## Sample Ranking Hierarchy

Samples are ranked within each cluster:

1. **Already sequenced** (always highest priority)
2. **Sample type** (quality hierarchy): user defined
3. **Sample age** (recent or old, depending on preference)

## Sample Selection Logic

The `select_samples()` function:

1. Always includes all sequenced samples
2. Selects unsequenced samples up to cluster-specific or default maximums
3. Removes clusters below minimum sample threshold
4. If budget constraint applies, removes worst-ranked samples from largest clusters
5. Respects protected clusters during budget enforcement

## Example Data

The package includes sample data in `inst/extdata/`:
- `sample_metadata.csv` - Example metadata with coordinates, sample types, and dates

Load example data:
```r
example_data <- read.csv(system.file("extdata", "sample_metadata.csv", 
                                     package = "popgensampler"))
```

## Requirements

- R >= 4.0.0
- dplyr >= 1.0.0
- tidyr >= 1.0.0
- geosphere >= 1.5.0 (for spatial clustering)

## License

MIT License

## Author

Holden Fox

## Citation

If you use this package in your research, please cite:

```
Fox, H. (2025). popgensampler: Sample Selection for Population Genomics Studies. 
R package version 0.1.0.
```
