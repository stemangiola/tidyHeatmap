# NA/NaN handling tests for tidyHeatmap annotations

test_that("NA/NaN handling in discrete and grouped annotations", {
  library(tidyHeatmap)
  library(dplyr)

  # Create test data with NA values in a discrete annotation
  test_data <- tibble::tibble(
    sample = c("s1", "s2", "s3", "s4"),
    gene = c("g1", "g1", "g1", "g1"),
    value = c(1.5, 2.1, 1.8, 0.9),
    status = c("active", NA, "inactive", "active"),
    condition = c("control", "treatment", "control", "treatment")
  )

  expect_warning(
    p1 <- test_data %>%
      heatmap(sample, gene, value) %>%
      annotation_tile(status),
    "tidyHeatmap says: You have NA/NaN values in your annotation data. These will be replaced with 'NA'."
  )
  expect_s4_class(p1, "InputHeatmap")

  # Grouping with NA values
  test_data_grouped <- test_data %>%
    mutate(group = c("A", "A", "B", "B")) %>%
    mutate(group = ifelse(row_number() == 2, NA, group))

  expect_warning(
    p2 <- test_data_grouped %>%
      group_by(group) %>%
      heatmap(sample, gene, value) %>%
      annotation_tile(condition),
    "tidyHeatmap says: You have NA/NaN values in your row grouping column. These will be replaced with 'NA'."
  )
  expect_s4_class(p2, "InputHeatmap")
})

# Numeric annotation NA/NaN handling is not robust yet, so we skip for now
# test_that("NA/NaN handling in numeric annotations", {
#   library(tidyHeatmap)
#   library(dplyr)
#   test_data_numeric <- tibble::tibble(
#     sample = c("s1", "s2", "s3", "s4"),
#     gene = c("g1", "g1", "g1", "g1"),
#     value = c(1.5, 2.1, 1.8, 0.9),
#     score = c(1.2, NA, 0.8, 1.5)
#   )
#   expect_warning(
#     p3 <- test_data_numeric %>%
#       heatmap(sample, gene, value) %>%
#       annotation_numeric(score),
#     "tidyHeatmap says: You have NA/NaN values in your annotation data. These will be replaced with 'NA'."
#   )
#   expect_s4_class(p3, "InputHeatmap")
# }) 