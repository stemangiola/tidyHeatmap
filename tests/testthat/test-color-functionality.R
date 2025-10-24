# Unit tests for color functionality in layer_text and layer_symbol functions
library(testthat)
library(tidyHeatmap)
library(dplyr)
library(tibble)
library(vdiffr)

# Create rectangular test data
test_data <- expand.grid(
  row = c("A", "B"),
  col = c("X", "Y"),
  stringsAsFactors = FALSE
) |>
  mutate(
    value = c(1, 2, 3, 4),
    color_col = c("darkred", "navy", "darkgreen", "purple"),
    size_col = c(5, 10, 15, 20)
  ) |>
  as_tibble()

test_that("layer_symbol with direct color value works", {
  p <- test_data |>
    heatmap(row, col, value) |>
    tidyHeatmap:::layer_symbol(value > 2, 
                 symbol = "point", 
                 .color = "red",
                 .size = 5)
  
  vdiffr::expect_doppelganger("layer_symbol_direct_color", p)
})

test_that("layer_symbol with color column works", {
  p <- test_data |>
    heatmap(row, col, value) |>
    tidyHeatmap:::layer_symbol(value > 2, 
                 symbol = "point", 
                 .color = color_col,
                 .size = 5)
  
  vdiffr::expect_doppelganger("layer_symbol_color_column", p)
})

test_that("layer_symbol with default color works", {
  p <- test_data |>
    heatmap(row, col, value) |>
    tidyHeatmap:::layer_symbol(value > 2, 
                 symbol = "point", 
                 .size = 5)
  
  vdiffr::expect_doppelganger("layer_symbol_default_color", p)
})

test_that("layer_text with direct color value works", {
  p <- test_data |>
    heatmap(row, col, value) |>
    layer_text(value > 2, 
               .value = "X", 
               .color = "blue",
               .size = 10)
  
  vdiffr::expect_doppelganger("layer_text_direct_color", p)
})

test_that("layer_text with color column works", {
  p <- test_data |>
    heatmap(row, col, value) |>
    layer_text(value > 2, 
               .value = "X", 
               .color = color_col,
               .size = 10)
  
  vdiffr::expect_doppelganger("layer_text_color_column", p)
})

test_that("layer_text with default color works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_text(value > 2, 
                 .value = "X", 
                 .size = 10)
  
  vdiffr::expect_doppelganger("layer_text_default_color", p)
})

test_that("layer_point with color works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_point(value > 2, 
                  .color = "green",
                  .size = 8)
  
  vdiffr::expect_doppelganger("layer_point_color", p)
})

test_that("layer_point with color column works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_point(value > 2, 
                  .color = color_col,
                  .size = 8)
  
  vdiffr::expect_doppelganger("layer_point_color_column", p)
})

test_that("layer_square with color works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_square(value > 2, 
                   .color = "purple",
                   .size = 6)
  
  vdiffr::expect_doppelganger("layer_square_color", p)
})

test_that("layer_diamond with color works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_diamond(value > 2, 
                    .color = "orange",
                    .size = 7)
  
  vdiffr::expect_doppelganger("layer_diamond_color", p)
})

test_that("layer_star with color works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_star(value > 2, 
                 .color = "pink",
                 .size = 9)
  
  vdiffr::expect_doppelganger("layer_star_color", p)
})

test_that("layer_asterisk with color works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_asterisk(value > 2, 
                     .color = "brown",
                     .size = 4)
  
  vdiffr::expect_doppelganger("layer_asterisk_color", p)
})

test_that("layer_arrow_up with color works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_arrow_up(value > 2, 
                     .color = "cyan",
                     .size = 6)
  
  vdiffr::expect_doppelganger("layer_arrow_up_color", p)
})

test_that("layer_arrow_down with color works", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_arrow_down(value > 2, 
                       .color = "magenta",
                       .size = 6)
  
  vdiffr::expect_doppelganger("layer_arrow_down_color", p)
})

test_that("multiple layers with different colors work", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_point(value > 2, .color = "red", .size = 5) |>
      layer_text(value > 3, .value = "!", .color = "blue", .size = 8)
  
  vdiffr::expect_doppelganger("multiple_layers_colors", p)
})

test_that("color and size columns work together", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_point(value > 2, 
                  .color = color_col,
                  .size = size_col)
  
  vdiffr::expect_doppelganger("color_size_columns", p)
})

test_that("color and size columns work together for text", {
  p <- test_data |>
      heatmap(row, col, value) |>
      layer_text(value > 2, 
                 .value = "X", 
                 .color = color_col,
                 .size = size_col)
  
  vdiffr::expect_doppelganger("color_size_columns_text", p)
})
