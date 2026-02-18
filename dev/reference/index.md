# Package index

## Manipulating data

Functions to manipulate data `R` objects.

- [`bind_intersect()`](https://stufield.github.io/wranglr/dev/reference/bind.md)
  [`bind_union()`](https://stufield.github.io/wranglr/dev/reference/bind.md)
  : Vertically Combine Data Frames by Intersect
- [`create_kfold()`](https://stufield.github.io/wranglr/dev/reference/create_kfold.md)
  [`is.k_split()`](https://stufield.github.io/wranglr/dev/reference/create_kfold.md)
  [`analysis()`](https://stufield.github.io/wranglr/dev/reference/create_kfold.md)
  [`assessment()`](https://stufield.github.io/wranglr/dev/reference/create_kfold.md)
  : Create k-Fold Partitioning
- [`distinct_rows()`](https://stufield.github.io/wranglr/dev/reference/distinct_rows.md)
  : Select Distinct Rows
- [`feature_matrix()`](https://stufield.github.io/wranglr/dev/reference/feature_matrix.md)
  : Select the Feature Matrix
- [`group_labels()`](https://stufield.github.io/wranglr/dev/reference/group_labels.md)
  : Return Grouping Labels
- [`rearrange()`](https://stufield.github.io/wranglr/dev/reference/rearrange.md)
  : Rearrange Rows by Variables
- [`rename(`*`<list>`*`)`](https://stufield.github.io/wranglr/dev/reference/rename.md)
  : Rename Elements of a List
- [`refactor_data()`](https://stufield.github.io/wranglr/dev/reference/refactor_data.md)
  : Refactor Ghost Level Meta Data
- [`match_samples()`](https://stufield.github.io/wranglr/dev/reference/match_samples.md)
  : Match Two Data Frames Based on Sample ID

## Adding and augmenting

Functions to create summaries and add summary data to objects.

- [`calc_cv()`](https://stufield.github.io/wranglr/dev/reference/calc_cv.md)
  : Calculate CV Decomposition
- [`create_sumry_tbl()`](https://stufield.github.io/wranglr/dev/reference/create_sumry_tbl.md)
  : Create A Summary Table

## Transforming data

Functions to transform data `R` objects.

- [`imputeNAs()`](https://stufield.github.io/wranglr/dev/reference/imputeNAs.md)
  : Impute NAs
- [`impute_outliers()`](https://stufield.github.io/wranglr/dev/reference/impute_outliers.md)
  : Impute Outlier Values
- [`impute_predictors()`](https://stufield.github.io/wranglr/dev/reference/impute_predictors.md)
  : Impute Predictor Variables
- [`center_scale()`](https://stufield.github.io/wranglr/dev/reference/center_scale.md)
  [`is_center_scaled()`](https://stufield.github.io/wranglr/dev/reference/center_scale.md)
  [`undo_center_scale()`](https://stufield.github.io/wranglr/dev/reference/center_scale.md)
  : Center and/or Scale Data
- [`cast_numeric()`](https://stufield.github.io/wranglr/dev/reference/cast_numeric.md)
  : Convert Table Entries to Numeric
- [`create_recipe()`](https://stufield.github.io/wranglr/dev/reference/create_recipe.md)
  [`bake_recipe()`](https://stufield.github.io/wranglr/dev/reference/create_recipe.md)
  [`is.baked()`](https://stufield.github.io/wranglr/dev/reference/create_recipe.md)
  [`convert_recipe()`](https://stufield.github.io/wranglr/dev/reference/create_recipe.md)
  : Pre-processing Analysis Data
- [`get_recipe_params()`](https://stufield.github.io/wranglr/dev/reference/get_recipe_params.md)
  : Get Parameters From A Recipe
- [`scale_features()`](https://stufield.github.io/wranglr/dev/reference/scale_features.md)
  : Scale/transform Features (variables)
- [`rebalance()`](https://stufield.github.io/wranglr/dev/reference/rebalance.md)
  : Sampling for Class Imbalances
- [`remove_outliers()`](https://stufield.github.io/wranglr/dev/reference/remove_outliers.md)
  : Remove Statistical Outliers

## Proteomic annotations

Useful helpers for matching, exploring, and looking up protein
annotations based on `SeqId`s.

- [`get_anno()`](https://stufield.github.io/wranglr/dev/reference/anno.md)
  [`grep_anno()`](https://stufield.github.io/wranglr/dev/reference/anno.md)
  [`seq_lookup()`](https://stufield.github.io/wranglr/dev/reference/anno.md)
  [`seqify()`](https://stufield.github.io/wranglr/dev/reference/anno.md)
  : Protein Annotations

## Data

A simulated proteomic data set for data science analyses.

- [`simdata`](https://stufield.github.io/wranglr/dev/reference/simdata.md)
  : Simulated Data Object
