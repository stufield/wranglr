# Changelog

## wranglr 0.0.2 🚀

#### New

- Updated and improved `create_summ_tbl()`
  - new name!
    - now is
      [`create_sumry_tbl()`](https://stufield.github.io/wranglr/reference/create_sumry_tbl.md)
  - now uses
    [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
    under the hood for ease of use
  - new snapshot tests
- New methods for
  [`imputeNAs()`](https://stufield.github.io/wranglr/reference/imputeNAs.md)
  - character and factor S3 methods
  - imputes randomly by sampling according to the distribution of unique
    values in `x` and replaces
  - now the `.create_strata()` function can use
    [`imputeNAs()`](https://stufield.github.io/wranglr/reference/imputeNAs.md)
  - does so internally using internal methods rather than on-the-fly
- S3 methods for `.get_indices()`
  - now dispatches correctly using S3 methods rather than
    [`switch()`](https://rdrr.io/r/base/switch.html) and complex logic

#### Bugs

- Fixed major bug in
  [`imputeNAs()`](https://stufield.github.io/wranglr/reference/imputeNAs.md)
  - data frame method was indexing incorrectly
  - removed [`seq()`](https://rdrr.io/r/base/seq.html) call and index
    only on `p`
  - this was a bad one …

#### Improvements

- Create `vfold_splits()` unit tests and minor syntax clean up
  - added new unit tests (snapshots) for `vfold_splits()`
  - fixed a bug that was caused by using the names of breaks *after* An
    unlist
    - now uses a tmp `strat_vars` variable
  - all snapshots included under the `vfold_splits` variant
- Simplified `.get_indices.default()`
  - no just returns if unknown class
  - no more `NULL` for no stratification
  - this is now handled upstream, in `.vfold_splits()`
- Removed the `idx` param from `.get_indices()` methods
  - this was superfluous and a little hacky
  - solution isn’t great but considering it is just in the single data
    frame use case, maybe acceptable
- Cleaned up
  [`create_kfold()`](https://stufield.github.io/wranglr/reference/create_kfold.md)
  - updated unit tests and simplified code
  - streamline methods, especially character and factor for
    `.get_indices()`
  - breaks now cannot be NA

#### Removed

- no longer imports the purrr package:
  - no longer uses
    [`purrr::transpose()`](https://purrr.tidyverse.org/reference/transpose.html)
  - uses base R and [`Map()`](https://rdrr.io/r/base/funprog.html)
    instead to invert lists
  - now uses
    [`helpr::piter()`](https://stufield.github.io/helpr/reference/liter.html)
    over
    [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
- removed all mention of term “adat” and nuke S4 methods
  - the `'soma_adat'` class is also a data frame so this shouldn’t
    affect it too much to remove the method explicitly. Should cascade
    into the date frame method
  - checks in place to ensure attributes are not stripped

## wranglr 0.0.1 🎉

- Initial release! 🥳
  - this is `v0.0.1` tagged and released
