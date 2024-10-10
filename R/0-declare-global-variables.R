##################################
# Declaring Global Variables:
# This is mostly for passing R CMD checks
# global variables that come from other dependent
# packages, or objects in the data/ directory
# Reference: https://github.com/tidyverse/magrittr/issues/29
##################################
if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables(
    c(".",
      "group_var",
      "feature",
      "name",
      "List",
      "rangeLower",
      "rangeUpper",
      "role",
      "sdLower",
      "sdUpper",
      "SeqId", "AptName", "Dilution", "EnsemblGeneId", "EntrezGeneSymbol", "HgncId", "Target", "TargetFullName", "Type", "aptname"  # remove one day
    )
  )
}
