
load("R/sysdata.rda", verbose = TRUE)

anno_file <- "~/gh/SomaDataIO/SomaScan_V5.0_11K_Annotated_Content_20240214.xlsx"
annotations_v5.0 <- readxl::read_xlsx(anno_file, skip = 8)

annotations_v5.0 <- dplyr::select(annotations_v5.0,
                                  -starts_with("SELEX"),
                                  -"Ensembl Gene ID",
                                  -"HGNC ID",
                                  -"Entrez Gene ID",
                                  -"New Content")

annotations_v5.0 <- dplyr::rename(annotations_v5.0,
                                  "ApparentKdM" = "Apparent Kd (M)",
                                  "Target" = "Target Name",
                                  "UniProt" = "UniProt ID",
                                  "EntrezGeneSymbol" = "Entrez Gene Name",
                                  "TargetFullName" = "Target Full Name")

annotations_v5.0 <- dplyr::left_join(
  annotations_v5.0,
  annotations_v4.1[, c("SeqId", "Reason", "List")],   # merge Reason, List
  by = "SeqId"
)

annotations_v5.0 <- dplyr::select(annotations_v5.0,
                                  all_of(names(annotations_v4.1)),
                                  everything())

annotations_v5.0 <- dplyr::relocate(annotations_v5.0, "List", "Reason",
                                    .after = Type)

x <- readRDS("inst/data-raw/sample-data.rds") |>
  dplyr::select(-SiteId, -Subject_ID, -RowCheck, -SampleType,
                -SampleMatrix, -PlateId,
                sid   = SlideId,
                sub   = Subarray,
                ind_id = SampleId,
                group = SampleGroup,
                time  = TimePoint,
                norm0 = HybControlNormScale,
                norm1 = NormScale_0_005,
                norm2 = NormScale_1,
                norm3 = NormScale_40)

attr(x, "anno") <- readRDS("inst/data-raw/sample-data.rds") |>
  attr("Col.Meta") |>
  dplyr::select(SeqId, Target, TargetFullName, EntrezGeneID, EntrezGeneSymbol)
sample_df <- x

save(annotations_v4.0,
     annotations_v4.1,
     annotations_v5.0,
     sample_df,
     file = "R/sysdata.rda",
     compress = "xz")
