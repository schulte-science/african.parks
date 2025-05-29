overview <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1VBkkzWRVTb5GcTZC1alg5mINGZfB75N0NJMrBLehkWo/edit?usp=sharing", sheet = "Overview") |>
  dplyr::select(DateRun, DateFirstAnalyzed, JVRunId, JVBatchId, Platform, SampleType, Amplicon, Samples) |>
  dplyr::filter(Amplicon %in% c("12SVert", "trnL"))
write.csv(overview, "Overview_20250519.csv", row.names = F)

meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1VBkkzWRVTb5GcTZC1alg5mINGZfB75N0NJMrBLehkWo/edit?usp=sharing", sheet = "Metadata_20250519")
write.csv(meta, "Metadata_20250519.csv", row.names = F)

vert <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1VBkkzWRVTb5GcTZC1alg5mINGZfB75N0NJMrBLehkWo/edit?usp=sharing", sheet = "12S_Summary_20250519", col_types = "c") |>
  dplyr::mutate(Reads = as.numeric(Reads)) |>
  dplyr::group_by(Barcode) |>
  dplyr::mutate(Reads_Prey = sum(Reads[!is.na(DNA_Species_Prey)], na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(RRA_Prey = ifelse(!is.na(DNA_Species_Prey), round(Reads / Reads_Prey * 100,2), NA))
write.csv(vert, "12S_Summary_20250519.csv", row.names = F)

trnl <- read.csv("~/Jonah Ventures Dropbox/JonahClients/MeyerJordana/MeyerJordana_Analysis/RunData/20250519/JVB3872-trnL-read-data_20250519.csv", check.names = FALSE) |>
  dplyr::select(ESVId, matches("S[0-9]")) |>
  tidyr::pivot_longer(-ESVId, names_to = "SampleId", values_to = "Reads") |>
  dplyr::filter(Reads > 0) |>
  dplyr::group_by(SampleId) |>
  dplyr::mutate(rel = Reads / sum(Reads)) |>
  dplyr::filter(rel >= 0.01) |>
  dplyr::mutate(rel = Reads / sum(Reads)) |>
  dplyr::ungroup() |>
  dplyr::mutate(Barcode = gsub("\\..*", "", SampleId),
                Replicate = gsub(".*\\.", "", SampleId)) |>
  dplyr::group_by(Barcode) |>
  dplyr::filter(!(Replicate == "1" & any(Replicate == "2"))) |>
  dplyr::ungroup() |>
  dplyr::relocate(c(Barcode, Replicate), .before = SampleId) |>
  dplyr::left_join(trnl |> dplyr::distinct(ESVId, .keep_all = TRUE) |> dplyr::select(TestId:`# species`), by = "ESVId") |>
  dplyr::left_join(vert |> dplyr::distinct(Barcode, .keep_all = TRUE) |> dplyr::select(Barcode, name_of_park, DNA_Species_Host, DNA_Common_Host, DNA_Diet_Host), by = "Barcode")

write.csv(trnl, "trnL_Summary_20250519.csv", row.names = F)
