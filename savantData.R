library(broom)
library(tibble)
library(purrr)
library(parallelly)
library(tictoc)
library(sabRmetrics)
library(tidyverse)


cluster <- makeClusterPSOCK(8)

on.exit(stopCluster(cluster), add = TRUE)

savant_data_2024 <- tryCatch(
  {
    sabRmetrics::download_baseballsavant(
      start_date = "2024-03-28",
      end_date   = "2024-9-10",
      cl         = cluster
    )
  },
  error = function(e) {
    message("Download failed: ", e$message)
    NULL
  }
)

savant_data_2025 <- tryCatch(
  {
    sabRmetrics::download_baseballsavant(
      start_date = "2025-03-18",
      end_date   = "2025-07-12",
      cl         = cluster
    )
  },
  error = function(e) {
    message("Download failed: ", e$message)
    NULL
  }
)

savant_data <- bind_rows(savant_data_2024, savant_data_2025)

write_csv(savant_data, "savantData2425.csv")