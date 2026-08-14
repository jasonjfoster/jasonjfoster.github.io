outputs <- strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n", fixed = TRUE)[[1]]

for (output in outputs) {
  if (!any(grepl("index_files", readLines(output, warn = FALSE), fixed = TRUE))) {
    unlink(file.path(dirname(output), "index_files"), recursive = TRUE)
  }
}
