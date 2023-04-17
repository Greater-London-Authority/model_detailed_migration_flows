fetch_mye <- function(
    fp_save,
    url_zip
) {

  dir_save <- paste0(dirname(fp_save), "/")

  fp_zip <- paste0(dir_save, basename(url_zip))

  download.file(url = url_zip,
                destfile = fp_zip)

  csv_name <- unzip(fp_zip,
                        list = TRUE) %>%
    filter(grepl(".csv", Name)) %>%
    pull(Name)


  unzip(fp_zip,
        files = csv_name,
        exdir = paste0(dir_save, "."))

  file.rename(from = paste0(dir_save, csv_name),
              to = fp_save)

  file.remove(fp_zip)
}
