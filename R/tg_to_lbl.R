library(tidyverse)

tg_to_lbl <- function() {
  text_grids <- list.files(path = here::here("data","01_interim","01_textgrids"),
                           pattern = ".TextGrid",
                           full.names = F)
  
  k <- 1
  
  while (k <= base::NROW(text_grids)) {
    file <- text_grids[k]
    
    segments <- rPraat::tg.read(fileNameTextGrid = here::here("data",
                                                              "01_interim",
                                                              "01_textgrids",
                                                              file))
    lbl_data <- segments$vowel %>%
      as.data.frame() %>%
      dplyr::select(t1:label) %>%
      dplyr::mutate(t1 = t1*1000,
                    t2 = t2*1000,
                    t1 = round(t1, digits = 3),
                    t2 = round(t2, digits = 3),
                    label = str_remove(label, "\\.[^.]*$"),
                    label = base::tolower(label)) %>%
      dplyr::filter(label != "")
    
    
    readr::write_delim(lbl_data,
                       file = here::here("data",
                                         "01_interim",
                                         "02_lbl_files",
                                         base::gsub(x = file,
                                                    pattern = ".TextGrid",
                                                    replacement = ".lbl")),
                       col_names = FALSE)
    
    rm(file, segments, lbl_data)
    
    k <- k + 1
  }
  
  rm(text_grids, k)
}
