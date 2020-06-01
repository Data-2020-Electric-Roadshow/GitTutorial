## This file test the "REA/1941.REA annual statistical report.pdf"

# To make the code work, you need to install below package
# install.packages("pdftools")
# install.packages("pdftables")
# install.packages("magick")
# install.packages("tesseract")

library(pdftools)
library(qpdf)

library(tesseract)
library(magick)

setwd("E:/Data Plus/data/raw/From REA/Annual Statistical Report")

tar <- c(seq(7, 8))
for (i in tar) {
  ## select the page of pdf files we want to convert (7-103)
  files <- pdf_subset("1941.REA annual statistical report.pdf", page= i, output= paste0('output_',i, '.pdf'))
  ## convert pdf to png
  bitmap <- pdf_render_page(files, dpi = 150)
  png::writePNG(bitmap, paste0('page_',i, '.png'))
  ## Read the picture
  input <- image_read(paste0('page_',i, '.png'))
  ## processing png to improve quality
  text <- input %>%
    image_convert(type = 'grayscale') %>% # convert the picture to white-black style (especially for those with yellow background)
    image_deskew() %>% # auto rotate to correct skewed images
    # image_rotate(90) %>% this command may help rotate tables vertically for some pages, but for this test, I quote it since original page has been rotated
    image_enhance() %>% # minimize the background noise
    image_contrast() %>% # enhances intensity differences in image
    image_write(path = paste0('page_new_',i, '.png'), format = 'png')
} 
  
  ## text recognization and export it into csv files
  results <- tesseract::ocr(paste0('page_new_',i, '.png'), engine = "eng")
  ## objects returned from ocr is a long character, thus we first
  ## export it to csv file using white space, and then convert it
  ## to "," separator for further analysis
  write.table (results, file = paste0('page_new_',i, '.csv'), sep=' ')
  results_csv <- read.csv(paste0('page_new_',i, '.csv'), header= FALSE, sep = ' ', quote = "")
  write.csv (results_csv, file = paste0('page_final_',i, '.csv'))

  ## delete useless files
  unlink(paste0('output_',i, '.pdf'))
  unlink(paste0('page_',i, '.png'))
  unlink(paste0('page_new_',i, '.csv'))
}
