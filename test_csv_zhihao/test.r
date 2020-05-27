# install.packages("pdftools")
# install.packages("pdftables")

library(pdftools)
library(qpdf)
library(pdftables)

files <- pdf_subset("1942.FPC.Production of Electric Energy.pdf", page= 23, output= "z4.pdf")

tar_files <- pdf_text("z.pdf")

convert_pdf('z.pdf', output_file = "test", format = "csv", message = TRUE, api_key = "i2d9cxyrhsp5")