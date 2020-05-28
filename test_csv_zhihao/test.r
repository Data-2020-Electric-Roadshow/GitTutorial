# install.packages("pdftools")
# install.packages("pdftables")

library(pdftools)
library(qpdf)
library(pdftables)
convert_pdf('page_new.pdf', output_file = "test.csv", format = "csv", message = TRUE, api_key = "i2d9cxyrhsp5")


Sys.setenv(JAVA_HOME='C:/Program Files (x86)/Java/jre1.8.0_251/')
# install.packages("rJava")
# install.packages("tabulizer")
library(rJava)
library(tabulizer)

file <- "z.pdf"
et<- extract_tables(file, pages = 1, stream=TRUE)

?tabulizer