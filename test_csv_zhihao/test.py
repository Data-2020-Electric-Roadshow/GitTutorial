from tabula import convert_into
from tabula import read_pdf

file4 = "e:/Data Plus/data/raw/From FPC/Production of Electric Energy/z4.pdf"
convert_into(file4, "test4.csv", encoding='utf-8', output_format="csv", pages=1, stream=True)
