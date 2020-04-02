
# INTERVENTION DATA (OXFORD)
# https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker

library(readxl)
data_path <- "https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx"
tmpfile <- tempfile()
tmpdir <- tempdir()
download.file(data_path, tmpfile)
unzip(tmpfile, exdir = tmpdir)
interventions_data <- read_excel(paste0(tmpdir,"\\OxCGRT_Download_latest_data.xlsx"))

file.remove(tmp)
download.file("https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx"
              , destfile = "C:/Users/tkiely/Desktop/OxCGRT_Download_latest_data.xlsx")
