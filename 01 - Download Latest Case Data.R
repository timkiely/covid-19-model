

# updated every ~4 hours
# cases_data <- suppressMessages(read_csv("http://hgis.uw.edu/virus/assets/virus.csv"))

latest_file <- paste0("hgis virus data/",Sys.Date()," virus.csv")
if(!file.exists(latest_file)){
  
  download.file("http://hgis.uw.edu/virus/assets/virus.csv", destfile = latest_file)
  
}