download_file <- function(url, dest){
  if (extension == 'csv') {
    download_csv(url, dest)
  } else if (extension == 'tif') {
    download_tif(url, dest)
  } else if (extension == 'shp') {
    download_shp(url, dest)
  } else if (extension == 'zip'){
    
  }
}


download_tif <- function(url, dest){
  temp <- tempfile()
  download.file(url, dest, mode = "wb")
  star <- read_stars(file.path(dest), proxy = TRUE)
  return(star)
}


download_csv <- function(url, dest){
  
  temp <- tempfile()
  download.file(url, temp, method = "wget", extra = "--no-check-certificate")
  master <- as.character(unzip(temp, list = TRUE)$Name)
  df <- read.csv(unz(temp, master[1]))
  return(df)
}


download_shp <- function(url, dest){
  temp <- tempfile()
  download.file(url, dest, method = "wget", extra = "--no-check-certificate")
  shp <- st_read(file.path("/vsizip", dest))
  return(shp)
}