#' Directly writes the content of an ADIS data file to multiple .csv files.
#' 
#' Do you really want to use another software than \code{R} to analyse your data???
#' - \code{adis2csv2()} helps you with this sad task...
#' It uses \link{read_adis} to read ADIS files to R and directly writes all entities as .csv files in subdirectories of 
#' \code{output_path} for each header (.csv files are written by \link{write.csv2}).
#' 
#' @param file Character of length 1. Full path to the ADIS file to be read, expected in UNIX-style ('/').
#' @param output_path Character of length 1. Where shall the .csv files be saved ? (expected in UNIX-style ('/')).
#'  If \code{NULL} (default), the working directory is used.
#' @param entities Character vector. Which entities should be exported? Is recycled for all headers.
#'  If \code{NULL} (default), all.
#' @param ... Further arguments passed to \link{read_adis}.
#' 
#' @note Despite using \link{write.csv2}, the decimal points in the .csv files will be '.' because all items 
#'  are imported as character vectors.



adis2csv2 <- function(file, output_path = NULL, entities = NULL, ...) {
  
  # Path arguments are expected in UNIX-style ('/' instead of Windows-'\')
  
  if (is.null(output_path)) {
    
    output_path <- getwd()
    
  } else {
    
    if (!dir.exists(output_path)) {
      
      dir.create(output_path)
      
    }
    
  }
  
  if (substr(output_path, nchar(output_path), nchar(output_path)) != "/") {
    
    output_path <- paste(output_path, "/", sep = "")
    
  }
  
  
  
  lsAdis <- read_adis(datei = file, kommentare = FALSE, ...)
  
  for (iH in seq_along(lsAdis)) {
    
    dirH <- paste(output_path, "/", names(lsAdis)[iH], "/", sep = "")
    dir.create(dirH)
    
    if (is.null(entities)) {
      
      IE <- seq_along(lsAdis[[iH]])
      
    } else {
      
      IE <- which(names(lsAdis[[iH]]) %in% entities)
      
    }
    
    for (iE in IE) {
      
      write.csv2(lsAdis[[iH]][iE], 
                 file = paste(dirH, names(lsAdis[[iH]][iE]), ".csv", sep = ""), 
                 row.names = FALSE)
      
    }
    
  }
  
  invisible(NULL)
  
}