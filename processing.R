use_package('doParallel')
use_package('foreach')
use_package('stringr')
use_package('TDA')
use_package('reshape2')
use_package('ggplot2')
use_package('png')
use_package('shiny')
use_package('shinydashboard')
use_package('plotly')
use_package('imager')
use_package('abind')
image_path <- "./images/miccai-2015/"
files <- list.files(image_path, pattern="\\.png$" )
files_with_path <- paste(image_path, files, sep = "")

num_cores <- detectCores()
cl <- makeCluster(num_cores)

registerDoParallel(cl)

persistence_diagrams <- foreach(i=files, .packages=c('TDA', 'png')) %dopar%{
    img_path <- paste(image_path, i, sep = "")
    img_mat <- readPNG(img_path) 
    # Luminance preserving grayscale conversion
    img_mat <- 0.2126*img_mat[,,1] + 0.7152*img_mat[,,2] + 0.0722*img_mat[,,3]
    Xlim <- c(0, nrow(img_mat)-1)
    Ylim <- c(0, ncol(img_mat)-1)
    lim <- cbind(Xlim, Ylim)
    by <- 1 
    # Generate persistence diagram
    tmp_diag <- gridDiag(FUNvalues=img_mat , lim=lim, by=by, sublevel=TRUE, 
                         library="Dionysus", printProgress=FALSE, location=TRUE)
    list(i, tmp_diag)
}

stopCluster(cl)

