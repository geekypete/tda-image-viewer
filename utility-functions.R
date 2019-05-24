#' Function to install a package if not installed, and then load
use_package <- function(p) {
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.cnr.berkeley.edu/"
    options(repos = r)
    rm(r)
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE)
    library(p, character.only = TRUE)
}

#' plots Betti 1 Cycles and nuclei position over original image
#' @param file filename and path to .png image to overlay cycles on
#' @param perst list in the form list(persistenceDiagram, nucleiXYcoordinates)
#' @export
perstImage <- function(file, diagram, min, max, betti){
    img<-readPNG(file)
    plot(NULL, type='n', ann=FALSE, main="Persistence", xlab="x", ylab="y",xlim =range(0:dim(img)[2]),ylim = rev(range(dim(img)[1]:0)),yaxs="i",xaxs="i")

    #plot(NULL, type='n', ann=FALSE, main="Persistence", xlab="x", ylab="y",xlim =range(0:dim(img)[2]),ylim = range(0:dim(img)[1]),yaxs="i",xaxs="i")
    rasterImage(img, 0, dim(img)[1], dim(img)[2], 0)
    cycl <- getCycles(diagram, min, max, betti=betti)
    points(cycl[[2]],cycl[[1]], col=2)
}

#' plots Betti 1 Cycles and nuclei position over original image
#' @param file filename and path to .png image to overlay cycles on
#' @param perst list in the form list(persistenceDiagram, nucleiXYcoordinates)
#' @export
perstGrayImage <- function(file, diagram, min, max, betti){
    img <- grayscale(load.image(file))
    img <- as.cimg(threshold(img, min))
    plot(NULL, type='n', ann=FALSE, main="Persistence", xlab="x", ylab="y",xlim =range(0:dim(img)[1]),ylim = rev(range(dim(img)[2]:0)),yaxs="i",xaxs="i")

    rasterImage(img, 0, dim(img)[2], dim(img)[1], 0)
    cycl <- getCycles(diagram, min, max, betti=betti)
    points(cycl[[2]],cycl[[1]], col=2)
}

#' Retrieves the cycles for a given persistence diagram
#' @param diag Persistence diagram to retrieve cycles from
#' @param betti Boolean if True return only betti 1, else return all
#' @return return a 2*N list of x and y coordinates of cycles for given diagram
getCycles<-function(diag, min, max, betti)
{

  if(betti==0){
        X<-unlist(lapply(diag$cycleLocation[which(diag$diagram[,1]==0 & diag$diagram[,3]-diag$diagram[,2]>min & diag$diagram[,3]-diag$diagram[,2]<max)], '[',,,1))
        Y<-unlist(lapply(diag$cycleLocation[which(diag$diagram[,1]==0 & diag$diagram[,3]-diag$diagram[,2]>min & diag$diagram[,3]-diag$diagram[,2]<max)], '[',,,2))
  }
    if(betti==1){
        X<-unlist(lapply(diag$cycleLocation[which(diag$diagram[,1]==1 & diag$diagram[,3]-diag$diagram[,2]>min & diag$diagram[,3]-diag$diagram[,2]<max)], '[',,,1))
        Y<-unlist(lapply(diag$cycleLocation[which(diag$diagram[,1]==1 & diag$diagram[,3]-diag$diagram[,2]>min & diag$diagram[,3]-diag$diagram[,2]<max)], '[',,,2))
  }
    return(list(X,Y))
}
