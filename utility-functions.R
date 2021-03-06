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
perstImage <- function(file, diagram, min, max, birth, image="Color", generators=TRUE, show_cc = FALSE, show_cycles=TRUE, show_bg=TRUE){
    if(image=="Color")
    {
        img<-readPNG(file)
        plot(NULL, type='n', ann=FALSE, main="Persistence", xlab="x", ylab="y",xlim =range(0:dim(img)[2]),ylim = rev(range(dim(img)[1]:0)),yaxs="i",xaxs="i")
        if(show_bg){
            rasterImage(img, 0, dim(img)[1], dim(img)[2], 0)
        }
    }

    if(image=="Grayscale")
    { 
        img <- grayscale(load.image(file))
        plot(NULL, type='n', ann=FALSE, main="Persistence", xlab="x", ylab="y",xlim =range(0:dim(img)[1]),ylim = rev(range(dim(img)[2]:0)),yaxs="i",xaxs="i")
        if(show_bg){
            rasterImage(img, 0, dim(img)[2], dim(img)[1], 0)
        }
    }

    if(image=="Threshold")
    {
        img <- grayscale(load.image(file))
        img <- as.cimg(threshold(img, birth))
        plot(NULL, type='n', ann=FALSE, main="Persistence", xlab="x", ylab="y",xlim =range(0:dim(img)[1]),ylim = rev(range(dim(img)[2]:0)),yaxs="i",xaxs="i")
        if(show_bg){
            rasterImage(1-img, 0, dim(img)[2], dim(img)[1], 0)
        }
    }
    if(show_cycles){
        cycl <- getCycles(diagram, min, max, betti=1, birth)
        points(cycl[[2]],cycl[[1]], col='chartreuse3', pch=19, cex=.25)
    }

    if(show_cc){
        cycl <- getCycles(diagram, min, max, betti=0, birth)
        points(cycl[[2]],cycl[[1]], col='red', pch=19, cex=.25)
    }
}


#' Retrieves the cycles for a given persistence diagram
#' @param diag Persistence diagram to retrieve cycles from
#' @param betti Return cycles corresponding to provided Betti number 
#' @return return a 2*N list of x and y coordinates of cycles for given diagram
getCycles<-function(diag, min, max, betti, birth)
{

    if(betti==0){
        X<-unlist(lapply(diag$cycleLocation[which(diag$diagram[,1]==0 & diag$diagram[,3]-diag$diagram[,2]>min & diag$diagram[,3]-diag$diagram[,2]<max & diag$diagram[,2]<birth)], '[',,,1))
        Y<-unlist(lapply(diag$cycleLocation[which(diag$diagram[,1]==0 & diag$diagram[,3]-diag$diagram[,2]>min & diag$diagram[,3]-diag$diagram[,2]<max & diag$diagram[,2]<birth)], '[',,,2))
    }
    if(betti==1){
        X<-unlist(lapply(diag$cycleLocation[which(diag$diagram[,1]==1 & diag$diagram[,3]-diag$diagram[,2]>min & diag$diagram[,3]-diag$diagram[,2]<max & diag$diagram[,2]<birth)], '[',,,1))
        Y<-unlist(lapply(diag$cycleLocation[which(diag$diagram[,1]==1 & diag$diagram[,3]-diag$diagram[,2]>min & diag$diagram[,3]-diag$diagram[,2]<max & diag$diagram[,2]<birth)], '[',,,2))
    }
    return(list(X,Y))
}

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
