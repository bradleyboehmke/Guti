library(shiny)
library(rmarkdown)
library(flexdashboard)
library(lattice)
library(matrixStats)
library(networkD3)
library(Matrix)
library(caret)
library(ggplot2)
library(gplots)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(qdapTools)

source("Gutie_functions.R")

sampledata = tibble::as_tibble(readr::read_csv("sampledata.csv"))


charc_vars = which(lapply(sampledata, class) == "character",arr.ind = TRUE)
sampledata[,charc_vars] = lapply(sampledata[,charc_vars],as.factor)
num_rows = as.numeric(nrow(sampledata))
block_choice = get_all_factors(num_rows)
block_choice = block_choice$`39304`[7:16]

## state vector
num_IPs <- 10
num_blocks <- 289
block_length <- 289

numeric_vars = as.numeric(sum(sapply(sampledata,is.numeric)==TRUE))
mylist = which(sapply(sampledata,nlevels) < 100,arr.ind = TRUE)
num_levels = as.numeric(sum(sapply(sampledata[mylist],nlevels)))
block_width = num_levels + numeric_vars + num_IPs*2
State_Vector = matrix(nrow = num_blocks, ncol = block_width)
  
  i=1
  start = 1
  for (i in 1:num_blocks){
    stopp = block_length*i
    assign("temp",sampledata[start:stopp,])
    assign(paste("block",i,sep = ""),temp)
    temp_fac = temp %>% select_if(is.factor)
    mylist = which(sapply(temp_fac,nlevels) < 100,arr.ind = TRUE)
    vec1 = sapply(temp_fac[mylist],summary)
    if ("sourceAddress" %in% names(temp) ==TRUE) vec3 = head(summary(temp$sourceAddress),n=num_IPs)
    if ("destinationAddress" %in% names(temp) ==TRUE) vec3 = c(vec3,head(summary(temp$destinationAddress),n=num_IPs))
    temp_num = temp %>% select_if(is.numeric)
    vec2 = sapply(temp_num,sum)
    vec0 = c(unlist(vec1),unlist(vec2))
    if (exists("vec3")) vec0 = c(vec0,vec3)
    State_Vector[i,] = vec0
    start = stopp + 1
    next
  }
  namelist = c(unlist(sapply(temp_fac[mylist],levels)),names(temp_num),paste("S",1:num_IPs,sep=""),paste("D",1:num_IPs,sep=""))
  colnames(State_Vector) = namelist

## Factor Analysis

State_Vector <- State_Vector[complete.cases(State_Vector), ]
N <- nrow(State_Vector)
M <- ncol(State_Vector)
curvepoints <- Hornscurve(N,M)
  
FA_output <- Factor_Analysis(State_Vector, curvepoints)  
IFS_score = list(unrotated = IFS(FA_output$fa_loadings), rotated = IFS(FA_output$fa_loadings_rotated))

if (IFS_score$rotated > IFS_score$unrotated) {
  myloadings = as.matrix(FA_output$fa_loadings_rotated)
} else {
  myloadings = as.matrix(FA_output$fa_loadings)
}

if (IFS_score$rotated > IFS_score$unrotated) {
  scores = data.frame(FA_output$fa_scores_rotated)
  colnames(scores) = paste("FS",1:FA_output$num_factors,sep="")
} else {
  scores = data.frame(FA_output$fa_scores)
  colnames(scores) = paste("FS",1:FA_output$num_factors,sep="")
}

combos = t(combn(1:FA_output$num_factors, m=2))
combos = paste(combos[,1],combos[,2],sep = ",")

##################
# Color outliers #
##################
# option 1 - color
scores %>%
  dplyr::select(FS5, FS6) %>%
  dplyr::mutate(outlier = ifelse(abs(FS5) > 2 | abs(FS6) > 2, TRUE, FALSE)) %>%
  ggplot(aes(x=FS5, y=FS6, color = outlier, label= 1:nrow(State_Vector))) + 
  geom_text(show.legend = FALSE)

# option 2 - alpha
scores %>%
  dplyr::select(FS3, FS4) %>%
  dplyr::mutate(outlier = pmax(abs(FS3), abs(FS4))) %>%
  ggplot(aes(x=FS3, y=FS4, alpha = outlier, label= 1:nrow(State_Vector))) + 
  geom_text(show.legend = FALSE)




  