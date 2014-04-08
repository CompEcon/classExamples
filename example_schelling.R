
#===========================================
#
#  computational economics:  2 Schelling Segregation
#
#  author: John Eric Humphries johneric@uchicago.edu
#  date: 4-7-2014
#
#  abstract: A Schelling example
#
#===========================================

#========================
# Section 0: setup
#========================

#setwd("/mnt/ide0/home/johneric/sbox/projects/neighborhoodVision/")
rm(list=ls())           # Clear the workspace
set.seed(777) 
library(compiler)
#enableJIT(1)
library(knitr)
library(ggplot2)
library(mvtnorm)
library(reshape2)




SchellingGrid <- function(gs = 100, probabilities = c(.495,.495,.1) , stop.val = .99 , happy.val = .4) {
    values = c(1,-1,0)
    grid.size = gs
    values.mat = matrix(sample(values, grid.size^2, replace = T, prob = probabilities), grid.size, grid.size )
    # starting plot
    p <- (qplot(x=Var1, y=Var2, data=melt(values.mat), fill=value, geom="tile", color = "white", main="SCHELLING GRID: 0") 
          +  scale_fill_gradient2(low = "lightgreen", mid = "white", high = "steelblue")  ) + theme(legend.position = "none") 
    print(p)  
    values.happy = matrix(-10, grid.size, grid.size)
    values.same  = matrix(NA, grid.size, grid.size)
    ratio = happy.val
    stop.ratio = stop.val
    i = 0
        while ( sum(values.happy, na.rm=T) / (sum(values.mat!=0, na.rm=T)) < stop.ratio) {
            i = i + 1
            for (row in sample(1:grid.size)) {
                for (col in sample(1:grid.size)) {
                    nbrs = c(rep(NA,8))
                    if( row>1 & col>1)                  nbrs[1] <- values.mat[row -1, col -1]
                    if( row>1)                          nbrs[2] <- values.mat[row -1, col   ]
                    if( row>1 & col<grid.size)          nbrs[3] <- values.mat[row -1, col +1]
                    if( col>1)                          nbrs[4] <- values.mat[row   , col -1]
                    if( col<grid.size)                  nbrs[5] <- values.mat[row   , col +1]
                    if( row<grid.size & col>1)          nbrs[6] <- values.mat[row +1, col -1]
                    if( row<grid.size )                 nbrs[7] <- values.mat[row +1, col   ]
                    if( row<grid.size & col<grid.size)  nbrs[8] <- values.mat[row +1, col +1]
                    # checking if they want to move, and if so, moving at random
                    val = values.mat[row,col]
                    if (val == 1) {
                        if (sum(nbrs==1 ,na.rm=T) / sum(!is.na(nbrs))  < ratio )  {
                            values.mat[row,col] = 0
                            newhome = sample(which(values.mat==0),1)
                            values.mat[newhome] = 1
                            values.happy[newhome] =0
                            values.happy[row, col] = NA
                        }
                        if (sum(nbrs==1 ,na.rm=T) / sum(!is.na(nbrs))  >= ratio ) {
                            values.happy[row, col] =1     
                            values.same[row,col] = sum(nbrs==1 ,na.rm=T) / sum(!is.na(nbrs))
                        }
                    }
                     if (val == -1 ) {
                        if (sum(nbrs== -1 , na.rm=T) / sum(!is.na(nbrs))  < ratio )  {
                            values.mat[row,col] = 0
                            newhome = sample(which(values.mat==0),1)
                            values.mat[newhome] = -1 
                            values.happy[newhome] =0
                            values.happy[row, col] = NA
                        }  
                        if (sum(nbrs== -1 ,na.rm=T) / sum(!is.na(nbrs))  >= ratio ) {
                            values.happy[row, col] =1
                            values.same[row,col] = sum(nbrs== -1 ,na.rm=T) / sum(!is.na(nbrs))
                        }
                     }
                    if (val == 0)  
                    {
                        values.happy[row,col] = NA
                    }
                } # end column loop
            } # end row loop
            print(paste("Percent Happy:", 100 * sum(values.happy, na.rm=T) / (sum(values.mat!=0)), "(iteration", i, ")" )) # Printing percent happy
            p <- (qplot(x=Var1, y=Var2, data=melt(values.mat), fill=value, geom="tile", color = "white", main= paste("SCHELLING GRID:",i)) 
                  +  scale_fill_gradient2(low = "lightgreen", mid = "white", high = "steelblue")  ) + theme(legend.position = "none") 
            if (i %% 5 == 0) print(p)  # printing intermediatne plot every so many iterations
        } # end while statement
    # Printing final figure   
    p <- (qplot(x=Var1, y=Var2, data=melt(values.mat), fill=value, geom="tile", color = "white", main= "SCHELLING GRID: (final)") 
          +  scale_fill_gradient2(low = "lightgreen", mid = "white", high = "steelblue")  ) + theme(legend.position = "none") 
    print(p)
    return(c(mean(values.happy, na.rm=T),mean(values.same, na.rm=T), i,p))
}


# Running the function
results.schelling = SchellingGrid(gs = 100, probabilities = c(.4955,.4955,.01) , stop.val = .995 , happy.val = .51)














