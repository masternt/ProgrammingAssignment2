
## Overall description of functions

##Matrix inversion is usually a costly computation
##There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
##This function is to write a pair of functions that cache the inverse of a matrix.


## 1.makeCacheMatrix function: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve function: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##   If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Assume that the matrix supplied is always invertible.


##sample
##
##makeVector <- function(x = numeric()) {
##        m <- NULL
##        set <- function(y) {
##                x <<- y
##                m <<- NULL
##        }
##        get <- function() x
##        setmean <- function(mean) m <<- mean
##        getmean <- function() m
##        list(set = set, get = get,
##             setmean = setmean,
##             getmean = getmean)
##}
#

## create makeCacheMatrix function similar to makeVector fuction, create for matrix instead of vector
## This function creates a special "matrix" object, which is really a list  
## containing a function to 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 4. get the value of the inverse which get from solve function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() 
                x
        setMatrixInverse<-function(solve) 
                m<<- solve ## use solve function to get inverse in hint
        getMatrixInverse<-function() 
                m
        list(set=set, get=get,
             setMatrixInverse=setMatrixInverse,
             getMatrixInverse=getMatrixInverse)
}



##sample
##cachemean <- function(x, ...) {
##m <- x$getmean()
##if(!is.null(m)) {
##        message("getting cached data")
##        return(m)
##}
##data <- x$get()
##m <- mean(data, ...)
##x$setmean(m)
##m
##}

## create cacheSolve function similar to cachemean fuction, create for matrix instead of vector

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getMatrixInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setMatrixInverse(m)
        return(m)
}

## run program to test. Note: my work directory is one level up to "ProgrammingAssignment2" folder

## > source("ProgrammingAssignment2/cachematrix.R")    load R functions
## > b <- matrix(1:4, 2, 2)  create a matrix, 2 by 2
## > a <- makeCacheMatrix(b) create function
## > cacheSolve(a) 
## result:
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## run another one
## > b <- matrix(5:10, 3, 2)  create a matrix
## > a <- makeCacheMatrix(b)  create functions
## > cacheSolve(a)   
## got error:  Error in solve.default(matrix, ...) : 'a' (3 x 2) must be square 

## run another one, 3 by 3
## > b <- matrix( c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow=3, ncol=3)
## > a <- makeCacheMatrix(b)  create functions
## > cacheSolve(a)  
## result:
##       [,1] [,2] [,3]
## [1,]    1    0    5
## [2,]    2    1    6
## [3,]    3    4    0

## run another one, 4 by 4
## > b <- matrix( c(3, 1, 4, 5, 0, 2, 0, 0, 2, 0, 6, 2, -1, -2, -3, 0), nrow=4, ncol=4)
## > a <- makeCacheMatrix(b)  create functions
## > cacheSolve(a)  
## result:
##      [,1] [,2] [,3] [,4]
## [1,]  0.6  0.0 -0.2  0.0
## [2,] -2.5  0.5  0.5  1.0
## [3,] -1.5  0.0  0.5  0.5
## [4,] -2.2  0.0  0.4  1.0
