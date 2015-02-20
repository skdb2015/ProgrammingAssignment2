## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function description:
## This function will cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## It will essentially create a two element list that contains functions
    ## for:
    ## setting the matrix
    ## getting the matrix
    ## setting the inverse of the matrix
    ## getting the inverse of the matrix
    ##
    ## Assumption: the matrix provided is always invertib
    
    ## xInv: the inverse of the cached matrix. At the time of creation, it is
    ## initialized to be NULL
    xInv <- NULL
    
    ## function to set the cached matrix
    set <- function(y){
        x <<- y
        xInv <<- NULL
    }
    
    ## function to get the cached matrix
    get <- function(){
        x
    }
    
    ## function to set the cached inverse of the matrix
    setInv <- function(inverseMatrix){
        xInv <<- inverseMatrix
    }
    
    ## function to get the cached inverse of the matrix
    getInv <- function(){
        xInv
    }
    
    ## the return of the makeCacheMatrix function: a list of set and get cache
    ## functions
    list(set = set, 
         get = get, 
         setInv = setInv, 
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
            
    ## first, see if the inverse is cached. If it is cached, return the cached
    ## value
    xInv <- x$getInv()
    
    if (!is.null(xInv)){
        message("getting cached data")
        return(xInv)
    }
    
    ## if it is not cached, compute the inverse, store the cache and return the
    ## computed inverse
    data <- x$get()
    xInv <- solve(data)
    x$setInv(xInv)
    xInv
}
