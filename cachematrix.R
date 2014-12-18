## R Programming Assignment 2
## Two functions to illustrate caching implementation to speed up 
## compute intensive operations that need to be repeated often

## special function to cache matrix and its inverse
## can be accessed with internal functions set, get, setinv and getinv
## caches matrix and its inverse using special operator <<

makeCacheMatrix <- function(x = matrix()) {

    i      <- NULL
    set    <- function(y) {
              x <<- y
              i <<- NULL
    }

    get    <- function()    { x }
    setinv <- function(inv) { i <<- inv }
    getinv <- function()    { i }

    list(set    = set, 
         get    = get,
         setinv = setinv,
         getinv = getinv)
}

## Function to return Inverse of Matrix efficiently
## returns inverse if found in cache (from previous compute) 
## computes inverse if not found in cache, stores inverse in cache
## and returns inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

     inv <- x$getinv()
     if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
     }
  
     data <- x$get()
     inv  <- solve(data, ...)
     
     x$setinv(inv)
     inv
}
