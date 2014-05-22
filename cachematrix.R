## This file contains two funtions.  The makeCacheMatrix function takes a matrix as input and
## returns a list of functions which will allow the inverse of the matrix to be stored to cache and retrieved
## The cacheSolve function returns the inverse of a matrix, using the solve function to calculate it, 
## or pulling it from the cache if available

## makeCacheMatrix is very similar to the makeVector example.  It takes a matrix as input and
## creates a list containing 4 functions wich
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the 
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve is similar to the cachemean example.  It takes a vector as input and returns its inverse
## using the solve function to find the inverse, or pulling it from the cache if available


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
