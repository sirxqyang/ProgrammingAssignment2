## this script was developed to cache potentially time-consuming
## computations for inverse of a matrix rather than computing it
## repeatedly.

## makeCacheMatrix was developed to creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() {
                x
        }
        setSolve <- function(solve) {
                s <<- solve
        }
        getSolve <- function() {
                s
        }
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.

## it first checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of the inverse in
## the cache via the setSolve function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
