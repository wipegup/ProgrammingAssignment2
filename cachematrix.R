## Two Functions - makeCacheMatrix, and cacheSolve
## Designed to solve and store the inverse of an invertable matrix
## to reduce required computation

## makeCacheMatrix creates a matrix object with four functions and stores
##       a matrix and its inverse
## "set(mtx)" will defines the value of the matrix (as "mtx") after initial construction
##       (and reset invert matrix data)
## "get()" returns the value of the matrix
## "setInv(inverse)" sets the value of the invert matrix to "inverse"
## "getInv()" returns the value of the invert matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(mtx) {
        x <<- mtx
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse){
        inv <<- inverse
    }
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Given an object of makeCacheMatrix, cacheSolve solves for the inverse
## then stores the invert matrix back into the makeCacheMatrix object.
## First check if inverse stored, if so, print return inverse
## else get() data from mCM object, then solve() using that data
##      setInv() in the mCM object and return inverse

cacheSolve <- function(x,...) {
    inv <- x$getInv()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setInv(inv)
    inv
}