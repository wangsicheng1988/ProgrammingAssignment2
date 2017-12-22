## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" that caches the inverse of the matrix:
## "x" represents the special "matrix."
## Since the assignment assumes that the matrix supplied is always invertible,
## I assign "inv <- NULL."
## "x <<-y" creates "x", which is got by "get()."
## "set" and "get" can create and cache the "matrix."
## "setinv" and "getinv" create and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)    
}


## Write a short comment describing this function
## This function "cacheSolve" can operate to compute the inverse of the matrix created by the above function.
## First, it tests the matrix. 
## If the inverse returned by the above function is not NULL, then this function retrieves the inverse from the cache.
## Otherwise, cacheSolve computes the inverse of the matrix by using "solve" function.

cacheSolve <- function(x, ...) {
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
