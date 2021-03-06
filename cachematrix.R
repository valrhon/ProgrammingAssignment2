## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix", which returns a list
## that contains functions to set the matrix, get the matrix, set the 
## inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inv) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)

}


## Write a short comment describing this function

## cacheSolve calculates the inverse of the special "matrix" created with
## makeCacheMatrix function.

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
