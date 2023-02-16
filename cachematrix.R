## makeCacheMatrix creates a wrapper around a matrix that allows its inverse
## to be cached. Calling cacheSolve on the 'wrapped' matrix, returns the inverse
## of the matrix. The inverse is returned either by fetching the already 
## calculated, cached, inverse, or by calculating (and then caching) the inverse.

## Creates a matrix where the inverse can be cached. The object created is a 
## list of four functions 
## get returns matrix
## set sets matrix
## getinverse returns the calculated inverse (or NULL if not calculted)
## setinverse sets the calculated invers
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates inverse of cachable matrix if not cached else uses cached value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinverse(inv)
    inv
}

A = matrix(c(2, 4, 3, 1), nrow=2, ncol=2) 









