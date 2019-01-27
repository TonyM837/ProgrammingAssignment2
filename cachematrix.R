## The first function creates a 'matrix' object that can cache its inverse.  
## The second function calculates and creates a cache of the inverse

## Creating a list of functions that set and return the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    
    setMatrix <- function(y) {
        if (!identical(all.equal(x,y), TRUE)) {
            x <<- y
            x_inv <<- NULL
        }    
    }
    
    getMatrix <- function() x
    
    setInverse <- function(inverse_matrix) x_inv <<- inverse_matrix
    
    getInverse <- function() x_inv
    
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Retrieves the inverse of matrix from cache if already calculated or
## calculates it (in this case also stores it in cache for future use)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cm_inv <- x$getInverse() 
    
    if (!is.null(cm_inv)) {
        message("retrieving from cache")
        return(cm_inv)
    }
    
    cm_inv <- solve(x$getMatrix())
    x$setInverse(cm_inv)
    cm_inv
}
