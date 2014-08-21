## makeCacheMatrix

## Matrix inversion is usually a costly computation and their 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. This source file contains a pair 
## of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## Setter for new value of Matrix. It uses parent environment to store the reference
    ## It also resets the variable used to store the cahced value
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Getter for stored Matrix value
    getMatrix <- function() x
    
    ## Setter for new value of inverse. It uses parent environment to store the reference
    setInverse <- function(inverse) inv <<- inverse
    
    ## Getter for cached Inverse value
    getInverse <- function() inv
    
    ## Specialized list that has the above functions
    list(setMatrix = setMatrix, 
    		getMatrix = getMatrix,
         	setInverse = setInverse,
         	getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then it retrieve the inverse from the cache.

## This function "solves" the inverse of the matrix and sets it to the special matrix
cacheSolve <- function(x, ...) {
    ## Look at cache to see if inverse exists
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
      ## Return cached value if found
    	message ('getting cached inverse')
    	return (inv)
    }

    ## Else compute the inverse and store it in cache
    mat <- x$getMatrix()
    inv <- solve(mat)
    x$setInverse(inv)
    message ('inverse stored in cache')
    
    ## Return inverse value
    inv
}
