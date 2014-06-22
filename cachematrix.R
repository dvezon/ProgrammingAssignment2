## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
## The next 2 functions is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(y) {                                    ## implements the set method
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x                                     ## implements the set method
        setinverse <- function() inverse_x <<- solve(x)         ## implements the setinverse method
        getinverse <- function() inverse_x                      ## implements thesetinverse
        
        ## Returns a list with the inverse matrix
        
        list(set = set, get = get,             
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse_x <- x$getinverse()
        if(!is.null(inverse_x)) {                       ## check if inverse has already calculated
                message("getting cached data")          ## If yes return inversed value
                return(inverse_x)
        }
        data <- x$get()                                 ## If not calculated the inverse matrix and return it
        inverse_x <- solve(data, ...)
        x$setinverse(inverse_x)
        inverse_x
}