## Assignment: Programming Assignment 2: Lexical Scoping
## written by Yeongmin Jang 
## SHA-1 hash identifier : 69030f8d8cbe1398dab1a306ba99b3d9345d14d5

## [makeCacheMatrix] 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
      
		set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## [cacheSolve] 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        m <- x$getInverse()
		
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
        data <- x$get()
        m <- solve(data, ...) # make inverse matrix
        x$setInverse(m)
        m
}
