## makeCacheMatrix accepts an invertible matrix as its argument.
## From its argument, it can create a special object capable of caching its inverse
## and retrieving the cached value. 

makeCacheMatrix <- function(x = matrix()) {
        ## Initially clears cache every time makeCacheMatrix is run.
        m <- NULL
        
        ## Sets a new matrix against which the remaiing activities below can be performed
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Retrieves the value of the matrix object
        get <- function() x
        
        ## solves for the inverse of the matrix and stores the inverse value in cache
        setinverse <- function() m <<- solve(x)
        
        ## Returns the inverse value stored in cache or "NULL" if the inverse hasn't yet been calculated
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve first checks to see if the inverse of its argument ('x') exists in cache.
## This 'x' argument is the object created by the makeCacheMatrix funciton above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## If if x's inverse already exists in cache,
        ## then cacheSolve simply returns the inverse of 'x' from cache without recalculation.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
                
        ## If if the cahced value of x's inverse does not exist in cache, 
        ## thencacheSolve calculates the inverse of 'x' and returns it.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse()
        m
}
