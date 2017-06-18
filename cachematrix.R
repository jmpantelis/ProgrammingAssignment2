## cachematrix.R - contribution for programming assigment 2
## By Jorge M Pantelis
##
## To make use of inversion of large matrices more efficient, a caching
## function will store the inverse of a matrix in a higher R environment
## using the <<- assignment option.  When the inverse of a function is
## necessary we can invoke the cached version of a matrix we are using.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse. This is using object oriented principles.
## It offers 4 methods: set() a copy of the matrix in higher environment 
## in 'x', get() the matrix stored in higher environment in 'x', 
## setinverse() generates the inverse of the matrix and stores it in 
## higher environment in 'm', getinverse() returns the previously inverted
## matrix stored in higher environment in 'm'.

makeCacheMatrix <- function(m = matrix()) {
    inv_m <- NULL
    
    ## Set initial matrix in <<'x' and default the inverse to NULL 
    set <- function(y) {
        ## this is an 'm' copy of matrix 'y' in higher environment
        m <<- y
        inv_m <<- NULL
    }
    
    get <- function() m
    setinverse <- function(solve) inv_m <<- solve
    getinverse <- function() inv_m
    
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of an invertable matix (assume this condition). 
## if the inverse, is already computed, fetch it from object 'm'.  'm'
## must be an 'CacheMatrix' 'object'. See above function.

cacheSolve <- function(m = matrix(), ...) {
    ## Return the cached inverse matrix that is the inverse of 'm'
    ## if not computed yet, proceed to compute inverse and cache it.
    inv_m <- m$getinverse()
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    
    ## if inverse was not computed, fetch the matrix and invert it.
    ## cache the inverse in m object
    matrix <- m$get()
    inv_m <- solve(matrix, ...)
    m$setinverse(inv_m)
    
    return(inv_m)
}
