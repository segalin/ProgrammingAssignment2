## These functions store a matrix and calculate its inverse when
## it is requested for the first time. This inverse is also stored 
## so that it is not necessary to calculate it every time it is requested. 

## This function stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse matrix of the parameter x. 
## If there is no stored inverse, this funtion calculates the 
## inverse ande stores it. Otherwise it just returns the inverse 
## matrix stored.

cacheSolve <- function(x, ...) { 
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
    
        inv
    }
    
    else {
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv
    }
}
