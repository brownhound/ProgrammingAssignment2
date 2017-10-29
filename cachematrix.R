## Functions will take what is assumed to be an invertible matrix (no
## validation on input format), check if the inverse exists.  If the inverse
## is already stored, it will return the stored value.  If it is not stored, 
## it will calculate and return the inverse of the matrix.


## Takes an invertible matrix, nulls the invert variable.  Can set, get, set 
## the inverse of the matrix, or get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) invert <<- inverse
    getinverse <- function() invert
    list(set = set, get = get, setinverse = setinverse, 
          getinverse = getinverse)
}


## Checks if invert is not null, and reports value if so.  If null, calculates
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
    invert <- x$getinverse()
    if(!is.null(invert)){
        message("getting inverse matrix")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setinverse(invert)
    invert
}
