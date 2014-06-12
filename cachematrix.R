##This two functions sets and caches the inverse of a square invertible matrix 'x'

## makeCacheMatrix takes a square invertible matrix as argument and produces a special matrix which is a list composed of functions which:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse , (by calling the R function 'solve' )
# get the value of the inverse
##All packaged in a particular environment.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
#Takes the result of makeCacheMatrix as argument,
#and get the inverse from calculation the first time, then 
#cache it ('setinverse') and call the cache after with a cool message.
cacheSolve <- function(x, ...) {      
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
