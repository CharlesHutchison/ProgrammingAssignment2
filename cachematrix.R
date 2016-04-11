
## makeCacheMatrix creates a special matrix that can be cached for more efficient computation.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <- NULL
    }
    
    get <- function() x
    setmatrix <<- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve creates the inverse of the cached matrix from the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
      
        m <- x$getmatrix()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
       }
       
       matrix <- x$get()
       m <- solve(matrix, ...)
       x$setmatrix(m)
       m
}
