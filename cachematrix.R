#----------------------------------------------------------------------------------------
# Function Title:   makeCacheMatrix
# Description:      This function creates a special "matrix" object that can cache its inverse.
# Output:           A list of functions
#----------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(mset=set, mget=get, msetinverse=setinverse, mgetinverse=getinverse)
}


#----------------------------------------------------------------------------------------
# Function Title:   cacheSolve
# Description:      This function computes the inverse of the special "matrix" returned by
#                   makeCacheMatrix. If the inverse has already been calculated (and the
#                   matrix has not changed), then cacheSolve should retrieve the inverse
#                   from the cache.
# Input:            A list of functions
# Output:           An inverse "matrix"
#----------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
     m <- x$mgetinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$mget()
     m <- solve(data)
     x$msetinverse(m)
     m
}
