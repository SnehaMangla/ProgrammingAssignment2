## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
   ##been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

## makeCacheMatrix creates an R object that stores a square matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## cacheSolve() takes argument that is returned by makeCacheMatrix() in order to retrieve 
## the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
