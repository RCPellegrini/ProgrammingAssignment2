## Two functions that are used to create a special object that stores a 
## matrix and caches the inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  if(!is.square.matrix(x) == TRUE) {
    stop("this is not a valid matrix")
    
  }
    inv_matrix <- NULL
    set <- function(y) {
      x <<- y
      inv_matrix <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv_matrix <<- solve
    getsolve <- function() inv_matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getsolve()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setsolve(inv_matrix)
  inv_matrix
  
}

