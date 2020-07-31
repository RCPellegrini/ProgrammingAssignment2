## Two functions that are used to create a special object that stores a 
## matrix and caches the inverse.

## The first function, `makeCacheMatrix` a cache from a matrix, which is
## really a list containing a function to

## 1.  set the matrix
## 2.  get the matrix
## 3.  set the values of the inversed matrix
## 4.  get the value of the inversed matrix

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

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

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

