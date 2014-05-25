## The following functions allow to cache the inverse of a matrix in order
## to avoid computing it repeatedly

## "makeCacheMatrix" generates a "matrix" object (list) from a given matrix
## and caches the related data (including inversed matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## Create a variable to store cached matrix, set it to NULL
  set <- function(y) {
    ## Set the cached matrix
    x <<- y  ## Assign passed object to cached matrix
    inv <<- NULL  ## Reset cached inverse matrix
  }
  get <- function() x  ## Return cached matrix
  setinv <- function(inv) {
    inv <<- inv  ## Assign passed object to cached inverse matrix
  }
  getinv <- function() inv  ## Return cached inverse matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  ## Return "matrix" object (list)
}


## "cacheSolve" retrieves cached inverse matrix, recalculates if no cached data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()  ## Retrieve cached inverse matrix
  if(!is.null(inv)) {  ## Test cached inverse matrix is not NULL
    message("getting cached data")  ## Message to user
    return(inv)  ## Return cached inverse matrix
  }
  ## If no cached inverse matrix
  data <- x$get()  ## Retrieve cached matrix
  inv <- solve(data, ...)  ## Invert matrix
  x$setinv(inv)  ## Set cached inverse matrix to computed object
  inv  ## Return computed inverse matrix
}