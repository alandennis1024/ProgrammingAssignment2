## The makeCacheMatrix creates a list that contains set, get, setInverse, and getInverse to create a 
## simple object. 

## Creates an object that caches the inverse of a matrix when called.
makeCacheMatrix <- function(originalMatrix = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    originalMatrix <<- y
    inverseMatrix <<- NULL
  }
  get <- function() {
    originalMatrix
  }
  
  setInverse <- function(inv) {
    inverseMatrix <<- inv
  }
  getInverse <- function() {
    inverseMatrix
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Returns the inverse of the supplied matrix (if it exists) and then caches the results for future use
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m  
}

# test script
# c=rbind(c(1, -1/4), c(-1/4, 1)) 
# mat <- makeCacheMatrix(c)
# cacheSolve(mat)
# c %*% cacheSolve(mat)
