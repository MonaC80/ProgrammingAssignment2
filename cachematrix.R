##caching the inverse of a matrix may by costly from a computational point of you, especially for large matrix
## this function creates a matrix object which can cache its inverse
makeCachceMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## this function the invesre of the matrix created above. If the inverse has already been calculated,
## then my function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
