#Special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
    x <<- y
    n <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) n <<- inverse
  getInverse <- function() n 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


#computes the inverse of the matrix used returned by the function above

cacheSolve <- function(x, ...) {
  n <- x$getInverse()
  if(!is.null(n)){
    message("getting cached data")
    return(n)
  }
  mat <- x$get()
  n <- solve(mat,...)
  x$setInverse(n)
  n
}
