## This pair of functions caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #First it creates a NULL object that will contain the inverse once it is calcullated
  inverse_x <- NULL
  #when the matrix changes the inverse is not valid so it is assigned to NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  #get returns the matrix x
  get <- function() x
  #setinverse stores a value in inverse_x and caches it (<<-)
  setinverse <- function(solve) inverse_x <<- solve
  #getinverse returns the value of the inverse matric of x
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  #read the value of the inverse matrix of x
  if(is.null(x$getinverse())) {
    #if the value of inverse_x is NULL it is neccesary to calcullate the value
    x$setinverse(solve(x$get()))
    message("Matrix changed.")
    message("Inverse re-computed")
  }
  else{
    #if the readed value is not NULL the cached inverse matrix is valid, so its value is returned and the function ends
    message("getting cached data")
  }
  x$getinverse()
}
