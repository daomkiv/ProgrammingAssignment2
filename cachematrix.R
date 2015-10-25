## The function caches the inverse of a matrix and store it for
## later use.

## The 1st function is a collection of functions. It stores the
## inverse in cm, which started as being NULL

makeCacheMatrix <- function(x = matrix()){
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  get <- function() x
  setInv <- function(inv) cm <<-inv
  getInv <- function() cm
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)  
}


## The 2nd function calls bach the inverse in cache, or if it is
## NULL, calculate the inverse

cacheSolve <- function(x, ...) {
  cm <- x$getInv()
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  data <- x$get()
  cm <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
  x$setInv(cm)
  cm
}

