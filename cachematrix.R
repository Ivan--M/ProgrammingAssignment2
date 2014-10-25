

## Cashing functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(mean) m <<- mean
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Actual cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()#check computed matrix
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #show if already has been computed
  }


  x$setInv(solve(x$get())) #set cache
  
  x$getInv() #show result
}
