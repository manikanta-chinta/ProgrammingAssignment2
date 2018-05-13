## Below functions on a high level give the inverse of a matrix. 
## If the inverse is already calculated for the matrix then the value is fetched from the cache instead of recalculating the value again

## To store the calculated inverse matrix value in the cache to return the value store in cache if inverse function is called again

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##above code is setting the new matrix values and clearing the cache 
  get <- function() x
  setInvM <- function(solve) m <<- solve
  getInvM <- function() m
  ## above code is calculating the inverse of the new matrix and setting the value to setInvM element
  list(set = set, get = get,
       setInvM = setInvM,
       getInvM = getInvM)
}


## To return the value of the inverse of a matrix if already calculated before if not then calculate and return the inverse of a matrix and to store it in cache

cacheSolve <- function(x, ...) {
  m <- x$getInvM()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## above code is searching the cache to see if inverse is already calculated before
  
  data <- x$get()
  m <- solve(data,...)
  x$setInvM(m)
  m
  ## above code calculate is calculating the iniverse, returing the value and storing it in the cache
}
