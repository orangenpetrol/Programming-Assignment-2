## A pair of functions that cache the inverse of a matrix.
## It is assumed that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  vni <- NULL
  
  set <- function(y)
  {
    x <<- y
    vni <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() vni
  
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{ ## Returns a matrix that is the inverse of 'x'
  vni <- x$getInv()
  if(!is.null(vni))
  {
    message("getting cached data")
    return(vni)
  }
  data <- x$get()
  vni <- solve(data, ...)
  x$setmean(vni)
  vni
}
