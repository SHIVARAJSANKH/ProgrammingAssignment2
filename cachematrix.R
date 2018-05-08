## The following functions primarily does 2 things 

## One it creates a cache of the inverse of a matrix and two checks if inverse exists and 
### return the inverse from the cache 

## Creates a cache of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
                x <<- y
                inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of input matrix 'x'
   inv = x$getinv()
#check if available in cache
  if (!is.null(inv)){
   
   message("getting previously computed cached data")
    return(inv)
  }
  
  # otherwise, freshly calculates the inverse value 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache using  the setinv function.
  x$setinv(inv)
  return(inv)
}
