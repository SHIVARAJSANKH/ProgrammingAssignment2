## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
