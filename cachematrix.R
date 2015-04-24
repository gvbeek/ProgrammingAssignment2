## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
## This program uses a pair of functions that cache the inverse of a matrix.




## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function ( x = matrix() ) {
  
  # initialize on an empty inverse
  i <- NULL
  
  # add the matrix to the cache (and clear the inverse from the cache)
  setmat = function(y) {
           x <<- y
           i <<- NULL
           }
  
  # add the inverse to the cach
  setinv <- function (inv) {
            i <<- inv
            }
  
  # get the matrix from the cache
  getmat <- function() {
            x
            }
  
  # get the inverse from the cache
  getinv <- function() {
            i
            }
  
  # return the cache
  list ( set     = setmat
       , get     = getmat
       , setinv  = setinv
       , getinv  = getinv
       )

}






## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

  # get the inverse from the cache
  i <- x$getinv()
  
  # if it exists, take it from the cache and return it to the function call
  if ( !is.null(i) ) {
        message("getting cached data")
        return(i)
        }
  
  # else, read the matrix, take it's inverse, add it to the cache, and return it to the function call
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
    
  i
}


## example

a = matrix (c(1,2,3,4), nrow=2)
b = makeCacheMatrix(a)
c = cacheSolve(b)


