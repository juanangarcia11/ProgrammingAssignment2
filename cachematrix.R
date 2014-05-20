## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  CacheMatrix <<- list(x, NULL)     ##create list to cache matrix and its inverse (initialy=NULL) 
  return (CacheMatrix)

}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if (is.null( CacheMatrix[[2]] ) ){                     ## CacheMatrix has been created, inverse not calculated
      
      CacheMatrix[[2]] <<- solve(x[[1]])
      print ("New matrix, inverse calculated")
  
  } else if (!identical(CacheMatrix[[1]], x[[1]])){      ## Inverse was calculated, matrix changed
      makeCacheMatrix (x[[1]])                           ## re-initialize object to store new data (both matrix and inverse)
      CacheMatrix[[2]] <<- solve(x[[1]])             ## calculate and set inverse
      print ("Original Matrix has Changed")
      
  } else {
    
                                       ##Matrix not changed, inverse retrieve from cache
      print("inverse from cache")
      
  }
  CacheMatrix[[2]]
}
