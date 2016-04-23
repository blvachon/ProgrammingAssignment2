# This functions taken together allow the end-user to cache the results of a an
## expensive matrix inversion call.

## makeCacheMatrix bundles together the matrix data and functions that can operate
## on it to save and retrieve the result of the matrix inversion 

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  
  set <- function(y) 
  {
    ## the <<- operator scans up from the current environment through the parent 
    ## environments looking for the named variable (left hand side) so that we
    ## can save the data (right hand side) there
    x <<- y
    m <<- NULL      ## clear cached inversion result  
  }
  
  get <- function() 
  {
    x
  }
  
  set_inverse <- function(inverse)
  {
    m <<- inverse
  }
  
  get_inverse <- function() 
  {
    m
  }
  
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## cacheSolve retrieves and returns the previously computed inverse (if available) 
## if the matrix has not previously been inverted then it does so and saves the 
## result for future reference

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  
  if (!is.null(m)) 
  {
    message("getting cached inverse")
    return (m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  
  x$set_inverse(m)
  m
}
