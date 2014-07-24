## R-prog005 Programming Assignment 2

## These functions are designed to calculate the inverse of a matrix and 
## cache the calculated inverse so if it needs to be calculated again, instead
## of re-doing the calculation the code will read the cache, thereby saving
## computation time


## makeCacheMatrix(x)
## This creates a cache for the inverse, and then displays a list of functions
## which do the following:
##
## "set" - set the value of the matrix
## "get" - get the value of the matrix
## "set.inverse" - set the value of the inverse
## "get.inverse" get the value of the inverse
##
## Parameters:
## x - This is the initial matrix to be evaulated. The cache data is 
## based on this.


makeCacheMatrix <- function(x = matrix()) {

    # m is the cache. This line initialises the cache
    m <- NULL
    
    # This sets a value to the matrix
    set <- function(y) {
      x <<- y
      
    # Once the value has changed, this line clears the cache
      m <<- NULL
    }
    
    # Get the original matrix
    get <- function(){
      x
    }
    # This line sets the cache value...
    set.inverse <- function(x){
      m <<- x
    }
    
    # ...and this line gets the cache value
    get.inverse <- function(){
      m
    }
    
    # Finally, this displays all of the functions in a list
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
  }


## cacheSolve(x,...)
## This function firstly tests whether a result is stored in the cache. 
## If there is a result in the cache, the function simply displays the result
## If there isn't a result in the cache, the function calculates the inverse
## of the given matrix and displays it
##
## Parameters:
## x - This is the object that stores the matrix that is to be evaluated
## ... - This is for any parameters that are to be passed onto the solve
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    # This part checks if the value is in the cache or not. If it is,
    # then it simply returns the cached value and displays a message
    # so the user knows that the value has come from the cache, and
    # has not been re-calculated.
  
    # This line gets the current value from the cache
    m <- x$get.inverse()
    
    # The following IF statement checks if the cache is null:
    # If the cache is not null, then it returns the cached data:

    if(!is.null(m)) {
      
      message("getting cached data")
      return(m)
    }
    
    # If the cache is null, then it calculates the inverse of the matrix
    # and displays it:
    data <- x$get()
    m <- solve(data, ...)
    x$set.inverse(m)
    m
}
