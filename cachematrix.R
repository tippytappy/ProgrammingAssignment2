## Two functions that either retrieve a matrix inverse where it exists 
## or calculate and retrieve it where it doesn't (thus reducing processing time)

## Function 1: Create a list vector of callable functions that can be used by Function 2
makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y){    ## assign values to the parent frame variables 'x' and 'm' 
    x <<- y
    m <<- NULL
  }  
  get <- function(){    ## retrieves the original matrix
    x
  }  
  setinv <- function(matinv){    ## assigns the inverse matrix value to the parent frame variable
    m <<- matinv
  }   
  getinv <- function(){    ## retrieves the inverse matrix value
    m  
  } 
  ## finally, create the list of the functions created above
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv)
}

## Function 2: calculate / retrieve the matrix inverse using the functions created above
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)    ## Matrix inversion
  x$setinv(m)
  m
}