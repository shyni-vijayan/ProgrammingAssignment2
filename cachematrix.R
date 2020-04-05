##Below is a pair of fucntions that computes the inverse of a matrix and 
##caches the inverse for later use

##Function makeCacheMatrix() below: 
##
##   This function is used to create a matrix object
##   Input -- A matrix
##   Returns -- A list with 4 functions. 
##   This list is used as an input to the next function cacheSolve().
##   The functions in the list are for the following:
##         1.Set the Matrix
##         2.Get the Matrix
##         3.Set the Inverse
##         4.Get the Inverse


makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  
  ##Define the 4 functions to be returned as alist
  
  ##Set function: Sets the value of x and n
  set <- function(z) {
    x <<- z
    n <<- NULL
  }
  
  ##Get function: Just Returns the value of x
  get         <- function() x
  
  ##Setinverse function : Calculates inverse and assigns to n
  setinverse  <- function(solve) n<<-solve
  
  getinverse  <- function() n
  
  ##Return a list with 4 functions
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}



##  Function cacheSolve() below: 
##
##   This function is used to compute the inverse of a matrix 
##   Input -- Is the output by above function makeCacheMatrix()
##   Returns -- The inverse of the matrix which was input to makeCacheMatrix() 

cacheSolve <- function(x, ...) {
  
  ## Call the getinverse function and see if inverse is already calculated
  ## If yes, use the cached data
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  
  ##If inverse is not already available, call the get functon to get the matrix.
  data <- x$get()
  
  ##compute the inverse of the matrix and return the inverse.
  n <- solve(data, ...)
  x$setinverse(n)
  n
}

