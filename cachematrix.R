## The objective of this program is to accept square matrix
## and calcuate the inverse of the matirx. In order to make this operation
## fast and efficent, the function stores the resulti the cache and prevents
## re-computation of the inverse of the same input matrix.
## For this, lexical scoping rule of R is used.

## makeCacheMatrix constructs a 2x2 sqaure matrix 
## and calculates the inverse of the matrix
## Author: abmonga
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y)  {     ## constructs a (2x2)matrix
    x <<- matrix(y,2,2) 
    m <<- NULL               ## 
  }
  get <- function () x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,setInv = setInv,getInv = getInv)
}


## cacheSolve checks whether inverse of the matrix has been computed
## earlier and displays the result from cache if computed before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if (!is.null(m)){               ## check cache for result
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data)    ## computes inverse
  x$setInv(m)
  m
}
