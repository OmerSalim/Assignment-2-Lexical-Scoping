
## The function "makeCacheMatrix" creates a new, unique environment. 
# The inverse matrix is cached inside the object m, within the main 
# environment, which is unique for EACH instance the function is called.
## The output of the function is a list with 5 named elements, which are 
# the five functions defined herein: setmatrix, getmatrix, setinverse, 
# getinverse and getenv
###################################

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL   
  evn <- environment()  
  y<-NULL 
  
  setmatrix<-function(y){  
    x<<-y  
    m<<-NULL 
  }
  
  getmatrix<-function() x  # Get the matrix value cached with setmatrix
  setinverse<-function(solve) m<<- solve  # Cached value of inverse matrix is saved in m
  getinverse<-function() m  # Get the saved value of inverse matrix m that was saved with setinverse
  getenv<- function() environment()
  
  list (setmatrix=setmatrix, getmatrix = getmatrix, # creates list to house the four functions  
        setinverse = setinverse,
        getinverse = getinverse,
        getenv = getenv)
  
}

###################################
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function
###################################

cacheSolve <- function(xMat= m(), ...) {
  
  m <- xMat$getinverse() # if an inverse has already been calculated this gets it
  if(!is.null(m)){ # check to see if cacheSolve has been run before
    if(xMat$setmatrix() == xMat$getmatrix()) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
      message("getting cached data")
      matrix<-xMat$get()
      m<-solve(matrix, ...)
      xMat$setmatrix(m)
      return(m) 
    }
    # otherwise 
    y <- xMat$getmatrix() # run the getmatrix function to get the value of the input matrix
    xMat$setmatrix(y) # run the setmatrix function on the input matrix to cache it
    m <- solve(y, ...) # compute the value of the inverse of the input matrix
    xMat$setinverse(m) # run the setinverse function on the inverse to cache the inverse
    m # return the inverse
  }
  # End
}