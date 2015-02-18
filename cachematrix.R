## Computing the inverse of a matrix can be a time consuming process. The below two functions are hence 
## defined to cache the inverse of a matrix so that it can be reused in future matrix inverse computations
## when the matrix values remain the same

## Create a matrix object that can be used to access the 4 inner functions - set(),get(),setInverse() and getInverse(). 
## The function return type is a list containing the 4 inner function names
makeCacheMatrix <- function(m = matrix()) {
  xIn<-NULL
  
  ## Set the values of the matrix m1 using the values from the function argument mArg
  set<-function(mArg){
    m<<-mArg
    xIn<<-NULL
  }
  
  ## Return the values of the matrix m
  get<-function() m
  
  ## Set inverse for a matrix. <<- operator used to treated inverse xIn as a global attribute
  setInverse<-function(inverse) xIn<<-inverse
  
  ## Compute the value of inverse
  getInverse<-function() xIn

  ## Return a list of inner function names
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  
  ##Get the inverse of the matrix
  c<-x$getInverse()
  
  ## Check to see if inverse exists. 
  ## If inverse exists return inverse from cache xIn
  if(!is.null(c)){
    message("Getting cached data..")
    return(c)
  }
  
  ##If inverse does not exist, get the matrix, compute the inverse and store the inverse in cache for future use
  data<-x$get()
  i<-solve(data)
  x$setInverse(i)
  
  ##Return inverse
  i     
}
