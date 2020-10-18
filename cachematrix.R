## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  invert<- NULL                                   # initialize invert  as NULL so that it do not print garbage values
  
  set_matrix <- function(y){                      #setting the values of matrix and its inverse to the argument passed  
    x <<- y
    invert = NULL
     
  }
   get_matrix <- function() x                     #get the value of the matrix 
   
   set_invert<- function(inv){ invert <<- inv}    # set the value of the inverse after calculating it
   
   get_invert<- function() invert                  # getting the value of the inverse from the cache if it already exist 
   
   list(set_matrix = set_matrix, get_matrix = get_matrix, set_invert = set_invert, get_invert = get_invert)
   # need this in order to refer to the functions with the $ operator
}


## Write a short comment describing this function

cacheSolve <- function(x ,...) {
        ## Return a matrix that is the inverse of 'x'
        
  inv<-x$get_invert()
  
  if(!is.null(inv)){
    message("wait!!! fetching from cache.")
    return(inv)
  }
  
  mat<-x$get_matrix()
  
  inv<-solve(mat,...)
  
  x$set_invert(inv)
  
  inv
}

cacheSolve(x)
