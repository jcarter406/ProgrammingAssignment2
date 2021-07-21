##Week 3 Assignment 2

##This pair of functions will first create a cache to save results of an inverse matrix
##function then use the second function to ensure cached matrix is the inverted version.

##Remind R to use the cachematrix functions

source("cachematrix.R")

##FUNCTION 1: Creating a cache to save results of an inverse matrix function.

##First two lines operationalize two objects
##x as a function argument with default setting of empty matrix and 
##m as an empty object in the makeCacheMatrix (parent) environment with a 
##default null value. Both objects can be altered by later code in the function 

makeCacheMatrix <- function(x = matrix(x)) {
      m <- NULL
      
##Set and Get are action verbs and will first address x then m 
##Set x will assign values to the X and m objects in the parent environment
## If value of x changes, the m value will be re-calculated by the Set function
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
##Get x will find the value of x in the parent environment 
      
      get <- function() x
      
##Set m will indicate the function that will be applied to m and will use the 
##double arrow assignment operator to indicate function should use the value of 
##m that is in the parent environment each time
##Get m will get the value of m from the parent environment
      
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      
##List will be used put the four functions as elements into a list that will be 
##placed in the parent environment where it can be used by other functions
##the addition of names will allow the use of the $ extractor
      
      list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
}
##Final outcome of this first set of codes is the makeCacheMatrix() which will 
##be needed in the second function listed below  

##FUNCTION 2: This function is needed to calculate inverse matrices or 
##retrieve those already cached

##Initially, x is used as an argument for cacheSolve and the triple dots indicate
##additional arguments can be passed to the new function

cacheSolve <- function(x, ...) {

##before calculating the inverse matrix, this will get the value of m stored in  
##parent environment 

  m <- x$getinverse()

##if statement is used to test whether cached m value is default null or a value
##based on a new matrix value. The process used logical TRUE / FALSE output

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
##In the latter case of FALSE,  the makeCacheMatrix function gets the new value 
##of m and completes its steps to cached that new inverted matrix
##The last part of the second function is where the actual inversion of the matrix
##occurs
  
  matrix_to_invert <- x$get()
  m <- solve(matrix_to_invert, ...)
  x$setinverse(m)
  m
}
##Output will be m which is the inverted matrix of the matrix submitted to 
##the two function described above

