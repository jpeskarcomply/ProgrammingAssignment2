#This function creates a matrix object that can make a cache of the inverse 
#This is a matrix 
#which sets the value of a matrix
#gets the value of a matrix
# sets the inverse of the matrix
#gets the value of the matrix

##Returns: AS a list containing the following functions:
        # set the matrix
        # get the matrix
        # set the inverse
        # get the inverse

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get=get, 
         setInverse = setInverse,
         getInverse = getInverse)
    
}
#This function utalizes the first function and a matrix passed into the construction of the first
#function to calculate the inverse of an inversable matrix. Prior to calculating the inverse
#the function check to see if the data is already in memory. This is all done using the list 
#output of the first function. 

#Returns: The inverse of the input matrix

cacheSolve <- function(x, ...) { 
         # Return a matrix that is the inverse of 'x' 
  inv <- x$getInverse() #get the matrix iverse using the passed in list of functions
  
  # Test to see if inv is an empty, if not get cached copy
  if(!is.null(inv)) {
    message("Retrieving cached copy")
    return(inv)
  }
  
  #if not cached, compute the inverse
  dat <- x$get()    #use the get function from the passed in list functions
  inv <- solve(dat) #use the solve function to compute the inverse
  x$setInverse(inv) #put the inverse into memory for future usage
  return(inv)
} 

#Usage of these two functions:
#1) create a matrix x <- rbind(c(1,2), c(2,1))
#2) make list of fn's and matrix
#   m <- makeCacheMatrix(x)
#3) Solve the inverse cacheSolve(m)
#4) enjoy the results!
#5) repeat step 3
#6) drink a beer and smile a smug smile. You have pulled data from memory!



  
 
