## makeCahceMatrix()
## soveCache()
##
## The first function "makeCacheMatrix" creates a special 
## matrix object that can cache it's inverse.  The function
## resets the cached inverse value to NULL if the value of
## the matrix is changed (either when it is initialised or
## through using the "set" function.  
## 
##
## The second function "cacheSolve" computes the inverse of a
## special matrix object created using the "makeCacheMatrix"
## function. cacheSovle will check to see if a value for the
## inverse has already been cached and if so will return the
## cached value.  If there is no cached value then it will
## calculate the inverse of the special matrix object using
## the "solve" function

## The makeCacheMatrix function creates the special matrix object
## that can cache it's inverse as a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

   ## initailise inverse variable (i) to NULL
   i <- NULL

   ## set function - sets the value of the matrix based on the
   ## supplied agrument value (y) and cache it's value
   ## sets the cached value of the inverse of the matrix to NULL
   set <- function(y) {
       x <<- y
       i <<- NULL
   }

   ## get function - returns the cached matrix value  
   get <- function () x

   ## setinverse - sets the value of matrix inverse to the argument
   ## provided (inverse) and caches the value
   setinverse <- function(inverse) i <<- inverse

   ## returns the cached inverse value 
   getinverse <- function() i

   ## returns special matrix object as a list
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)


}


## cacheSovle will check to see if a value for the
## inverse for the special matrix object passed as an argument
## has already been cached and if so will return the
## cached value.  
## If there is no cached value stored for the special matrix
## object then it will calculate the inverse of the special 
## matrix object using the "solve" function, set the vaule
## of the inverse (calling setinverse) and return the
## resulting inverse matrix value


cacheSolve <- function(x, ...) {

    ## retrieve the cached value for the inverse of the special
    ## matrix object x using the getinverse function
    i <- x$getinverse()

    ## test to see if the cached inverse value is non-NULL
    ## if there is non-NULL cached value print a message to
    ## screen and return the cached value
    if(!is.null(i)) {
        message("getting inverse of matrix")
        return(i)
    }

    ## if the cached value of the inverse is NULL retrieve the
    ## value of special matrix object x
    data <- x$get()

    ## calculate the inverse of x using the solve function
    i <- solve(data)

    ## cache the inverse value of x using the setinverse function
    x$setinverse(i)
 
    ## Return the matrix 'i' that is the inverse of 'x'
    i
}
