## Put comments here that give an overall description of what your
## functions do
## Programming assignment #2 in Coursera "R Programming" week 3 - 
## Cashing inverse of a matrix
## Write a short comment describing this function
## This function creates a special matrix to cash its inverse
makeCacheMatrix <- function(x = matrix()) {     ## define the new function and initialize x as a matrix object
        inv <- NULL                             ## initialize NULL value to inv, later will hold value of the inverse matrix
        set <- function(y) {                    ## set will assign the new value of the matrix if present
                x <<- y                         ## then reset inv
                inv <<- NULL
                           }
        get <- function() x                     ## define get function to return the value of matrix
        setinv <- function(inverse) inv <<- inverse     #assigns value of inv in parent environment
        getinv <- function() inv                        #get value of inv in cirrent environment
        list(set = set, get = get, setinv = setinv,     #put functions is a list to use $ op later
                                        getinv = getinv)
                                          }


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" given by makeCacheMatrix.
## If inverse has exists in the list returned by the above function or still the same
##cacheSolve will retrieve the inverse from cache
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

        inv <- x$getinV()
    if(!is.null(inv)) {
        message("retreiving cached matrix")
        return(inv)
                      }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv                        
                                
                            }
