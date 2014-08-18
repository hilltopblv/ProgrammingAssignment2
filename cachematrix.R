# This function creates a special "matrix" object that can cache its inverse
# input: a matrix
# output: a cache invesed matrix
# assumpiton: input is square matrix. 
# if X is a square invertible matrix, then solve(X) returns its inverse.
# naming: 'p_' parameter/arguments/input to a function

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- matrix()    #  creaes an empty matrix to hold inverse matrix
    set <- function(y) {          # takes an input matrix
        x <<- y                   # saves the input matrix
        inverse_matrix <<- matrix()
    }
    get <- function() { x }         # this function returns the value of the original matrix
    
    setMatrix <- function (m) {inverse_matrix <<- m }     # this is called by cacheSolve () during the first cacheSolve ()
    # access and it will store the value using superassignment
    
    getMatrix <- function() { inverse_matrix } # this will return the cached value to cacheSolve() on
    #  subsequent accesses
    
    list(set = set,                 # return a list of functions 
         get = get,                       
         setMatrix = setMatrix,     
         getMatrix = getMatrix)  
}


#This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.
# input: a list of function
# output: inverse matrix of the original input

cacheSolve <- function(x, ...) {    
    m <- x$getMatrix()                   # call function to check if x is set
    
    if(length(which (is.na(m)))==0) {   # check to see if the matrix is not empty
        message("getting cached data")  # ... send this message to the console
        return(m)                       # ... and return the mean ... "return" ends 
    }
    else {
        data <- x$get()        # if x$getMatrix() returned empty matrix
        m <- solve(data)       # if m was NULL then we have to calculate the new inverse
        x$setMatrix(m)           # store the calculated inverse matrix value in x 
        return(m)               # return the inverse matrix to the code that called this function
    }
}
