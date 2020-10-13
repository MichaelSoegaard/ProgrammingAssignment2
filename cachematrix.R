#Here is a set of functions which takes a matrix as argument, calculates the inverse matrix.
#The variable you chose for output will store and the input matrix, the inversed matrix, as well
#as all the functions used for getting and setting values if you choose to input a new matrix. 


#'makeCacheMatrix' takes a matrix as argument and outputs functions to variable. 'invm' stores
#the inversed matrix, if it has been calculated othervise it will just be NULL.
#To use the functions call 'makeCacheMatrix' with you matrix as an argument and store it in
#a new variable. Next call the 'cacheSolve' with your variable as an argument to get the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        set_inv_matrix <- function(matrix) invm <<- matrix
        get_inv_matrix <- function() invm
        list(set = set, get = get,
             setmatrix = set_inv_matrix,
             getmatrix = get_inv_matrix)
}


## 'cacheSolve' takes the output from 'makeCacheMatrix', outputs the inversed matrix and stores
## it in the output variable from 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
        invm <- x$getmatrix()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setmatrix(invm)
        invm
        ## Returns a matrix that is the inverse of 'x'
}
