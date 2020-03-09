#This R script produces two functions and three test matrices. The purpose of this 
#script is to take an invertible matrix as input and provide the inverse of the matrix 
# as output. Utilizing the results of this script requires two function calls for 
# each matrix. See the "HOW TO USE" section below for instructions on calling 
# functions.

#I. This first function, makeCacheMatrix, produces a list of four functions that
# are intended to be consumed by the cacheSolve() function below. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) {
        m <<- solve
    }
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#II. The second function, cacheSolve(), takes the captured output of makeCacheMatrix() 
#and checks cache memory to see if the inverse for that same input matrix has already been 
#processed. If it has, then the cached inverse is returned. If not, then 
#the inverse of the matrix is calculated and then returned.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

#III. HOW TO USE THESE FUNCTIONS:
#1. Provide an invertible matrix as input and create an assignment statement to 
#   capture the output from makeCacheMatrix(). 
#   Example:  matinv1 <- makeCacheMatrix(mat1)
#2. Call the function cacheSolve() using the output variable from Step 1 above. 
#   The output from cacheSolve() will print to the screen.
#   Example:  cacheSolve(matinv1))
#3. Repeated calls of cacheSolve() that reference the same matrix 
#   will return the inverse from cache. This output will be preceded by a message 
#   "getting cached data". 


#IV. Sample test matrices and their inverses:
mat1 <- matrix(c(.5, -.25, -1, .75), nrow = 2, ncol = 2)
# > solve(mat1)
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
mat2 <- matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2)
# > solve(mat2)
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
mat3 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
# > solve(mat3)
#      [,1] [,2]
# [1,]    3    7
# [2,]    1    5
