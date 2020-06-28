## Assignment: Caching the Inverse of a Matrix
## The aim of following two functions is:
## 1) to create a special object that can cache a matrix and its inverse.
## 2) to compute the inverse of the "matrix" in case the inverse matrix has not already been created 

## In first function we create a function "makeCacheMatrix" which stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
matrix_inv <- NULL
        set <- function(y) {
                x <<- y
               matrix_inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) matrix_inv <<- inverse
        getInv <- function() matrix_inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)

}


## In the second function "cacheSolve" we create a function which firstly checks if the inverse function has already been created.
##If not, it computes and returns the inverse matrix.

cacheSolve <- function(x, ...) {
         matrix_inv <- x$getInv()
        if (!is.null(matrix_inv)) {
                message("getting cached data")
                return(matrix_inv)
        }
        mat <- x$get()
        matrix_inv <- solve(mat, ...)
        x$setInv(matrix_inv)
        matrix_inv
}
##Now we test the two functions. Let's create the object my_matrix which contains the function makeCacheMatrix and a matrix (numbers from 1 to 4, 2 rows, 2 columns)
my_matrix<-makeCacheMatrix(matrix(1:4,2,2))
## with the argument my_matrix$get() we print the matrix
my_matrix$get()
  ##   [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## Let's find the inverse
my_matrix$getInv()
NULL  #the result is NULL.  We have to compute the inverse with the second function
cacheSolve(my_matrix)
 ##   [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5  This is the inverse matrix. If we want, we can check by using getInv
my_matrix$getInv()
 ##    [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5  The inverse matrix is the same as the previous one
## Wow we can set a different matrix (numbers from 4 to 7, 2 rows, 2 columns)
my_matrix$set(matrix(c(4:7), 2, 2))
my_matrix$get()
   ##  [,1] [,2]
##[1,]    4    6
##[2,]    5    7 This is our new matrix
my_matrix$getInv()
NULL 
## As before we have to run the cacheSolve function to get the inverse matrix
cacheSolve(my_matrix)
  ##   [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2
my_matrix$getInv()
##[,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2
        
