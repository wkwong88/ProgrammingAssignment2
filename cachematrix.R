##  R Programming - Week 3 - Programming Assignment 2  (Wing Kwong)
##  Function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
##  Function 'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix'.
##     But if the inverse has already been calculated (and the matrix has not changed), then the cached 
##     inverse matrix is returned.


##  This function creates a special "matrix" object that cache and returns the original supplied matrix, 
##  and also computes and cache the inverse of the supplied matrix.

makeCacheMatrix <- function(x = matrix()) {
    cached_original_matrix_inverse <<- NULL
    cached_original_matrix <<- x
    cached_original_matrix_inverse <<- solve(x)
    x
}


##  This function computes the inverse of the special "matrix" returned by the function 'makeCacheMatrix'. 
##  If the inverse has already been calculated (and the matrix has not changed), then this function 
##  should retrieve the inverse from the cache (thereby eliminating the need to compute it again).

cacheSolve <- function(x, ...) {
    inverse_matrix <- NULL
    if( !is.null(cached_original_matrix_inverse) & identical(x, cached_original_matrix) )  {
          inverse_matrix <- cached_original_matrix_inverse 
    }
    else {
          inverse_matrix <- solve(x)
    }
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix
}