## Assignment: Caching the Inverse of a Matrix

## Computing the inverse of a square matrix can be done with the solve function
## in R. For example, if X is a square invertible matrix, then solve(X) returns
## its inverse.

## Assumptions: the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      push <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      pull <- function() x
      pushInverseMatrix <- function(InverseMatrix) inverse <<- InverseMatrix
      pullInverseMatrix <- function() inverse
      list (push = push,
            pull = pull,
            pushInverseMatrix = pushInverseMatrix,
            pullInverseMatrix = pullInverseMatrix)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve function should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
      inverse <- x$pullInverseMatrix()
      if (!is.null(inverse)) {
            message("Cached data load: success!")
            return(inverse)
      }
      myMatrix <- x$pull()
      inverse <- solve(myMatrix, ...)
      x$pushInverseMatrix(inverse)
      inverse
        
## Return a matrix that is the inverse of 'x'
        
}
