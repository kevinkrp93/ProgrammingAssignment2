## Put comments here that give an overall description of what your
## functions do

## function that obtain,intialize,invert and produces the inversed matrix


makeCacheMatrix <- function(x = matrix()) 
{

	z <- NULL
        setmatrix <- function(y) 
	{
                x <<- y
                z <<- NULL
        }
        getmatrix <- function() x
        inverse <- function(solve) z <<- solve
        getinverse <- function() z
        list(setmatrix = setmatrix, getmatrix = getmatrix,inverse = inverse,getinverse = getinverse)

}


## checks if the matrix is already inversed and if it isn't it produces the inversed matrix

cacheSolve <- function(x, ...) 
{
        
        z <- x$getinverse()
        if(!is.null(z)) 
	{
                message("getting cached data")
                return(z)
        }
        matrix_1 <- x$getmatrix()
        z <- solve(matrix_1, ...)
        x$inverse(z)
        z
}

}
