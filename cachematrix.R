# This function will get us a cached inversed matrix.
makeCacheMatrix <- function(x = matrix()) # Initializing object x as a function argument. 
{
        # Initializing object inverse_matrix that is set to NULL.
        inverse_matrix <- NULL
        # "Set" function. 
        set <- function(y) {
                # Assigning the input argument to the x object in the parent environment. 
                x <<- y
                # Assigning the value of NULL to the inverse_matrix object in the parent environment.
                inverse.matrix <<- NULL
        }
        # "Get" function.
        get <- function() x
        # Defining the setter for the inverse_matrix.
        set_inverse_matrix <- function(solve) inverse_matrix <<- solve
        # Defyning the getter for the inverse_matrix. 
        get_inverse_matrix <- function() inverse_matrix
                # Assigning all the functions as elements within a list(), returning them to parent environment. 
                list (set = set, get = get, 
                      set_inverse_matrix = set_inverse_matrix,
                      get_inverse_matrix = get_inverse_matrix)
} 


# This function will retrieve the cached inverse matrix of x (from makeCacheMatrix). 
cacheSolve <- function(x, ...) # Object x is used as a function argument. The elypsis allows to use more arguments if needed.
{
        # Retriving an inverse_matrix for the object that is defined as the function's argument.
        inverse_matrix <- x$get_inverse_matrix()
        # Checking if the result is NULL. 
        if(!is.null(inverse_matrix)) {
                # In the result is not equal to NULL, the function retrives it.
                message("getting cached data")
                return(inverse_matrix)
        }
        # If the result in NULL, the function calculates inverse matrix and prints the result.
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$set_inverse_matrix(inverse_matrix)
        inverse_matrix
}
