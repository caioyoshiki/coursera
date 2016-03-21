
# These two functions was adapted from the examples given by the Coursera to calculate and cache a mean of a vector.
#This function creates a matrix and a list that will be used to save the matrix within its inverse.


makeCacheMatrix <- function(m) { 
    assist <- NULL
    set <- function(y) {
        m <<- y
        assist <<- NULL
    }
    get <- function() m
    set_inverse <- function(inverse) assist <<- inverse
    get_inverse <- function() assist
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

# This function return the inverse of the matrix m.
cacheSolve <- function(m, ...) {
    assist <- m$get_inverse() # Firstly, it is checked if the inverse has been calculated before.
    if(!is.null(assist)) { # if so, get it from the cache data.
        message("getting cached data")
        return(assist)
    }
    hand <- m$get() # if it has not, then it calculates the inverse immediately.
    assist <- solve(hand)
    m$set_inverse(assist) #put the inverse in the list created by the first function; 
    assist #return the inverse.
}

