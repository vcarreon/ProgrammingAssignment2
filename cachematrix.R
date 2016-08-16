##Create a special matrix that can cache its inverse
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    #Create variable for inverse and set to NULL- it starts out uncomputed
    inv<-NULL
    #set value of the matrix
    set<-function(y) {
        x<<- y
        inv<<- NULL
    }
    #get value of the matrix
    get<-function()x
    #set the value of the inverse but do not calculate it
    set_inverse<-function(inverse) inv<<-inverse
    #get the value of the inverse
    get_inverse<-function() inv
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    #first look to see if the inverse has already been calculated
    inv<-x$get_inverse()
    #if inverse already calculated, returns the inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #If not, calculate the inverse
    data<-x$get()
    inv<-solve(data, ...)
    x$set_inverse(inv)
    #Return the inverse
    inv
}

##Test the functions
my_matrix<-makeCacheMatrix(matrix(c(4,2,7,6),2,2))
my_matrix$get()
cacheSolve(my_matrix)
my_matrix$get_inverse()
cacheSolve(my_matrix)

##Compare to calculating the inverse using the solve function
first_matrix<-matrix(c(4,2,7,6),2,2)
first_matrix
inverted_matrix<-solve(first_matrix)
inverted_matrix
