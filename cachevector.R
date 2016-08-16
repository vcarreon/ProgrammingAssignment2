##Caching mean of a vector
##The first function,  makeVector  creates a special "vector", which 
##is really a list containing a function to
##1. set the value of the vector
##2. get the value of the vector
##3. set the value of the mean
##4. get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    #Set the value of the vector
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #Get the value of the vector
    get <- function() x
    #Set the value of the mean
    setmean <- function(mean) m <<- mean
    #Get the value of the mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

##Calculate mean of the special vector
cachemean <- function(x, ...) {
    #first look to see if the mean has already been calculated
        m <- x$getmean()
    #if mean already calculated, returns the mean
        if(!is.null(m)) {
        message("getting cached data")
    #if not, calculates the mean
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    #Set the value of the mean in the cache
    x$setmean(m)
    m
}

##Test it
a <- makeVector(c(1,2,3,4))
a$get()
a$getmean()
cachemean(a)
a$getmean()  # this is only to show you that the mean has been stored and does not affect anything
a$set(c(10,20,30,40))
a$getmean()
cachemean(a)
cachemean(a)
a$get()
cachemean(a)
a <- makeVector(c(5, 25, 125, 625))
a$get()
cachemean(a)
cachemean(a)

##4,2,7,6
##create a matrix
A= matrix(c(3,3.2,3.5,3.6),nrow=2,ncol=2)
A
##R formula for inverse of matrix
A1<-solve(A)
A1
##Test to see if get identity matrix- does not seem to work
I<-A%*%A1
I
