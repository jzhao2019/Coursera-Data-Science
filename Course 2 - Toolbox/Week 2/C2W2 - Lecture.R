# Free Variable
make.power<-function(n){
        pow<-function(x){
                x^n
        } 
        pow
}
cube<-make.power(3)
sqare<-make.power(2)

# understand Lexical Scoping (first search in the environment the free variable is evaluated or defined not the environment 
# where the function was called, then the parent environment)
y=10

f<-function(x) {
        y=6
        g(x)
        
        
}
g<-function(x){
                z<-x+y
                y<-5
                z
}

# search up in the environment the free variable is evaluated.
f <- function(x) {
        g <- function(y) {
                y + z
        }
        
        h<-x + g(x)
        z <- 4
        # h<-x + g(x)
        h
}