add2 <- function(x,y){
        x+y
}

##above 10
above10 <- function(x){
        use<- x>10
        x[use]
}
above <-function(x,n=10){
        use <-x >n
        x[use]
}
##Column Mean
columnmean <-function(y,removeNA=TRUE){
        nc<-ncol(y)
        means <- numeric(nc)
        for(i in 1:nc){
                means[i]<- mean(y[,i],na.rm=removeNA)
        }
        means
}

###Writing out a Data Frame example
write_letters<-function(x){ ##x is the number of letter you want to write
        ## letters<-LETTERS[1:x]
        ## fac <- sample(letters,x, replace = TRUE)
        z<-1
        while (z>=1 && z<=x){
                letters<-LETTERS[1:x]
                fac <- sample(letters,x, replace = TRUE)
                coin <-rbinom(1,1,.5)
                if (coin==1){
                        winner <-letters
                        z<-z+1
                        
                } else {
                        winner<-fac
                        z<-z+1
                        coin
                }
        }
        d<-data.frame(Col_1=letters,Col_2=winner,Letter=fac)
        d
}

cft<-function(x){
        
}

f<-function(x){
        g<-function(y){
                y+z
        }
        z<-4
        x+g(x)
}
###Constructor function
make.power <-function(n){
        pow<- function(x){
                x^n
        }
        pow
}


