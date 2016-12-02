dataset_url<-"http://s3.amazonaws.com/practice_assignment/diet_data.zip"
download.file(dataset_url,"diet_data.zip")
unzip("diet_data.zip", exdir="diet_data")
getwd()
andy<-read.csv("diet_data/Andy.csv")
head(andy)
length(andy$Day)
dim(andy)
str(andy)
summary(andy)
names(andy)##gives the column names
##subsetting
andy[1,"Weight"]
andy[30,"Weight"]
andy[which(andy$Day==30),"Weight"]
andy[which(andy[,"Day"]==30),"Weight"]
subset(andy$Weight, andy$Day==30)

#####Let’s assign Andy’s starting and ending weight to vectors:
andy_start <- andy[1, "Weight"]
andy_end <- andy[30, "Weight"]
andy_start - andy_end
##Let’s take the output of list.files() and store it:
files<- list.files("diet_data")
files
files[1]
files[2]
files[3:5]
head(read.csv(files[3]))
files_full <- list.files("diet_data",full.names =TRUE) ###probably want to use this
files_full
head(read.csv(files_full[3]))


###Binding two Dataframes
andy_david <-rbind(andy, read.csv(files_full[2]))
head(andy_david)
tail(andy_david)
day_25<- andy_david[which(andy_david$Day==25),]
day_25

####binding loop
dat<-data.frame()
for (i in 1:5){
        dat<- rbind(dat,read.csv(files_full[i]))
}
str(dat)

####Finding the median
median(dat$Weight, na.rm=TRUE)

dat_30 <- dat[which(dat[, "Day"]==30),]
dat_30

median(dat_30$Weight)

weightmedian <- function(directory, day) {
        files_list <- list.files(directory, full.names = TRUE) #creates a list of files
        dat <- data.frame() #creates an empty data frame
        for (i in 1:5) {
                # loops through the files, rbinding them together
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        dat_subset <- dat[which(dat[, "Day"] == day), ] #subsets the rows that match the 'day' argument
        median(dat_subset[, "Weight"], na.rm = TRUE) #identifies the median weight
        # while stripping out the NAs
}
dataset_url<-"https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url,"specdata.zip")
unzip("specdata.zip", exdir="specdata")
list.files("specdata/specdata")

files_list <- list.files("specdata/specdata", full.name = TRUE)
pol_dat<-data.frame()
for (i in 1:10){
        pol_dat<- rbind(pol_dat,read.csv(files_list[i]))
}
str(pol_dat)
summary(pol_dat)



###Week 2 Assignment 
###Part 1
pollutantmean <- function(directory,pollutant,id){
        files_list <- list.files(directory, full.name = TRUE)
        dat_p1 <- data.frame()
        for(i in id){
                dat_p1 <-rbind(dat_p1,read.csv(files_list[i])) ###binds all the files in the directory into one dataframe named dat_p1    
        }
        mean(dat_p1[,pollutant], na.rm=TRUE) ##takes the mean of the polluntant column specified in the function, removes the NA values
}

pollutantmean <- function(directory,pollutant,id){
        files_list <- list.files(directory, full.name = TRUE)[id]
        tmp<-lapply(files_list, read.csv)
        output<- do.call (rbind, tmp)
        mean(output[,pollutant], na.rm=TRUE) ##takes the mean of the polluntant column specified in the function, removes the NA values
}

complete <- function(directory,id){
        fnames<- list.files(directory, full.name = TRUE)[id] ##returns a character vector with the specified amount of id files
        count_complete<-function(fname) sum(complete.cases(read.csv(fname)))##creates a function called count_complete
        data.frame(id=id, nobs = unlist(lapply(fnames, count_complete)))##creates a dataframe with id, and nobs using lapply instead of using a loop
}

####Part 3
corr <- function(directory, threshold=0){
        id= 1:332
        files_list <- list.files(directory, full.names=TRUE) ## creates a character vector with the names of the files in the directory
        nobs <- c() ##creates an empty vector
        cns <- c() ## creates an empty vector
        x<-c() ## creates and empty vector
        for (i in id) {
                dat <- read.csv(files_list[i]) #creates data frame "dat" with data from all 332 files in it
                nobs <- c(nobs, sum(complete.cases(dat))) #creates column "nobs" populated with number of complete cases for all variables, per id number
                cns <- c(cns, cor(dat$sulfate, dat$nitrate, use= "pairwise.complete.obs")) #creates column "cns" populated with Pearson correlation between nitrate and sulfate, with all NAs removed
        }
        output<- data.frame(cns, nobs, id) #creates dataframe named output "data.frame" populated with variables cns, nobs, and id
        head(output) ## prints the head of the output dataframe 
        x<-ifelse(threshold < output$nobs,output$cns,0)##if threshold < nobs, returns the cns value to x, else the cns value is 0  
        x[x!=0]##subset of nonzero values in the vector x 
}
