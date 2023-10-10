setwd("C:/Users/Ana Januário/ownCloud - ana.januario@owncloud.decsis.cloud/Ana/google_curso/R")

x<-c(1,3,5)
y<-c(3,2,10)
b<-cbind(x,y)
a<-rbind(x,y)
class(a)
is.vector(x)

x<-list(2,"a","b",TRUE)
x[[2]]
is.list(x[[1]])
is.numeric(x[[1]])
is.character(x[[1]])
length(x[[1]])


x<-1:4
y<-2
y<-2:3
a<-x+y
is.integer(a)
is.numeric(a)

x<-c(17,14,4,5,13,12,10)
x[x>=10]<-4
x
x[x>=10]<-4

x<-c(3,5,1,10,12,6)
x[x<6]<-0
x[x %in% 1:5]<-0


dados<-read.csv("hw1_data.csv", header = T, sep = ",")
names(dados)

head(dados,2)

head(dados,2)
tail(dados,2)

dados[47,]

sum(is.na(dados$Ozone))

mean(dados$Ozone, na.rm = T)

subset_dados <- subset(dados, Ozone > 31 & Temp > 90)
mean(subset_dados$Solar.R)

media_junho <- mean(dados$Temp[dados$Month == 6], na.rm = TRUE)

max(dados$Ozone[dados$Month==5], na.rm=T)

#-------------------------------------------------------------

#install.packages("swirl")
packageVersion("swirl")
library(swirl)
#instalar do swirl o R programming course
install_from_swirl("R Programming")

swirl()
hello<-paste("Hello", "world!", sep = " ")
hello

#loop
x<- c("a", "b", "c" , "d")

for ( i in 1:4) {
    print(x[i])
}


for ( i in seq_along(x)){
  print(x[i])
}

for ( letter in x){
  print(letter)
}

for ( i in 1:4) print(x[i])

#nested Loops

x<- matrix(1:6, 2,3)
x

for(i in seq_len(nrow(x))){
        for(j in seq_len(ncol(x))){
          print(x[i,j])
        }
}


#while
#o loop com while começa testando uma condição
#pode resultar em loop infinito!!!

count<-0
while(count<10){
  print(count)
  count<- count +1
}

#As vezes teremos que testar mais d euma condição

z<-5

while(z>=3 && z<=10){
  print(z)
  coin<- rbinom(1,1,0.5)
  
  if(coin==1){#random walk
        z<-z+1
  } else{z<-z-1}
}
