y <- c(106,118,124,94,82,88,87,88,88,68,98,115,135)
k_val <- function(r){
  sum(choose(r,0:r)^2)
}

#difference
d1<-diff(y)
d1
d2<-diff(d1)
d2
d3<-diff(d2)
d3
d4<-diff(d3)
d4

#mean
m1<-mean(d1^2)
m1
m2<-mean(d2^2)
m2
m3<-mean(d3^2)
m3
m4<-mean(d4^2)
m4

#constant
c1<-k_val(1)
c2<-k_val(2)
c3<-k_val(3)
c4<-k_val(4)

#variance
v1<-m1/c1
v1
v2<-m2/c2
v2
v3<-m3/c3
v3
v4<-m4/c4
v4

