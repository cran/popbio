splitA<-function(A, r=1, c=-1)
{
   tm<-A
   tm[r,c]<-0
   fm<-A
   fm[-r, ]<-0
   fm[ r, -(c) ]<-0
   list(T=tm,F=fm)
}

