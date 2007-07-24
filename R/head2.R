head2<-function(x, head = 3, tail=1, dotrows=2)
{
   x <- format(rbind(head(x,head + dotrows), tail(x,tail)))
   if(dotrows>0)
   {
      x[(head + 1):(head + dotrows),] <- "."
      for(i in 1:dotrows){rownames(x)[head+i]<-paste(".", substring("         ", 1, i-1))}
   }
   x
}

