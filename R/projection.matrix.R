projection.matrix <-function(transitions, stage=NULL, fate=NULL, fertility=NULL, sort=NULL,  add=NULL, TF=FALSE )
{
   if(missing(stage)){ stage <-"stage" }
   if(missing(fate)){  fate  <-"fate" }


   
   ## copied from subset.data.frame -- specify stage, fate, fertility columns by number or name (with or without quotes)
     nl <- as.list(1:ncol(transitions))
     names(nl) <- names(transitions)
     stage <- eval(substitute(stage), nl, parent.frame())
     fate <- eval(substitute(fate), nl, parent.frame())

   ## default - sort matrix by levels in stage column 
   if(missing(sort)) { sort<-levels( transitions[,stage]  )  }

   ## default -- fertility columns equal stage class name
   if(missing(fertility)){ fertility<- intersect(sort, names(transitions)) }

   fertility <- eval(substitute(fertility), nl, parent.frame())




   ### TRANSITION matrix
  
   tf<-table( transitions[,fate], transitions[,stage])   ## Create transition frequency table.

   ## check if "sort"  matches levels in fate,stage columns , e.g, sort names are misspelled or wrong columns selected by accident
   T_matrix <- try( prop.table(tf,2)[sort,sort] , silent=TRUE)  ## Create transition matrix using prop.table

   if(class(T_matrix)=="try-error")
   {
      warning( paste("Error sorting matrix.
  Make sure that levels in stage and fate columns
  match stages listed in sort.\n Printing unsorted matrix instead!\n"), call. = FALSE)
      # set sort to TRUE for fertility matrix in next section
      sort<-TRUE
      T_matrix <- prop.table(tf,2)
   }
   T_matrix[is.nan(T_matrix)] <- 0             ## Replace NaN values 

   ## Hack to add estimated transitions (change to list?)
   if(length(add)>0)
   { 
      for ( i in seq(1,length(add), 3))
      {
         T_matrix[add[i+0], add[i+1]]<- as.numeric(add[i+2])
      }
   }
 

   ## Fertility  matrix
   n<-length(fertility)
   F_matrix <- T_matrix * 0  ## Create matrix of zeros. 
   if( n==0  )
      { warning ("Missing a fertility column with individual fertility rates\n", call. = FALSE )}
   else
   {
       
     for (i in 1:n)                                  
      {                                              
                                           
         fert <- tapply(transitions[, fertility[i] ],  ## Summarize "stage i" fertility column 
                        transitions[, stage], mean)[sort]    ## by classes to calculate mean fertility rates.
         F_matrix[i, ] <- fert                     ## Add fertilities to i row of matrix - skipping rows not allowed.
      }
   }
   F_matrix[is.na(F_matrix)] <-0                   ## Remove NA values if offspring class is missing in "stage" column

  if(TF){ list( T=T_matrix, F=F_matrix )}
   else{  T_matrix + F_matrix}

 }

