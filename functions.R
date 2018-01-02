
strdf <- function(df, n=4, width=60, 
                     n.levels=n, width.levels=width, 
                     factor.values=as.character) {
     stopifnot(is.data.frame(df))
     tab <- data.frame(variable=names(df),
                       class=rep(as.character(NA), ncol(df)),
                       levels=rep(as.character(NA), ncol(df)),
                       examples=rep(as.character(NA), ncol(df)),
                       stringsAsFactors=FALSE)
     collapse.values <- function(col, n, width) {
          result <- NA
          for(j in 1:min(n, length(col))) {
               el <- ifelse(is.numeric(col),
                            paste0(col[1:j], collapse=', '),
                            paste0('"', col[1:j], '"', collapse=', '))
               if(nchar(el) <= width) {
                    result <- el
               } else {
                    break
               }
          }
          if(length(col) > n) {
               return(paste0(result, ', ...'))
          } else {
               return(result)
          }
     }
     
     for(i in seq_along(df)) {
          if(is.factor(df[,i])) {
               tab[i,]$class <- paste0('Factor w/ ', nlevels(df[,i]), ' levels')
               tab[i,]$levels <- collapse.values(levels(df[,i]), n=n.levels, width=width.levels)
               tab[i,]$examples <- collapse.values(factor.values(df[,i]), n=n, width=width)
          } else {
               tab[i,]$class <- class(df[,i])[1]
               tab[i,]$examples <- collapse.values(df[,i], n=n, width=width)
          }
          
     }
     
     class(tab) <- c('strtable', 'data.frame')
     return(tab)
}