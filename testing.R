
df <- function(n1,n2,s1,s2){
  nu = ((s1^2/n1)+(s2^2/n2))^2
  de = (s1^4)/(n1^2*(n1-1)) + (s2^4)/(n2^2*(n2-1))
  return(round(nu/de,0))
}
df(50,50,4,3)

conf_int <- function(n1, n2, s1, s2,x1_bar, x2_bar, alpha){
cric <- qt(p = alpha/2, df = df(n1,n2,s1,s2), lower.tail = FALSE)
se <- ((s1^2/n1)+(s2^2/n2))^0.5
value <- x1_bar - x2_bar
lb <- value - (se*cric)
ub <- value + (se*cric)
cat('Lower limit is', lb, '\n')
cat('Upper limit is', ub, '\n')
}

# 965 confidence implies alpha = (100-96)% = 0.04

conf_int(50,50,4,3, 30,39,0.04)

-9-1.4700 
-9 +0.01192

