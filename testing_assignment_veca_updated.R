

#library(combinat)
#nsample <- 9
n_t <- .6
nsample <- 9
num_treat <- ceil(nsample * n_t)
count_combs <- choose(nsample,num_treat)

fill_in_treat <- as.data.frame(matrix(0, nrow = nsample, ncol = count_combs))
row_counter <- 1
treat_counter <-0
while(treat_counter < num_treat) {
  fill_in_treat[row_counter,1] <- 1
  row_counter <- row_counter + 1
  treat_counter <- treat_counter + 1
}


base <- as.data.frame(fill_in_treat[,1])
last_base_treat <- 0
for(i in 1: nrow(base)) {
  if(base[i,1] == 1) {
    last_base_treat <- i
  }
}

base_but_one <- base
base_but_one[last_base_treat,1] <- 0
add_treat <- last_base_treat 

for(c in 1:ncol(fill_in_treat)) {
#for(c in 1:40){
  for(r in 1:nrow(fill_in_treat)) {
  
  fill_in_treat[r,c] <- base_but_one[r,1] + ifelse(r == add_treat,1,0)
  
  if(r == nrow(fill_in_treat)) {
    add_treat <- add_treat + 1
  }
  
  #Once Changed All Possible With Current Base, Move Base
  #update add treat
  if(add_treat > nrow(fill_in_treat) & r == nrow(fill_in_treat)) {
    treat_on_flag <- 1
    count_treat_at_bottom <- 0
    for(i in nrow(base_but_one):1) {
      if(base_but_one[i,1] == 1 & treat_on_flag == 1) {
        count_treat_at_bottom <- count_treat_at_bottom + 1
        base_but_one[i,1] <- 0
      }
      if(base_but_one[i,1] == 0 & treat_on_flag < 2) {
        treat_on_flag <- 0
      }
      if(base_but_one[i,1] ==1 & treat_on_flag == 0) {
        first_treat_not_at_bottom <- i
        treat_on_flag <- 2
      }
    }  
    
    base_but_one[first_treat_not_at_bottom,1] <- 0
    
    for(j in 1:(count_treat_at_bottom+1)) {
      base_but_one[first_treat_not_at_bottom + j, 1] <- 1
    }
    
    add_treat <- first_treat_not_at_bottom + count_treat_at_bottom + 2

    #update move base loop ends
  }

  flag_loop_update_base <- ifelse(base_but_one[nrow(fill_in_treat),1] == 1, 1, 0)
  while( flag_loop_update_base > 0 & c <  ncol(fill_in_treat) ) {
    treat_on_flag <- 1
    count_treat_at_bottom <- 0
    for(i in nrow(base_but_one):1) {
      if(base_but_one[i,1] == 0 & treat_on_flag < 2) {
        treat_on_flag <- 0
       }
      if(base_but_one[i,1] == 1 & treat_on_flag == 1) {
        count_treat_at_bottom <- count_treat_at_bottom + 1
        base_but_one[i,1] <- 0
      }
      if(base_but_one[i,1] ==1 & treat_on_flag == 0) {
        first_treat_not_at_bottom <- i
        treat_on_flag <- 2
      }
    }  
    
    base_but_one[first_treat_not_at_bottom,1] <- 0
    
    for(j in 1:(count_treat_at_bottom+1)) {
      base_but_one[first_treat_not_at_bottom + j, 1] <- 1
    }
    
    add_treat <- first_treat_not_at_bottom + count_treat_at_bottom + 2
    flag_loop_update_base <- ifelse(base_but_one[nrow(fill_in_treat),1] == 1, 1, 0)
    #update move base loop ends
  }
  

  #row loop end            
  }
  
#col loop end  
}