#####
#Created by RM on 2019.01.30
#Econ 666: PS 1,Q3
#####

library(MASS)
library(plyr)
library(data.table)
library(matlib)
library(randomizr)
library(tidyverse)
library(boot)
library(ggplot2)
library(gmm)
library(foreign)
library(xtable)
library(Hmisc)
library(sem)
library(AER)

##Set Seed

set.seed('666666')

##Export Path
export <- "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Output/PS1"


##FUNCTION for fisher matrix
fisher_assign_matrix <- function(units,numtreat) {
  count_combs <- choose(units,numtreat)
  
  fill_in_treat <- as.data.frame(matrix(0, nrow = units, ncol = count_combs))
  row_counter <- 1
  treat_counter <-0
  while(treat_counter < numtreat) {
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
  
#fct end  
  return(fill_in_treat)
}

#TEST FCT
#fisher_mat <- fisher_assign_matrix(9,5)


##Code up key parameters

obs <- 100

subgroup1 <- .4
subgroup2 <- .7

reps <- 100
#reps <- 20

corr_x_y0 <- c(0, .1, .6, .1)
corr_x_tau <- c(0, .1, .1, .6)

tau <- .5

treatment_perc <- c(.3, .5)
#treatment_perc <- c(.3)

sample_size <- c(10,25,100)
#sample_size <- c(10,25)

for_fisher_loop <- 0

fisher_reps <- 10000
#fisher_reps <- 500

fisher_results_counter <- 0

#Make Shell For Storing Output

corr_x_y0_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
corr_x_tau_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
samplesize_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
perc_treated_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
ols_beta_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
ols_se_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
olsx_beta_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
olsx_se_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
olsstrata_beta_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
olsstrata_se_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
olsinteract_beta_out <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
olsinteract_se_out  <- rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
iter <-  rep(-100,reps*length(sample_size)*length(treatment_perc)*length(corr_x_y0))
results_mat <- data.frame(corr_x_y0_out,corr_x_tau_out,samplesize_out,perc_treated_out,ols_beta_out,ols_se_out,
                          olsx_beta_out, olsx_se_out, olsstrata_beta_out, olsstrata_se_out, olsinteract_beta_out, olsinteract_se_out,
                          iter)


corr_x_y0_out <- rep(-100,length(sample_size)*length(treatment_perc)*length(corr_x_y0))
corr_x_tau_out <- rep(-100,length(sample_size)*length(treatment_perc)*length(corr_x_y0))
samplesize_out <- rep(-100,length(sample_size)*length(treatment_perc)*length(corr_x_y0))
perc_treated_out <- rep(-100,length(sample_size)*length(treatment_perc)*length(corr_x_y0))
rand_te_out_fish <- rep(-100,length(sample_size)*length(treatment_perc)*length(corr_x_y0))
rand_se_out_fish <- rep(-100,length(sample_size)*length(treatment_perc)*length(corr_x_y0))
results_mat_fish <- data.frame(corr_x_y0_out,corr_x_tau_out,samplesize_out,perc_treated_out,
                               rand_te_out_fish, rand_se_out_fish )

results_counter <- 0
#Generate Data: INITIAL LOOP BY CORRELATION
for(c in 1:length(corr_x_y0)) {
  y0 <- rnorm(obs,0,1)
    
  corr_x_y0_iter <- corr_x_y0[c]
  x <- sqrt(corr_x_y0_iter^2/(1-corr_x_y0_iter^2)) * y0 + rnorm(obs,0,1)
    
  corr_x_tau_iter <- corr_x_tau[c]
  tau <- sqrt(corr_x_tau_iter^2/(1-corr_x_tau_iter^2)) * x + rnorm(obs,0,1)
  
  data <- data.frame(y0,tau,x)
  data <- data[order(x),]
  data$counter <- seq(1,obs,1)
  
  ##ADD LOOP FOR SAMPLE SIZE:::
  for(n in 1:length(sample_size)) {
    
    #Ramdomly Sample by Ordering and Sorting
    sample_order <- runif(obs,0,1)
    obs_counter <- seq(1,obs,1)
    sample_order_data <- data.frame(obs_counter,sample_order)
    sample_order_sorted <- sample_order_data[order(sample_order), ]
    
    nsample <- sample_size[n]
    
    ids_to_subset <- as.data.frame(sample_order_sorted[1:nsample,1])
    names(ids_to_subset) <- "counter"
    subset_data <- merge(data, ids_to_subset, by.x = "counter", by.y = "counter")
    subset_data$group <- ifelse(subset_data$counter < obs * subgroup1, 1, ifelse(subset_data$counter < obs * subgroup2, 2, 3)   )
    subset_data$ones <- 1

      ##ADD LOOP FOR PERC TREATED    
      for(nt_count in 1:length(treatment_perc)) {
        n_t <- treatment_perc[nt_count]
        
        #Figure out How Many to Treat
        count_by_group <- ddply(subset_data, .(group), summarise, count = sum(ones))
        count_by_group <- count_by_group[order(count_by_group$group),]
        count_by_group$first_treat <- 1
        for(i in 2:nrow(count_by_group)) {
          count_by_group[i,3] <- count_by_group[i-1,2] + count_by_group[i-1,3] 
        }
        count_by_group$to_treat <- ceil(count_by_group$count * n_t)
        count_by_group$last_treat <- -1 + count_by_group$first_treat + count_by_group$to_treat
        
        ##ADD LOOP FOR REPS:::
        for(r in 1:reps) {
        #Assign to Treatment And Control
          treatment_rand <- subset_data$group + runif(nrow(subset_data),0,1)
          subset_data_sorted <- subset_data[order(treatment_rand), ]
          data_pot_outcome <- merge(subset_data_sorted,count_by_group, by.x = "group", by.y = "group")
          data_pot_outcome$row_counter <- seq(1,nrow(data_pot_outcome),1)
          data_pot_outcome$treat <- ifelse(data_pot_outcome$row_counter >= data_pot_outcome$first_treat, 
                                            ifelse(data_pot_outcome$row_counter <= data_pot_outcome$last_treat,1,0),0)
          data_pot_outcome$y_obs <- ifelse(data_pot_outcome$treat > 0, data_pot_outcome$y0 + data_pot_outcome$tau
                                            , data_pot_outcome$y0  )
          data_pot_outcome$y1 <- data_pot_outcome$y0 + data_pot_outcome$tau
          #Measure Treatment Effect
          olsmodel <- lm(y_obs ~  treat ,data=data_pot_outcome)        
          olsresults <- coef(summary(olsmodel))
          
          olsxmodel <- lm(y_obs ~  treat + x ,data=data_pot_outcome)        
          olsxresults <- coef(summary(olsxmodel))
          
          olsstratamodel <- lm(y_obs ~ treat + I(group < 2) + I(group > 2), data = data_pot_outcome )
          olsstrataresults <- coef(summary(olsstratamodel))
          
          olsinteractmodel <- lm(y_obs ~ treat + I(group < 2) + I(group > 2) + treat:I(group < 2) + treat:I(group > 2), data = data_pot_outcome )
          olsinteractresults <- as.data.frame(coef(summary(olsinteractmodel)))
          olsinteractresults$var <- row.names(olsinteractresults)
          olsinteractresults_treat <- olsinteractresults[regexpr("tre",olsinteractresults$var) > 0,]
          olsinteractresults_treat$group <- ifelse(regexpr("group < 2",olsinteractresults_treat$var) > 0, 1, 
                                                ifelse(regexpr("group > 2",olsinteractresults_treat$var) > 0, 3, 0) )
          counts_by_group <- ddply(data_pot_outcome, .(group), summarise, groupcount = sum(ceil(group/max(data_pot_outcome$group))))
          olsinteractresults_treat_gr <- merge(olsinteractresults_treat, counts_by_group, all.x = TRUE)
          olsinteractresults_treat_gr$groupcount <- ifelse(is.na(olsinteractresults_treat_gr$groupcount) == 1, nrow(data_pot_outcome), olsinteractresults_treat_gr$groupcount  )
          olsinteractresults_treat_gr$forate <- olsinteractresults_treat_gr$Estimate *  olsinteractresults_treat_gr$groupcount / nrow(data_pot_outcome)
          olsinteractresults_treat_gr$forvar <- olsinteractresults_treat_gr$`Std. Error`^2 *  (olsinteractresults_treat_gr$groupcount / nrow(data_pot_outcome))^2
          
          olsinteract_ate <- sum(olsinteractresults_treat_gr$forate)
          olsinteract_var <-  sum(olsinteractresults_treat_gr$forvar)
      
          
          #Fill Results Matrix
          results_counter <- results_counter + 1
          results_mat[results_counter,1] <- corr_x_y0_iter
          results_mat[results_counter,2] <- corr_x_tau_iter
          results_mat[results_counter,3] <- nsample
          results_mat[results_counter,4] <- n_t
          results_mat[results_counter,5] <- olsresults[2,1]
          results_mat[results_counter,6] <- olsresults[2,2]
          results_mat[results_counter,7] <- olsxresults[2,1]
          results_mat[results_counter,8] <- olsxresults[2,2]     
          results_mat[results_counter,9] <- olsstrataresults[2,1]
          results_mat[results_counter,10] <- olsstrataresults[2,2]        
          results_mat[results_counter,11] <- olsinteract_ate
          results_mat[results_counter,12] <- sqrt(olsinteract_var)
          results_mat[results_counter,13] <- r
          
        }
      
        #for_fisher_loop <- for_fisher_loop + 1
      ##FISHER VERSION: ONCE FOR EACH N, N_t AND CORR (so 6 * 4)
        num_treat <- ceil(nsample * n_t)
        count_combs <- choose(nsample,num_treat)
        fisher_test_data <- data.frame(data_pot_outcome$y0, data_pot_outcome$y1)
        
        if (count_combs <= 10000) {
          fisher_mat <- fisher_assign_matrix(nsample,num_treat)
          fisher_results_rand <- rep(-100,ncol(fisher_mat))

          for(i in 1:ncol(fisher_mat)) {
            fisher_pot_outcomes <- fisher_mat[,i] * fisher_test_data$data_pot_outcome.y1 + (1-fisher_mat[,i]) * fisher_test_data$data_pot_outcome.y0
            fisher_test_data_iter <- data.frame(fisher_pot_outcomes)
            fisher_test_data_iter$fisher_treat <- fisher_mat[,i]
            pre_fisher_treatment_effect <- ddply(fisher_test_data_iter, .(fisher_treat), summarise, y_bar_by_group = mean(fisher_pot_outcomes))
            pre_fisher_treatment_effect <- pre_fisher_treatment_effect[order(pre_fisher_treatment_effect$fisher_treat),]
            fisher_effect <- pre_fisher_treatment_effect[2,2] - pre_fisher_treatment_effect[1,2]
            fisher_results_rand[i] <- fisher_effect
          }
  
        }
        
        #Otherwise do random assignment
        if (count_combs > 10000) {
          fisher_results_rand <- rep(-100,fisher_reps)
          
          for(i in 1:fisher_reps) {
            randtreat <- runif(nrow(fisher_test_data),0,1) 
            fisher_test_data_iter <- fisher_test_data[order(randtreat),]
            seq <- seq(1,nrow(fisher_test_data_iter),1)
            fisher_test_data_iter$fisher_treat <- ifelse(seq <= num_treat, 1, 0)
            fisher_test_data_iter$fisher_outcome <- fisher_test_data_iter$fisher_treat * fisher_test_data_iter$data_pot_outcome.y1
            fisher_test_data_iter$fisher_outcome <- fisher_test_data_iter$fisher_outcome + (1 - fisher_test_data_iter$fisher_treat) * fisher_test_data_iter$data_pot_outcome.y0
            pre_fisher_treatment_effect <- ddply(fisher_test_data_iter, .(fisher_treat), summarise, y_bar_by_group = mean(fisher_outcome))
            pre_fisher_treatment_effect <- pre_fisher_treatment_effect[order(pre_fisher_treatment_effect$fisher_treat),]
            fisher_effect <- pre_fisher_treatment_effect[2,2] - pre_fisher_treatment_effect[1,2]
            fisher_results_rand[i] <- fisher_effect
          }

        }
        
        fisher_results_counter <- fisher_results_counter + 1
        results_mat_fish[fisher_results_counter, 1] <- corr_x_y0_iter
        results_mat_fish[fisher_results_counter, 2] <- corr_x_tau_iter
        results_mat_fish[fisher_results_counter, 3] <- nsample
        results_mat_fish[fisher_results_counter, 4] <- n_t
        results_mat_fish[fisher_results_counter, 5] <- mean(fisher_results_rand)
        results_mat_fish[fisher_results_counter, 6] <- sd(fisher_results_rand)
        
      }
        
  }
  
}


#Prepare the not fisher results for output
not_fisher_summary <- ddply(results_mat, .(corr_x_y0_out, corr_x_tau_out, samplesize_out, perc_treated_out), summarise,
                            avg_ols_beta = mean(ols_beta_out), avg_ols_se = mean(ols_se_out),
                            avg_olsx_beta = mean(olsx_beta_out), avg_olsx_se = mean(olsx_se_out),
                            avg_olsstrata_beta = mean(olsstrata_beta_out), avg_olsstrata_se = mean(olsstrata_se_out),
                            avg_olsinteract_beta = mean(olsinteract_beta_out), avg_olsinteract_se = mean(olsinteract_se_out)
                            )

all_summary <- merge(not_fisher_summary, results_mat_fish)

for(c in 5:ncol(all_summary)) {
  all_summary[,c] <- round(all_summary[,c], digits = 3)
}


outpath <- paste(export,"/R_PS1_Q3.csv", sep = "")
write.csv(all_summary,outpath)

