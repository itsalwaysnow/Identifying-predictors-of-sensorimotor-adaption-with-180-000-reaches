#### HAND ANGLE PROCESSING  ####

outlier_rm <- function(my.data, hand_angle_cutoff, movingwindow, howmanysd, withreturn) {
  
  myoutliers <- setNames ( data.frame(matrix(NaN, nrow = length(unique( my.data$SN)) * 12, ncol = 6)), 
                           c('SN', 'ti','num_hand_rm', 'num_RT_rm', 'num_MT_rm', 'totaltrials') )
  
  for(si in 1:length(unique( my.data$SN))) {
    
    idx_sub <- my.data$SN == unique( my.data$SN)[si] & ( ! is.na(my.data$Hand) )
    
    for(ti in unique(my.data$ti[idx_sub])){
      
      idx <- my.data$SN == unique( my.data$SN)[si] & ( ! is.na(my.data$Hand) ) & my.data$ti == ti
      
      idx[is.na(idx)] <- FALSE
      
      if( sum(idx) > 10 ){
        idx_rm <- abs( my.data$Hand[idx] - movavg(my.data$Hand[idx], movingwindow, type = 's') ) > howmanysd * sd(my.data$Hand[idx] - movavg(my.data$Hand[idx], movingwindow, type = 's'), na.rm = TRUE) | abs(my.data$Hand[idx]) > hand_angle_cutoff
        idx_rt_rm <- abs( my.data$RT[idx] - movavg(my.data$RT[idx],  movingwindow, type = 's') ) > howmanysd * sd(my.data$RT[idx] - movavg(my.data$RT[idx], movingwindow, type = 's'), na.rm = TRUE) | abs(my.data$RT[idx]) > 2000
        idx_mt_rm <- abs( my.data$MT[idx] - movavg(my.data$MT[idx], movingwindow, type = 's') ) >  howmanysd * sd(my.data$MT[idx] - movavg(my.data$MT[idx], movingwindow, type = 's'), na.rm = TRUE) | abs(my.data$MT[idx]) > 2000
        idx_st_rm <- abs( my.data$ST[idx] - movavg(my.data$ST[idx], movingwindow, type = 's') ) >  howmanysd * sd(my.data$ST[idx] - movavg(my.data$ST[idx], movingwindow, type = 's'), na.rm = TRUE) | abs(my.data$ST[idx]) > 8000
        
        idx_rm[is.na(idx_rm)] <- FALSE
        idx_rt_rm[is.na(idx_rt_rm)] <- FALSE
        idx_mt_rm[is.na(idx_mt_rm)] <- FALSE
        idx_st_rm[is.na(idx_st_rm)] <- FALSE
        
        my.data$Hand[idx][idx_rm] <- NaN
        my.data$RT[idx][idx_rt_rm] <- NaN
        my.data$MT[idx][idx_mt_rm] <- NaN
        my.data$ST[idx][idx_st_rm] <- NaN
        
        idx_new <- my.data$SN == unique( my.data$SN)[si] & ( ! is.na(my.data$Hand) ) & my.data$ti == ti
        idx_new[is.na(idx_new)] <- FALSE
        idx_RT_new <- my.data$SN == unique( my.data$SN)[si] & ( ! is.na(my.data$RT) ) & my.data$ti == ti
        idx_RT_new[is.na(idx_RT_new)] <- FALSE
        idx_MT_new <- my.data$SN == unique( my.data$SN)[si] & ( ! is.na(my.data$MT) ) & my.data$ti == ti
        idx_MT_new[is.na(idx_MT_new)] <- FALSE
        idx_ST_new <- my.data$SN == unique( my.data$SN)[si] & ( ! is.na(my.data$ST) ) & my.data$ti == ti
        idx_ST_new[is.na(idx_ST_new)] <- FALSE
        
        if(length( my.data$Hand[idx_new]) > movingwindow){
          my.data$Hand_dt[idx_new] <- my.data$Hand[idx_new] - movavg(my.data$Hand[idx_new], movingwindow, type = 's')
        }
        
        if(length(my.data$RT[idx_RT_new]) > movingwindow){
          my.data$RT_dt[idx_RT_new] <- my.data$RT[idx_RT_new] - movavg(my.data$RT[idx_RT_new], movingwindow, type = 's')
        }
        
        if(length(my.data$MT[idx_MT_new]) > movingwindow){
          my.data$MT_dt[idx_MT_new] <- my.data$MT[idx_MT_new] - movavg(my.data$MT[idx_MT_new], movingwindow, type = 's')
        }
        
        if(length(my.data$ST[idx_ST_new]) > movingwindow){
          my.data$ST_dt[idx_ST_new] <- my.data$ST[idx_ST_new] - movavg(my.data$ST[idx_ST_new], movingwindow, type = 's')
        }
        
        myoutliers$SN[si] <- si
        myoutliers$ti[si] <- ti
        myoutliers$totaltrials[si] <- length(my.data$TN[idx])
        myoutliers$num_hand_rm[si] <- sum(idx_rm, na.rm = TRUE)
        myoutliers$num_RT_rm[si] <- sum(idx_rt_rm, na.rm = TRUE)
        myoutliers$num_MT_rm[si] <- sum(idx_mt_rm, na.rm = TRUE)
        myoutliers$num_ST_rm[si] <- sum(idx_st_rm, na.rm = TRUE)
        
        if(withreturn == 1){
          idx_return_rm <- abs( my.data$ReturnAngle[idx] - movavg(my.data$ReturnAngle[idx], movingwindow, type = 's') ) > howmanysd * sd(my.data$ReturnAngle[idx] - movavg(my.data$ReturnAngle[idx], movingwindow, type = 's'), na.rm = TRUE) | abs(my.data$ReturnAngle[idx]) > hand_angle_cutoff
          idx_return_rm[is.na(idx_return_rm)] <- FALSE
          my.data$ReturnAngle[idx][idx_return_rm] <- NaN
          idx_return_new <- my.data$SN == unique( my.data$SN)[si] & ( ! is.na(my.data$ReturnAngle) ) & my.data$ti == ti
          idx_return_new[is.na(idx_return_new)] <- FALSE
          my.data$Return_dt[idx_return_new] <- my.data$ReturnAngle[idx_return_new] - movavg(my.data$ReturnAngle[idx_return_new], movingwindow, type = 's')
          myoutliers$num_Return_rm[si] <- sum(idx_return_rm, na.rm = TRUE)
        }
      }
    }     
  }
  return(list(my.data, myoutliers)) 
}

add_CN <- function(my.data){
  for(si in unique(my.data$Subject.ID)){
    idx <- my.data$Subject.ID == si
    my.data$numtar[idx] <- length(unique(my.data$ti[idx]))
  }
  my.data$CN <- ceiling(my.data$TN / my.data$numtar )
  return(my.data)
}

under180hand <- function(my.data){
  my.data$Hand <- my.data$Hand - my.data$ti
  idx_pos <- my.data$Hand >= 180
  idx_neg <- my.data$Hand <= -180
  my.data$Hand [ idx_pos ] <- my.data$Hand [ idx_pos ] - 360 # relative to 0
  my.data$Hand [ idx_neg] <- my.data$Hand [ idx_neg ] + 360 # relative to 0
  return(my.data)
}

assign_SN <- function(my.data){
  for(si in 1:length(unique(my.data$Subject.ID))){
    idx <- my.data$Subject.ID == unique(my.data$Subject.ID)[si]
    my.data$SN[idx] <- si
  }
  return(my.data)
}

baseline_subtract <- function(my.data, base_cn_start, base_cn_end, returnAngle){
  
  
  my.subjects <- unique(my.data$SN)
  counter <- 1
  mybaseline <- setNames ( data.frame(matrix(NaN, nrow = length(my.subjects) * 12, ncol = 5)), 
                           c('SN', 'ti', 'Hand_base', 'RT_base','Return_base'))
                                                    
  for(si in my.subjects){
    
    idx <- my.data$SN == si
      
    for(mi in unique(my.data$ti[idx])){
      
      idx_sub <- my.data$SN == si & my.data$ti == mi
      idx_bl <- my.data$SN == si & my.data$ti == mi & 
        my.data$CN >= base_cn_start & my.data$CN <= base_cn_end
      
      sub_Hand_mean <- mean(my.data$Hand[idx_bl], na.rm = TRUE)
      my.data$Handb[idx_sub] <- my.data$Hand[idx_sub] - sub_Hand_mean
      
      sub_RT_mean <- mean(my.data$RT[idx_bl], na.rm = TRUE)
      my.data$RTb[idx_sub] <- my.data$RT[idx_sub] - sub_RT_mean
      
      if(returnAngle == 1){
        sub_Return_mean <- mean(my.data$ReturnAngle[idx_bl], na.rm = TRUE)
        my.data$ReturnAngleb[idx_sub] <- my.data$ReturnAngle[idx_sub] - sub_Return_mean
        mybaseline$Return_base[counter] <- sub_Return_mean
      }
      
      mybaseline$SN[counter] <- si
      mybaseline$ti[counter] <- mi
      mybaseline$Hand_base[counter] <- sub_Hand_mean
      mybaseline$RT_base[counter] <- sub_RT_mean
      counter <- counter + 1
    }
  }
  
  return(list(my.data, mybaseline))
}

complete_exp <- function(my.data, numberoftrials){
  
  num.sub <- length(unique(my.data$Subject.ID))
  complete_data <- setNames ( data.frame(matrix(NaN, ncol = 3, nrow = num.sub)), c('Subject.ID', 'num_trials', 'complete') )
  
  for(si in 1:num.sub){
    idx <- my.data$Subject.ID == unique(my.data$Subject.ID)[si]
    complete_data$Subject.ID[si] <- as.character( unique(my.data$Subject.ID)[si] )
    complete_data$num_trials[si] <- nrow(my.data[idx, ])
    
    if(nrow(my.data[idx, ]) == numberoftrials){
      complete_data$complete[si] <- 1
    }else{
      complete_data$complete[si] <- 0
    }
  }
  return(complete_data)
}

computeblockstats <- function(my.data){
  
 my.data <- my.data %>%
   group_by(SN, Block) %>% 
   dplyr::summarise(Hand_mean = mean(HandFlip, na.rm = TRUE),
                    RT_mean = mean(RT, na.rm = TRUE),
                    MT_mean = mean(MT, na.rm = TRUE),
                    ST_mean = mean(ST, na.rm = TRUE),
                    Hand_median = median(HandFlip, na.rm = TRUE), 
                    RT_median = median(RT, na.rm = TRUE), 
                    MT_median = median(MT, na.rm = TRUE), 
                    ST_median = median(ST, na.rm = TRUE), 
                    Hand_SD = sd(Hand_dt, na.rm = TRUE), 
                    RT_SD = sd(RT_dt, na.rm = TRUE), 
                    MT_SD = sd(MT_dt, na.rm = TRUE), 
                    ST_SD = sd(ST_dt, na.rm = TRUE))
 return(my.data)
}

plotind <- function(my.data, plot_these_subjects){
  
  plot_list <- vector("list", length = length(plot_these_subjects))
  counter <- 1
  for(si in plot_these_subjects){
    
    my_ind_plot <- my.data %>% 
      filter(SN == si) %>% 
      ggplot(aes(x = TN, y = HandFlip)) + 
      geom_line() + ggtitle(si)
    
    plot_list[[counter]] <- my_ind_plot
    counter <- counter + 1
  }
  return(plot_list)
}


assign_CCW <- function(my.data){
  
  my.data$CCW <- NaN
  
  for(si in unique(my.data$SN)){
    
    idx <- my.data$SN == si
    
    if(unique(my.data$clamp_size[idx])[2] < 0){
      my.data$CCW[idx] <- 0
    }else{
      my.data$CCW[idx] <- 1
    }
  }
  return(my.data)
}


#### MAKE TARGET FILE  ####

pseudo.rand <- function(targets, number_of_cycles){
  
  number_of_targets <- length(targets)
  
  # shuffle targets
  target_output <- array(NaN, number_of_targets * number_of_cycles)
  counter <- 1 
  
  for(si in 1:number_of_cycles){
    
    start_trial <- counter
    end_trial <- counter + number_of_targets - 1
    temp_order <- sample(number_of_targets)
    tgt_order <- targets[temp_order]
    target_output[start_trial : end_trial] <- tgt_order
    counter <- counter + number_of_targets 
  }
  
  return(target_output)
  
}


#### PLOTTING ####

call_aesthethics <- function(text_size){
  
  th <- theme(   panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(size = 0.5), 
                 legend.position = 'right', 
                 legend.text = element_text(size= text_size, family="Helvetica"),
                 text = element_text(size= text_size, family="Helvetica"), 
                 strip.text.x = element_text(size = rel(0.90)), 
                 strip.text.y = element_text(size = rel(0.90)), 
                 axis.title.x = element_text(vjust=-0.3), 
                 plot.title = element_text(hjust = 0.5, vjust = 0), 
                 axis.ticks = element_line(size = 0.4), 
                 axis.text.x.bottom  = element_text(size = rel(0.90), margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
                 axis.title.y = element_text(vjust = 1),
                 axis.text.y = element_text(size = rel(0.90), margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
                 axis.ticks.length = unit(-1.2, "mm"),
                 axis.text.x.top = element_text(size = rel(0.90), margin = unit(c(t = 0, r = 0, b = 2.5, l = 0), "mm")))
  
  return(th)
}


my.colors <-  c('darkred', "#006600", "#800080")
my.colors2 <-  c("#006600", "#800080", "#FF9900", 'deepskyblue4')


#### ALL MY LIBRARIES ####

call_tara_libraries <- function(){
  library(dplyr)
  library(ggplot2)
  library(pracma)
  library(plotrix)
}

call_libraries <- function(){
  library(skimr)
  library(stringr)
  library(effectsize)
  library(forcats)
  library(janitor)
  library(visdat)
  library(emmeans)
  library(see)
  library(performance)
  library(permuco)
  library(lmPerm)
  library(quantreg)
  library(nptest)
  library(EnvStats)
  library(L1pack)
  library(nonpar)
  library(formattable)
  library(psych)
  library(lme4)
  library(car) 
  library(ggplot2)
  library(lattice)
  library(reshape)
  library(plotrix)
  library(gdata)
  library(tidyr)
  library(gridExtra)
  library(nlme)
  library(JM)
  library(multcomp)
  library(FSA)
  library(pbkrtest)
  library(MuMIn)
  library(nloptr)
  library(progress)
  library(lsr)
  library(dplyr)
  library(quickpsy)
  library(ggpubr)
  library(pracma)
  library(effects)
  library(ggsci)
  library(ggthemes)
  library(lemon)
  library(GGally)
  library(extrafont)
  library(ggfortify)
  library(ggExtra)
  library(NlcOptim)
  library(gtsummary)
  library(sjstats)
  library(tidyverse)
  library(fastDummies)
  library(mlbench)
  library(caret)
  library(Hmisc)
  library(corrplot)
  library(randomForest)
  library(plotly)
  library(plyr)
  library(readr)
  library(dplyr)
  library(caret)
  library(repr)
  library(glmnet)
  library(MASS)
  library(MXM)
  library(gglasso)
  library(gridExtra)
  library(gt)
  library(scales)
  library(readr)
  library(tidyverse)
  library(fastDummies)
  library(Boruta)
  library(mlbench)
  library(caret)
  library(Hmisc)
  library(corrplot)
  library(randomForest)
  library(plotly)
  library(GGally)
  library(data.table)
  library(lubridate)
  library(writexl)
  library(ggbreak)
  library(ggnewscale)
  library(viridis)
}

#### OTHER USEFUL FUNCTIONS #####

':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

#### SIMULATING ####

onerate_ss <- function(A, B, initial_hand, Rot){
  
  num_trials <- length(Rot)
  x_state <- setNames(as.data.frame(matrix(0, ncol = 2, nrow = num_trials)),        
                      c('TN', 'Hand'))
  x_state$TN[1] <- 1
  x_state$Hand[1] <- initial_hand
  
  # estimating the state on the current trial 
  for(ti in 2:num_trials){
    x_state$TN[ti] <- ti
    x_state$Hand[ti] <- A*x_state$Hand[ti - 1] + B * ( Rot[ti - 1] - x_state$Hand[ti - 1])
  }
  return(x_state[, 2])
}


tworate_ss <- function(As, Bs, Af, Bf, initial_hand, Rot, slownoise, fastnoise, outputnoise){
  
  num_trials <- length(Rot)
  x_state <- setNames(as.data.frame(matrix(0, ncol = 4, nrow = num_trials)),        
                      c('TN', 'Hand', 'Handfast', 'Handslow'))
  x_state$TN[1] <- 1
  x_state$Hand[1] <- initial_hand
  x_state$Handslow[1] <- initial_hand
  x_state$Handfast[1] <- initial_hand
  
  for(ti in 2:num_trials){
    x_state$TN[ti] <- ti
    slowupdate <-  Bs * ( Rot[ti - 1] - x_state$Hand[ti - 1])
    fastupdate <- Bf * ( Rot[ti - 1] - x_state$Hand[ti - 1])
    
    x_state$Handslow[ti] <- As*x_state$Handslow[ti - 1] + slowupdate + rnorm(1, mean = 0, sd = abs(slowupdate) * slownoise)
    x_state$Handfast[ti] <- Af*x_state$Handfast[ti - 1] + fastupdate + rnorm(1, mean = 0, sd = abs(fastupdate) * fastnoise)
    
    x_state$Hand[ti] <- x_state$Handslow[ti] + x_state$Handfast[ti] + rnorm(1, mean = 0, sd = outputnoise)
  }
  return(x_state[, 2:4])
}

#### SUMMARY STATISTICS (OBSOLETE) ####

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

data_summary_med <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = median(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

data_summary_grp <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sem = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

data_summary_grp_med <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(median = median(x[[col]], na.rm=TRUE),
      #q1 = as.numeric( quantile(x[[col]], 0.25, na.rm=TRUE)), 
      #q3 = as.numeric( quantile(x[[col]], 0.75, na.rm=TRUE)))
      sem = mad(x[[col]], constant = 1,  na.rm=TRUE)) 
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("median" = varname))
  return(data_sum)
}

data_summary_count <- function(data, varname, groupnames){
  
  require(plyr)
  summary_func <- function(x, col){
    c(count = length(x[[col]]))
  }
  
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("count" = varname))
  return(data_sum)
}

data_summary_sum <- function(data, varname, groupnames){
  
  require(plyr)
  summary_func <- function(x, col){
    c(sum = sum(x[[col]]))
  }
  
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("sum" = varname))
  return(data_sum)
}




