.libPaths("/rigel/rent/users/zl2817/rpackages/")
library('dequer')


#generate data, 2 models with 2 exp dist
get_data <- function(parm, size){
  data <- list()
  for (i in 1:length(parm)) {
    temp_data <- rexp(size[i], parm[i])
    while (any(duplicated(temp_data))) {
      temp_data <- rexp(size[i], parm[i])
    }
    data[[i]] <- temp_data 
  }
  return(data)
}

# simulate to get the expectation
get_sim <- function(input_data, num_sim){
  data_size <- lapply(input_data, length)
  # queue for each node/link
  queues <- list()
  inflow_time <- vector()
  wait <- vector()
  mess_type <- vector()
  mess_timein <- vector()
  mess_timeout <- vector()
  mess_len <- vector()
  mess_stage <- vector()
  # mess_outidx <- vector()
#  max_count <- 1e4
  
  
  for (r in 1:num_sim) {
    # initialize empty queue for each node/link
    for (i in 1:8) {
      queues[[i]] <- queue()
    }
    
    # time of next event for each node/link
    trans_time <- rep(Inf, 8)
    
    # arrival time for each type
    for (i in 1:12) {
      inflow_time[i] <- input_data[[i]][ceiling(runif(1)*data_size[[i]])]
    }
    
    # message length for each type
    inflow_len <- input_data[[13]][ceiling(runif(12)*data_size[[13]])]
    
    count <- 0
    # sys_time <- 0
    # busy <- TRUE
    depart_count <- 0
    while (depart_count < horizon) {
      idx1 <- which.min(trans_time) # index for server
      idx2 <- which.min(inflow_time) # index for type of message
      # sys_time <- min(trans_time[idx1], inflow_time[idx2])
      if (trans_time[idx1] > inflow_time[idx2]) {
        # record info about the new arrival
        count <- count + 1
        mess_type[count] <- idx2
        mess_timein[count] <- inflow_time[idx2]
        mess_len[count] <- inflow_len[idx2]
        mess_stage[count] <- 1
        # mess_outidx[count] <- FALSE
        
        # push it into the queue of the entering node
        node <- path[[idx2]][1]
        pushback(queues[[node]], count)
        
        # update the event time at the entering node if needed
        if (length(queues[[node]]) == 1) {
          trans_time[node] <- inflow_time[idx2] + proc_time
        }
        
        # generate a new arrival
        inflow_time[idx2] <- inflow_time[idx2] +
          input_data[[idx2]][ceiling(runif(1)*data_size[[idx2]])]
        inflow_len[idx2] <- input_data[[13]][ceiling(runif(1)*data_size[[13]])]
      } else if (idx1 <= 4) {
        # update the queue of the node
        mess_idx <- pop(queues[[idx1]])
        
        # pass to the next link, or depart the network
        if (mess_stage[mess_idx] < length(path[[mess_type[mess_idx]]])) {
          # if pass to link: update the queue of the link
          mess_stage[mess_idx] <- mess_stage[mess_idx] + 1
          next_link <- path[[mess_type[mess_idx]]][mess_stage[mess_idx]]
          pushback(queues[[next_link]], mess_idx)
          # if the queue is of length one, update the event time of the link
          if (length(queues[[next_link]]) == 1) {
            trans_time[next_link] <- trans_time[idx1] + mess_len[mess_idx]/queue_speed +
              chan_length[next_link - 4]/trans_speed
          }
        } else {
          # if depart the network: record the depart time
          mess_timeout[mess_idx] <- trans_time[idx1]
          # mess_outidx[mess_idx] <- TRUE
          depart_count <- depart_count + as.numeric(mess_idx <= horizon)
        }
        
        # update the event time of the node
        if (length(queues[[idx1]]) > 0) {
          trans_time[idx1] <- trans_time[idx1] + proc_time
        } else {
          trans_time[idx1] <- Inf
        }
      } else {
        # update the queue of the link
        mess_idx <- pop(queues[[idx1]])
        
        # pass to the next node
        # update the queue and event time of the node
        mess_stage[mess_idx] <- mess_stage[mess_idx] + 1
        next_node <- path[[mess_type[mess_idx]]][mess_stage[mess_idx]]
        pushback(queues[[next_node]], mess_idx)
        if (length(queues[[next_node]]) == 1) {
          trans_time[next_node] <- trans_time[idx1] + proc_time
        }
        
        # update the event time of the link
        if (length(queues[[idx1]]) > 0) {
          mess_idx <- as.list(queues[[idx1]])[[1]]
          trans_time[idx1] <- trans_time[idx1] + mess_len[mess_idx]/queue_speed +
            chan_length[idx1 - 4]/trans_speed
        } else {
          trans_time[idx1] <- Inf
        }
      }
      
      # check if a busy period ends
      # busy <- FALSE
      # for (i in 1:8) {
      #   if (length(queues[[i]]) > 0){
      #     busy <- TRUE
      #     break
      #   }
      # }
      # busy <- busy & (count <= max_count)
    }
    
    # compute average delay
    # if (any(is.na(mess_timeout[1:horizon]))) {
    #   stop('error')
    # }
    wait[r] <- mean(mess_timeout[1:horizon] - mess_timein[1:horizon])
  }
  
  return(list(mean = mean(wait), variance = var(wait)))
}

# percentile bootstrap
percentile_bootstrap <- function(input_data, B, R){
  data_size <- lapply(input_data, length)
  
  resample <- list()
  sim_mean <- rep(0, B)
  
  for (b in 1:B){
    for (i in 1:length(data_size)) {
      resample[[i]] <- sample(input_data[[i]], data_size[[i]], replace = TRUE)
    }
    sim_output <- get_sim(resample, R)
    sim_mean[b] <- sim_output$mean
  }

  return(list(left_CI = quantile(sim_mean, c(0.025))[[1]], right_CI = quantile(sim_mean, c(0.975))[[1]]))
}


# subsampled bootstrap
sub_bootstrap <- function(input_data, theta, B, R){
  data_size <- lapply(input_data, length)
  s <- data_size
  for (i in 1:length(s)) {
    s[[i]] <- floor(s[[i]]*theta)
  }
  
  resample <- list()
  sim_mean <- rep(0, B)
  sim_var <- rep(0, B)
  
  for (b in 1:B){
    for (i in 1:13) {
      resample[[i]] <- sample(input_data[[i]], s[[i]], replace = TRUE)
    }
    sim_output <- get_sim(resample, R)
    sim_mean[b] <- sim_output$mean
    sim_var[b] <- sim_output$variance
  }
  
  sim_var_mean <- mean(sim_var)
  var_sim_mean <- var(sim_mean)
  return(list(input_variance = theta*(max(var_sim_mean - sim_var_mean/R, 0)),
              mean = mean(sim_mean), sim_variance = sim_var_mean,
              bootstrap_variance = var_sim_mean/B))
}

# subsampled infinitesimal jackknife
sub_ij <- function(input_data, theta, B, R){
  data_size <- lapply(input_data, length)
  s <- data_size
  for (i in 1:length(s)) {
    s[[i]] <- floor(s[[i]]*theta)
  }

  resample <- list()
  count <- list()
  sim_mean <- rep(0, B)
  sim_var <- rep(0, B)

  for (i in 1:length(data_size)) {
    count[[i]] <- matrix(-s[[i]]/data_size[[i]], nrow = B, ncol = data_size[[i]])
  }


  for (b in 1:B){
    for (i in 1:length(data_size)) {
      idx <- sample(data_size[[i]], s[[i]], replace = TRUE)
      resample[[i]] <- input_data[[i]][idx]
      for (k in 1:s[[i]]){
        count[[i]][b, idx[k]] <- count[[i]][b, idx[k]] + 1
      }

      # resample[[i]] <- sample(input_data[[i]], s[[i]], replace = TRUE)
      # resample_count <- as.data.frame(table(factor(resample[[i]], levels = input_data[[i]])))
      # count[[i]][b,] <- resample_count[,2]
    }
    sim_output <- get_sim(resample, R)
    sim_mean[b] <- sim_output$mean
    sim_var[b] <- sim_output$variance
  }


  cov_squared <- 0
  bias_correction <- 0
  sim_mean_centered <- sim_mean - mean(sim_mean)
  for (i in 1:length(data_size)) {
    cov_squared <- cov_squared + sum((sim_mean_centered %*% count[[i]])^2)/(B - 1)/B
    bias_correction <- bias_correction + sum((sim_mean_centered^2) %*% (count[[i]]^2))/B
    # for (j in 1:1:data_size[[i]]) {
    #   cov_squared <- cov_squared + cov(count[[i]][,j], sim_mean)^2*(B - 1)/B
    #   bias_correction <- bias_correction + mean(((count[[i]][,j] - s[[i]]/data_size[[i]])*(sim_mean - mean(sim_mean)))^2)
    # }
  }

  sim_var_mean <- mean(sim_var)
  var_sim_mean <- var(sim_mean)
  return(list(input_variance = max(cov_squared - bias_correction/(B - 1), 0),
              mean = mean(sim_mean), sim_variance = sim_var_mean,
              bootstrap_variance = var_sim_mean/B))
}

# proportionate pre-subsampled IJ
psij <- function(input_data, theta, B, R){
  data_size <- lapply(input_data, length)
  s <- data_size
  for (i in 1:length(s)) {
    s[[i]] <- floor(s[[i]]*theta)
  }

  resample <- list()

  for (i in 1:13) {
    resample[[i]] <- sample(input_data[[i]], s[[i]], replace = FALSE)
  }      

  sij_output <- sub_ij(resample, 1, B, R)
  return(list(input_variance = theta*sij_output$input_variance))
}

get_inputvar <- function(parm, size, B, R){
  sim_mean <- rep(0, B)
  sim_var <- rep(0, B)
  
  for (b in 1:B){
    input_data <- get_data(parm, size)
    sim_output <- get_sim(input_data, R)
    sim_mean[b] <- sim_output$mean
    sim_var[b] <- sim_output$variance
  }
  
  sim_var_mean <- mean(sim_var)
  input_var <- max(var(sim_mean) - sim_var_mean/R, 0)
  error <- 2*(input_var + sim_var_mean/R)^2/(B - 1) + 2*(sim_var_mean/R)^2/(B*(R - 1))
  nu <- 2*input_var^2/error
  
  return(list(input_variance = input_var,
              CI = c(nu*input_var/qchisq(0.975, nu), nu*input_var/qchisq(0.025, nu))))
}


get_truth <- function(parm, num_sim){
  # queue for each node/link
  queues <- list()
  inflow_time <- vector()
  wait <- vector()
  mess_type <- vector()
  mess_timein <- vector()
  mess_timeout <- vector()
  mess_len <- vector()
  mess_stage <- vector()
  # mess_outidx <- vector()
  # max_count <- 1e4
  
  
  for (r in 1:num_sim) {
    # initialize empty queue for each node/link
    for (i in 1:8) {
      queues[[i]] <- queue()
    }
    
    # time of next event for each node/link
    trans_time <- rep(Inf, 8)
    
    # arrival time for each type
    inflow_time <- rexp(12, parm[1:12])
    
    # message length for each type
    inflow_len <- rexp(12, parm[13])
    
    count <- 0
    # sys_time <- 0
    # busy <- TRUE
    depart_count <- 0
    while (depart_count < horizon) {
      idx1 <- which.min(trans_time) # index for server
      idx2 <- which.min(inflow_time) # index for type of message
      # sys_time <- min(trans_time[idx1], inflow_time[idx2])
      if (trans_time[idx1] > inflow_time[idx2]) {
        # record info about the new arrival
        count <- count + 1
        mess_type[count] <- idx2
        mess_timein[count] <- inflow_time[idx2]
        mess_len[count] <- inflow_len[idx2]
        mess_stage[count] <- 1
        # mess_outidx[count] <- FALSE
        
        # push it into the queue of the entering node
        node <- path[[idx2]][1]
        pushback(queues[[node]], count)
        
        # update the event time at the entering node if needed
        if (length(queues[[node]]) == 1) {
          trans_time[node] <- inflow_time[idx2] + proc_time
        }
        
        # generate a new arrival
        inflow_time[idx2] <- inflow_time[idx2] + rexp(1, parm[idx2])
        inflow_len[idx2] <- rexp(1, parm[13])
      } else if (idx1 <= 4) {
        # update the queue of the node
        mess_idx <- pop(queues[[idx1]])
        
        # pass to the next link, or depart the network
        if (mess_stage[mess_idx] < length(path[[mess_type[mess_idx]]])) {
          # if pass to link: update the queue of the link
          mess_stage[mess_idx] <- mess_stage[mess_idx] + 1
          next_link <- path[[mess_type[mess_idx]]][mess_stage[mess_idx]]
          pushback(queues[[next_link]], mess_idx)
          # if the queue is of length one, update the event time of the link
          if (length(queues[[next_link]]) == 1) {
            trans_time[next_link] <- trans_time[idx1] + mess_len[mess_idx]/queue_speed +
              chan_length[next_link - 4]/trans_speed
          }
        } else {
          # if depart the network: record the depart time
          mess_timeout[mess_idx] <- trans_time[idx1]
          # mess_outidx[mess_idx] <- TRUE
          depart_count <- depart_count + as.numeric(mess_idx <= horizon)
        }
        
        # update the event time of the node
        if (length(queues[[idx1]]) > 0) {
          trans_time[idx1] <- trans_time[idx1] + proc_time
        } else {
          trans_time[idx1] <- Inf
        }
      } else {
        # update the queue of the link
        mess_idx <- pop(queues[[idx1]])
        
        # pass to the next node
        # update the queue and event time of the node
        mess_stage[mess_idx] <- mess_stage[mess_idx] + 1
        next_node <- path[[mess_type[mess_idx]]][mess_stage[mess_idx]]
        pushback(queues[[next_node]], mess_idx)
        if (length(queues[[next_node]]) == 1) {
          trans_time[next_node] <- trans_time[idx1] + proc_time
        }
        
        # update the event time of the link
        if (length(queues[[idx1]]) > 0) {
          mess_idx <- as.list(queues[[idx1]])[[1]]
          trans_time[idx1] <- trans_time[idx1] + mess_len[mess_idx]/queue_speed +
            chan_length[idx1 - 4]/trans_speed
        } else {
          trans_time[idx1] <- Inf
        }
      }
      
      # check if a busy period ends
      # busy <- FALSE
      # for (i in 1:8) {
      #   if (length(queues[[i]]) > 0){
      #     busy <- TRUE
      #     break
      #   }
      # }
      # busy <- busy & (count <= max_count)
    }
    
  
    # compute average delay
    # if (count <= max_count) {
    #   if (any(is.na(mess_timeout[1:count]))) {
    #     stop('error')
    #   }
    # } else {
    #   mess_timeout[1:count][!mess_outidx[1:count]] <- sys_time
    # }
    wait[r] <- mean(mess_timeout[1:horizon] - mess_timein[1:horizon])
    
    # if (r %% 1000 == 0) {
    #   print(r)
    # }
  }
  
  mean <- mean(wait)
  variance <- var(wait)
  return(list(mean = mean, CI = c(mean - 1.96*sqrt(variance/num_sim),
                                  mean + 1.96*sqrt(variance/num_sim))))
}