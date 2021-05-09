#' SimSAUC
#'
#' This is a function to automatically run simulations given a simulated data.
#' This function will split data into training set and testing set for a given times.
#' It then computed SAUC estimator and MLE in training set. And using the estimated model,
#' function computed error rate in the testing set.
#'
#'
#' @param y binary outcome of a logistic model
#' @param x a dataframe containing markers
#' @param sim_size simulation size
#' @param parallel TRUE if parallel computation is applied
#' @param n_core the numbe of cores used in parallel computation
#' @param ratio threshold for decsion rule
#' @param seed random seed for reproducibility
#'
#' @return 'beta_SAUC mean' is the SAUC estimates from simulations.
#' 'beta_Logistic' is the mean MLE from simulations. 'sd_of_beta_SAUC' is the
#' standard error of SAUC estimates. 'sd_of_beta_Logistic' is the standard error
#' of SAUC estimates. 'error_SAUC' is mean error of SAUC and 'error_Logistic'
#' is mean error of MLE. 'sd_of_error_SAUC' and 'sd_of_error_Logistic' are
#' standard errors of mean errors of SAUC and MLE.
#'
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterSetRNGStream
#' @importFrom parallel parLapply
#' @importFrom parallel mclapply
#' @importFrom parallel stopCluster
#' @importFrom stats sd
#' @importFrom stats runif
#' @importFrom stats binomial
#' @export



SimSAUC <- function(y, x, sim_size, parallel=F, n_core=1, ratio=2/3, seed=2021){
  ind<-y
  z.dat<-x
  set.seed(seed)
  train_size <- as.integer(nrow(z.dat)*ratio)
  id_data <- 1:nrow(z.dat)
  id_train <- matrix(nrow = sim_size, ncol = train_size)
  id_test <- matrix(nrow = sim_size, ncol = nrow(z.dat)-train_size)
  train <- list()
  test <- list()
  prop_case <- list()
  for(i in 1:sim_size){
    id_train[i, ] <- sample(1:nrow(z.dat), train_size)
    id_test[i, ] <- id_data[-id_train[i, ]]
    z_train <- z.dat[id_train[i, ], ]
    ind_train <- ind[id_train[i, ]]
    train[[i]] <- cbind(ind_train, z_train)
    z_test <- z.dat[id_test[i, ], ]
    ind_test <- ind[id_test[i, ]]
    test[[i]] <- cbind(ind_test, z_test)
    prop_case[[i]] <- sum(ind_train)/length(ind_train)
  }
  if(parallel==F){
    res <- lapply(train, SAUC)
    output <- do.call(rbind.data.frame, res)
    res_lr <- lapply(train, Logistic)
    output_lr <- do.call(rbind.data.frame, res_lr)
  }

  if(parallel==T){
    if (Sys.info()[1] == "Windows"){
      RNGkind("L'Ecuyer-CMRG")
      cl <- makeCluster(n_core)
      clusterSetRNGStream(cl, iseed=2021)
      res <- parLapply(cl, train, SAUC)
      output <- do.call(rbind.data.frame, res)
      res_lr <- parLapply(cl, train, Logistic)
      output_lr <- do.call(rbind.data.frame, res_lr)
      stopCluster(cl)
    } else {
      RNGkind("L'Ecuyer-CMRG")
      res <- mclapply (train, SAUC, mc.cores=n_core)
      output <- do.call(rbind.data.frame, res)
      res_lr <- mclapply (train, Logistic, mc.cores=n_core)
      output_lr <- do.call(rbind.data.frame, res_lr)
    }
  }
  colnames(output) <- c("V1", "V2", "V3", "V4")
  colnames(output_lr) <- c("V1", "V2", "V3", "V4")
  beta_mean <- apply(output, 2, mean)
  beta_sd <- apply(output, 2, sd)
  beta_mean_lr <- apply(output_lr, 2, mean)
  beta_sd_lr <- apply(output_lr, 2, sd)

  er <- array(dim = sim_size)
  er_lr <- array(dim = sim_size)
  for(i in 1:sim_size){
    er[i] <- Error(test[[i]], prop_case[[i]], output[i, ])
    er_lr[i] <- Error(test[[i]], prop_case[[i]], output_lr[i, ])
  }
  er_mean <- mean(er)
  er_sd <- sd(er)
  er_mean_lr <- mean(er_lr)
  er_sd_lr <- sd(er_lr)
  sim_res <- list(beta_mean, beta_sd, beta_mean_lr, beta_sd_lr,
                  er_mean, er_sd, er_mean_lr, er_sd_lr)
  names(sim_res) <- c("beta_SAUC", "sd_of_beta_SAUC", "beta_Logistic",
                      "sd_of_beta_Logistic", "error_SAUC", "sd_of_error_SAUC",
                      "error_Logistic", "sd_of_error_Logistic")
  return(sim_res)
}





