#' SimSAUC
#' @param y binary outcome
#' @param x n*p covariates
#' @param sim_size simulation size
#' @param parallel logic value if parallel computation is applied
#' @param n_core if parallel computation is applied, the numbe of cores used
#' @param ratio decision rule
#' @param seed seed for random split the data into test set and train set
#' @return
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterSetRNGStream
#' @importFrom parallel parLapply
#' @importFrom parallel mclapply
#' @importFrom parallel stopCluster
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





