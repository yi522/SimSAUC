#' SAUC
#'
#' This function gets SAUC estimates of a logistic model while fixing the first component
#' of coefficents to be 1.
#'
#'
#' @param l a matrix containing outcome and markers. Please put outcome in the
#' first column of the matrix, and put markers in the rest columns
#' @return it returns the SAUC estimates of coefficients while fixing the first component
#' of coefficents to be 1.
#' @export



SAUC <- function(l){
  ind <- l[, 1]
  z.dat <- l[, 2:ncol(l)]
  # Cross Validation####
  sigma.n<-0.10; # --- sigma.n selected using another program
  anchor<-1;
  V<- 3; step<- 1000; increase<-0.01; tau<-1;

  samplesize<-nrow(z.dat);
  num.cov<-ncol(z.dat);
  z<-matrix(0, samplesize, num.cov);
  for(i in 1:num.cov){
    z[ ,i]<-z.dat[[i]]
  }

  z<-z/sigma.n;
  aug.cross<-400; aug.eva<-25;
  delete.size<-as.integer(samplesize/V);
  criteria<- rep(0, step);

  cross.position<-rep(0, samplesize);
  for(i in 1:V){
    for(j in 1:delete.size){
      cross.position[(i-1)*delete.size+j]<-i
    }
  }
  #  randomly permute the data, so that there will be no "hiden pattern"
  cross.position<-cross.position[order(runif(samplesize, 0, 1))]

  for(v in 1:V){
    # --- cut data ---
    cross.ind<-ind[cross.position!=v];
    cross.z<-z[cross.position!=v, ];
    case.z<-cross.z[cross.ind==1, ];
    control.z<-cross.z[cross.ind==0, ];

    evaluate.ind<-ind[cross.position==v];
    evaluate.z<-z[cross.position==v, ];
    e.case.z<-evaluate.z[evaluate.ind==1, ];
    e.control.z<-evaluate.z[evaluate.ind==0, ];

    # --- data augmentation ---
    # --- to make different cross validation sets comparable ---
    aug.cross.data<-matrix(0, aug.cross, num.cov);
    for(i in 1:aug.cross){
      p1<-as.integer(runif(1, min=1, max=(nrow(case.z)+1)));
      p2<-as.integer(runif(1, min=1, max=(nrow(control.z)+1)));

      for(j in 1:num.cov){
        aug.cross.data[i, j]<- case.z[p1, j]-control.z[p2, j];
      }
    }

    aug.evaluate.data<-matrix(0, aug.eva, num.cov);
    for(i in 1:aug.eva){
      p1<-as.integer(runif(1, min=1, max=(nrow(e.case.z)+1)));
      p2<-as.integer(runif(1, min=1, max=(nrow(e.control.z)+1)));

      for(j in 1:num.cov){
        aug.evaluate.data[i, j]<-e.case.z[p1, j]-e.control.z[p2, j];
      }
    }

    # --- now estimate ---
    beta.est<-rep(0, num.cov); beta.est[anchor]<- 1;
    h.v<-rep(0, num.cov); f.v<-rep(0, num.cov); g.v<-rep(0, num.cov);

    for(s in 1:step){
      gradient<- rep(0, num.cov);
      for(i in 1:nrow(aug.cross.data)){
        diff.z<-aug.cross.data[i, ];
        s.func<-1/(1+exp(-sum(diff.z*beta.est)));
        gradient<-gradient+s.func*(1-s.func)*diff.z;
      }

      gradient[anchor]<- 0;

      # -- standardized gradient ---
      if(max(abs(gradient)) >0.000001){
        for(i in 1:length(g.v))
          g.v[i]<-gradient[i]/max(abs(gradient));
      }
      if(max(abs(gradient)) <= 0.000001){
        for(i in 1:length(g.v))
          g.v[i]<-0.0;
      }

      f.v<-ifelse(abs(g.v)>=tau, 1, 0);

      for(i in 1:length(g.v))
        h.v[i]<-f.v[i]*g.v[i];

      beta.est<-beta.est+increase*h.v;

      # --- now evaluate ---
      cri.temp<-0;
      for(p in 1:nrow(aug.evaluate.data)){
        e.t.betaz<-sum(aug.evaluate.data[p, ]*beta.est);
        cri.temp<-cri.temp+1/(1+exp(-e.t.betaz));
      }
      if(is.na(cri.temp))
        cri.temp<-0;
      criteria[s]<-criteria[s]+cri.temp;
    } # --- end of step ---
  }

  ss<-seq(from=1, to=step, by=1);
  step.est<-ss[criteria==max(criteria)];

  # Estimation####
  sigma.n<-0.10 # --- sigma.n selected using another program
  anchor<-1
  step<- step.est    # --- number of iterations estimated from cross validation
  increase<-0.01
  tau<-1

  samplesize<-nrow(z.dat)
  num.cov<-ncol(z.dat)

  z<-matrix(0, samplesize, num.cov);
  for(i in 1:num.cov){
    z[ ,i]<-z.dat[[i]]
  }

  z<-z/sigma.n;
  aug.cross<-600;
  case.z<-z[ind==1, ]; control.z<-z[ind==0, ];

  aug.cross.data<-matrix(0, aug.cross, num.cov);
  for(i in 1:aug.cross){
    p1<-as.integer(runif(1, min=1, max=(nrow(case.z)+1)));
    p2<-as.integer(runif(1, min=1, max=(nrow(control.z)+1)));

    for(j in 1:num.cov){
      aug.cross.data[i, j]<- case.z[p1, j]-control.z[p2, j];
    }
  }

  beta.est<-rep(0, num.cov);  beta.est[anchor]<- 1.0;
  h.v<-rep(0, num.cov); f.v<-rep(0, num.cov); g.v<-rep(0, num.cov);

  for(s in 1:step){
    gradient<- rep(0, num.cov);
    for(i in 1:nrow(aug.cross.data)){
      diff.z<-aug.cross.data[i, ];
      s.func<-1/(1+exp(-sum(diff.z*beta.est)));
      gradient<-gradient+s.func*(1-s.func)*diff.z;
    }

    gradient[anchor]<- 0;

    # -- standardized gradient ---
    if(max(abs(gradient)) >0.000001){
      for(i in 1:length(g.v))
        g.v[i]<-gradient[i]/max(abs(gradient));
    }
    if(max(abs(gradient)) <= 0.000001){
      for(i in 1:length(g.v))
        g.v[i]<-0.0;
    }

    f.v<-ifelse(abs(g.v)>=tau, 1, 0);

    for(i in 1:length(g.v))
      h.v[i]<-f.v[i]*g.v[i];

    beta.est<-beta.est+increase*h.v;
  }
  return(beta.est)
}
