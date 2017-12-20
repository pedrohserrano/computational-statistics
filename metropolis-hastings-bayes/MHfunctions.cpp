#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double objdens1(NumericMatrix X, NumericVector theta, NumericVector Y,double alpha=1, double beta=1){
  
  int m=X.nrow();
  int e=X.ncol()+1;
  int j,i;
  double lkh,logprior;
  
  NumericVector productoXBi(m);
  
  for(j = 0; j < m; ++j){
    
    productoXBi(j)=0.0;
    
  }
  lkh=0;
  for (j = 0; j < m; ++j) {
    for (i = 0; i < e-1; ++i) {
      productoXBi(j) = productoXBi(j)+  X(j,i)*theta[i];
    }
    lkh += (-.5/theta[4])*pow(Y(j)-productoXBi(j),2)-log(theta[4]);
  }
  
  // Compute logprior
  logprior =0;
  for(i = 0; i < e-1; ++i) {
    logprior += R::dnorm(theta[i], 0.0, 100, true);
  }
  logprior += R::dgamma(theta[e], alpha, beta, true);
  // Log of target density
  return lkh + logprior;
  
}

// [[Rcpp::export]]
NumericVector proposal1(NumericVector theta,NumericMatrix X){
  int nparam = theta.size();
  double jump=.25/sqrt(X.nrow());
  NumericVector newtheta(nparam);
  for (int i=0; i<nparam; i++){
    newtheta[i] = R::rnorm(theta[i], jump);
  }
  return newtheta;
}

// [[Rcpp::export]]
List MHBayes(int nsim, NumericVector theta0, Function objdens1, Function proposal, NumericMatrix X, NumericVector Y){
  // theta will contain the output, one column pero parameter, row per simulation
  int nparam=theta0.size();
  NumericMatrix theta(nsim, nparam);  
  theta(0,_) = theta0;
  
  // X will save proposals, Rej will save number of rejection rates=(trials-1)/trials
  NumericVector XT(nparam);
  NumericVector rejections(nsim);
  // logU is for the test
  double logU;
  // accept tells wether a proposal is accepted, trials counts attemps before accepting
  bool accept=false;
  // trials max is the maxnumber of inner cycles in what follows, trial the counter
  int trials;
  int maxtrials=100000;
  // outer cycle: sim n jumps
  for (int i=1; i<nsim; i++){
    // inner cycle: repeat until accepting
    trials = 0;
    accept = false;
    while (!accept && trials<maxtrials){
      XT = as<NumericVector>(proposal(theta(i-1,_),X));
      logU = log(R::runif(0,1));
      // the minus is since we used LOGS!!!!!
      if(logU <= as<double>(objdens1(X, XT, Y))-as<double>(objdens1(X, theta(i-1,_),Y))) { 
        accept = true;
        theta(i,_) = XT;
      } 
      trials++;
    }  
    rejections[i] = trials;
  } 
  return List::create(Named("theta")  = theta, Named("rejections")  = rejections);
}

// [[Rcpp::export]]
List SimulacionesYest(NumericMatrix theta, NumericMatrix X){
  int filas=X.nrow(); // 150
  int columnas =theta.nrow(); // 1,000
  int nparam = theta.ncol() -1; // 4
  
  NumericMatrix Simu_Yest(filas, columnas);
  NumericVector Yest(filas);
  
  for (int i=0; i<filas; i++){
    
    for (int j=0; j<columnas; j++){
      
      Simu_Yest(i,j)=0;
      
      for (int z=0; z<nparam; z++){
        
        Simu_Yest(i,j) = Simu_Yest(i,j) + X(i,z)*theta(j,z);
        
      }
      
    }
    
    Yest[i]=mean(Simu_Yest(i,_));
    
  }
  
  return List::create(Named("Simulaciones")  = Simu_Yest, Named("Medias_Simulaciones")  = Yest);
  
}
