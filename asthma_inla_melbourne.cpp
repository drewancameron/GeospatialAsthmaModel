#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;
  
  // Input Data
  
  DATA_INTEGER(N_SA1_melbourne);
  DATA_STRUCT(spde_melbourne,spde_t); // INLA SPDE object (components of precision matrix)
  DATA_SPARSE_MATRIX(A_melbourne); // INLA SPDE projection matrix: mesh to pixels [dim: NMB x nMesh]
  
  DATA_INTEGER(N_pollution);
  DATA_MATRIX(pollution_covariates);
  
  DATA_INTEGER(N_dry_cold);
  DATA_MATRIX(dry_cold_covariates);
  
  DATA_INTEGER(N_humid_hot);
  DATA_MATRIX(humid_hot_covariates);
  
  DATA_INTEGER(N_vegetation);
  DATA_MATRIX(vegetation_covariates);
  
  DATA_IVECTOR(ses);
  
  DATA_VECTOR(lstdiff);
  
  DATA_VECTOR(npos); // [dim: NMB]  
  DATA_VECTOR(nneg); // [dim: NMB]  
  
  DATA_VECTOR(MUA);
  
  // Parameters
  
  PARAMETER(log_range);
  PARAMETER(log_sd);
  
  PARAMETER(logit_ses_par);
  PARAMETER(log_ses_sd);
  
  PARAMETER_VECTOR(log_pollution_norm);
  
  PARAMETER(log_interaction_pollution_cold);
  PARAMETER(log_interaction_pollution_hot);
  PARAMETER(log_interaction_vegetation_cold);
  PARAMETER(log_interaction_vegetation_hot);
  
  PARAMETER(intercept_melbourne);
  
  PARAMETER_VECTOR(log_pollution_slopes);
  PARAMETER_VECTOR(log_dry_cold_slopes);
  PARAMETER_VECTOR(log_humid_hot_slopes);
  PARAMETER_VECTOR(log_vegetation_slopes);
  
  PARAMETER(log_lstdiff_slopes);
  
  PARAMETER_VECTOR(ses_slopes);
  
  PARAMETER_VECTOR(field_melbourne); // [dim: nMesh_melbourne]
  
  // Parameter Transforms
  
  Type range = exp(log_range);
  Type kappa = 2.8284/range;
  Type sd = exp(log_sd);
  
  // Priors
  
  Type nll = 0.0;
  
  nll -= dnorm(log_sd,Type(-1),Type(0.5),true);
  nll -= dnorm(log_range,Type(1),Type(0.5),true);
  
  nll -= dnorm(exp(log_pollution_slopes),Type(0),Type(1.0),true).sum()+log_pollution_slopes.sum();
  nll -= dlgamma(log_pollution_norm,Type(1),Type(1),true).sum();
  
  nll -= dnorm(exp(log_dry_cold_slopes),Type(0),Type(1.0),true).sum()+log_dry_cold_slopes.sum();
  
  nll -= dnorm(exp(log_humid_hot_slopes),Type(0),Type(1.0),true).sum()+log_humid_hot_slopes.sum();
  
  nll -= dnorm(exp(log_vegetation_slopes),Type(0),Type(1.0),true).sum()+log_vegetation_slopes.sum();
  
  nll -= dnorm(logit_ses_par,Type(3),Type(1.0),true);
  nll -= dnorm(log_ses_sd,Type(-2),Type(1.0),true);
  nll += SCALE(AR1(invlogit(logit_ses_par)),exp(log_ses_sd))(ses_slopes);
  
  nll -= dnorm(exp(log_lstdiff_slopes),Type(0),Type(1.0),true)+log_lstdiff_slopes;
  nll -= dnorm(exp(log_interaction_pollution_hot),Type(0),Type(1.0),true)+log_interaction_pollution_hot;
  nll -= dnorm(exp(log_interaction_pollution_cold),Type(0),Type(1.0),true)+log_interaction_pollution_cold;
  nll -= dnorm(exp(log_interaction_vegetation_hot),Type(0),Type(1.0),true)+log_interaction_vegetation_hot;
  nll -= dnorm(exp(log_interaction_vegetation_cold),Type(0),Type(1.0),true)+log_interaction_vegetation_cold;
  
  SparseMatrix<Type> Q_melbourne = Q_spde(spde_melbourne,kappa);
  nll += SCALE(GMRF(Q_melbourne),sd)(field_melbourne);
  
  // Algebra
  
  vector<Type> pollution_norm = exp(log_pollution_norm);
  pollution_norm = pollution_norm/pollution_norm.sum();
  
  vector<Type> baseline_field_melbourne(N_SA1_melbourne);
  baseline_field_melbourne = A_melbourne*field_melbourne;
  
  matrix<Type> pollution_expanded(N_SA1_melbourne,N_pollution);
  vector<Type> buffer(N_SA1_melbourne);
  for (int i=0; i<N_pollution; i++) {
    buffer = pollution_covariates.col(i).array()*exp(log_pollution_slopes[i]);
    pollution_expanded.col(i) = buffer;
  }
  vector<Type> pollution_one(N_SA1_melbourne);
  vector<Type> pollution_two(N_SA1_melbourne);
  vector<Type> pollution_four(N_SA1_melbourne);
  pollution_one.setZero();
  pollution_two.setZero();
  pollution_four.setZero();
  for (int i=0; i<N_pollution; i++) {
    pollution_one = pollution_one.array()+pollution_expanded.col(i).array();
    pollution_two = pollution_two.array()+pow(pollution_expanded.col(i).array(),Type(2.0));
    pollution_four = pollution_four.array()+pow(pollution_expanded.col(i).array(),Type(4.0));
  }
  pollution_two = pow(pollution_two.array(),Type(0.5));
  pollution_four = pow(pollution_four.array(),Type(0.25));
  vector<Type> pollution(N_SA1_melbourne);
  pollution = pollution_one.array()*pollution_norm[0]+pollution_two.array()*pollution_norm[1]+pollution_four.array()*pollution_norm[2];
  
  matrix<Type> dry_cold_expanded(N_SA1_melbourne,N_dry_cold);
  for (int i=0; i<N_dry_cold; i++) {
    buffer = dry_cold_covariates.col(i).array()*exp(log_dry_cold_slopes[i]);
    dry_cold_expanded.col(i) = buffer;
  }
  vector<Type> dry_cold_two(N_SA1_melbourne);
  dry_cold_two.setZero();
  for (int i=0; i<N_dry_cold; i++) {
    dry_cold_two = dry_cold_two.array()+pow(dry_cold_expanded.col(i).array(),Type(2.0));
  }
  dry_cold_two = pow(dry_cold_two.array(),Type(0.5));
  vector<Type> dry_cold(N_SA1_melbourne);
  dry_cold = dry_cold_two.array();
  
  matrix<Type> humid_hot_expanded(N_SA1_melbourne,N_humid_hot);
  for (int i=0; i<N_humid_hot; i++) {
    buffer = humid_hot_covariates.col(i).array()*exp(log_humid_hot_slopes[i]);
    humid_hot_expanded.col(i) = buffer;
  }
  vector<Type> humid_hot_two(N_SA1_melbourne);
  humid_hot_two.setZero();
  for (int i=0; i<N_humid_hot; i++) {
    humid_hot_two = humid_hot_two.array()+pow(humid_hot_expanded.col(i).array(),Type(2.0));
  }
  humid_hot_two = pow(humid_hot_two.array(),Type(0.5));
  vector<Type> humid_hot(N_SA1_melbourne);
  humid_hot = humid_hot_two.array();
  
  matrix<Type> vegetation_expanded(N_SA1_melbourne,N_vegetation);
  for (int i=0; i<N_vegetation; i++) {
    buffer = vegetation_covariates.col(i).array()*exp(log_vegetation_slopes[i]);
    vegetation_expanded.col(i) = buffer;
  }
  vector<Type> vegetation_two(N_SA1_melbourne);
  vegetation_two.setZero();
  for (int i=0; i<N_vegetation; i++) {
    vegetation_two = vegetation_two.array()+pow(vegetation_expanded.col(i).array(),Type(2.0));
  }
  vegetation_two = pow(vegetation_two.array(),Type(0.5));
  vector<Type> vegetation(N_SA1_melbourne);
  vegetation = vegetation_two.array();
  
  vector<Type> ses_effects(N_SA1_melbourne);
  for (int i=0; i<N_SA1_melbourne; i++) {ses_effects[i] = ses_slopes[ses[i]];}
  
  vector<Type> predicted_surface_asthma_melbourne(N_SA1_melbourne);
  predicted_surface_asthma_melbourne = intercept_melbourne  + baseline_field_melbourne.array();
  
  vector<Type> predicted_surface_asthma(N_SA1_melbourne);
  predicted_surface_asthma = predicted_surface_asthma_melbourne.array() + pollution.array() + dry_cold.array() + humid_hot.array() + vegetation.array() + ses_effects.array() + exp(log_lstdiff_slopes)*lstdiff.array() + exp(log_interaction_pollution_hot)*humid_hot.array()*pollution.array() + exp(log_interaction_pollution_cold)*dry_cold.array()*pollution.array() + exp(log_interaction_vegetation_hot)*humid_hot.array()*vegetation.array() + exp(log_interaction_vegetation_cold)*dry_cold.array()*vegetation.array();
  predicted_surface_asthma = invlogit(predicted_surface_asthma).array();
  
  // Likelihood
  
  for (int i=0; i<N_SA1_melbourne; i++) {
    nll -= MUA[i]*dbinom(npos[i],npos[i]+nneg[i],predicted_surface_asthma[i],true);
  }
  
  // Reporting
  
  pollution = pollution.array() + exp(log_interaction_pollution_hot)*humid_hot.array()*pollution.array() + exp(log_interaction_pollution_cold)*dry_cold.array()*pollution.array();
  vegetation = vegetation.array() + exp(log_interaction_vegetation_hot)*humid_hot.array()*vegetation.array() + exp(log_interaction_vegetation_cold)*dry_cold.array()*vegetation.array();
  
  REPORT(pollution);
  REPORT(dry_cold);
  REPORT(humid_hot);
  REPORT(vegetation);
  REPORT(ses_effects);
  vector<Type> lstdiff_effects(N_SA1_melbourne);
  lstdiff_effects = exp(log_lstdiff_slopes)*lstdiff.array();
  REPORT(lstdiff_effects);
  
  REPORT(predicted_surface_asthma);
  
  return nll;
}
