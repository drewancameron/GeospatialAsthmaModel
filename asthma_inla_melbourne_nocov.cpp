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
  
  DATA_VECTOR(logit_pred);
  
  DATA_VECTOR(npos); // [dim: NMB]  
  DATA_VECTOR(nneg); // [dim: NMB]  
  
  DATA_VECTOR(MUA);
  
  // Parameters
  
  PARAMETER(log_range);
  PARAMETER(log_sd);
  
  PARAMETER(intercept_melbourne);
  
  PARAMETER_VECTOR(field_melbourne); // [dim: nMesh_melbourne]
  
  // Parameter Transforms
  
  Type range = exp(log_range);
  Type kappa = 2.8284/range;
  Type sd = exp(log_sd);
  
  // Priors
  
  Type nll = 0.0;
  
  nll -= dnorm(log_sd,Type(-1),Type(0.5),true);
  nll -= dnorm(log_range,Type(1),Type(0.5),true);
  
  SparseMatrix<Type> Q_melbourne = Q_spde(spde_melbourne,kappa);
  nll += SCALE(GMRF(Q_melbourne),sd)(field_melbourne);
  
  // Algebra
  
  vector<Type> baseline_field_melbourne(N_SA1_melbourne);
  baseline_field_melbourne = A_melbourne*field_melbourne;
  

  vector<Type> predicted_surface_asthma_melbourne(N_SA1_melbourne);
  predicted_surface_asthma_melbourne = intercept_melbourne  + baseline_field_melbourne.array() + logit_pred.array();
  
  vector<Type> predicted_surface_asthma(N_SA1_melbourne);
  predicted_surface_asthma = predicted_surface_asthma_melbourne.array();
  predicted_surface_asthma = invlogit(predicted_surface_asthma).array();
  
  // Likelihood
  
  for (int i=0; i<N_SA1_melbourne; i++) {
    nll -= MUA[i]*dbinom(npos[i],npos[i]+nneg[i],predicted_surface_asthma[i],true);
  }
  
  // Reporting
  
  REPORT(predicted_surface_asthma);
  
  return nll;
}
