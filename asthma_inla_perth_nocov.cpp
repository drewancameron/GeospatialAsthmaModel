#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;
  
  // Input Data
  
  DATA_INTEGER(N_SA1_perth);
  DATA_STRUCT(spde_perth,spde_t); // INLA SPDE object (components of precision matrix)
  DATA_SPARSE_MATRIX(A_perth); // INLA SPDE projection matrix: mesh to pixels [dim: NMB x nMesh]
  
  DATA_VECTOR(logit_pred);
  
  DATA_VECTOR(npos); // [dim: NMB]  
  DATA_VECTOR(nneg); // [dim: NMB]  
  
  DATA_VECTOR(MUA);
  
  // Parameters
  
  PARAMETER(log_range);
  PARAMETER(log_sd);
  
  PARAMETER(intercept_perth);
  
  PARAMETER_VECTOR(field_perth); // [dim: nMesh_perth]
  
  // Parameter Transforms
  
  Type range = exp(log_range);
  Type kappa = 2.8284/range;
  Type sd = exp(log_sd);
  
  // Priors
  
  Type nll = 0.0;
  
  nll -= dnorm(log_sd,Type(-1),Type(0.5),true);
  nll -= dnorm(log_range,Type(1),Type(0.5),true);
  
  SparseMatrix<Type> Q_perth = Q_spde(spde_perth,kappa);
  nll += SCALE(GMRF(Q_perth),sd)(field_perth);
  
  // Algebra
  
  vector<Type> baseline_field_perth(N_SA1_perth);
  baseline_field_perth = A_perth*field_perth;
  

  vector<Type> predicted_surface_asthma_perth(N_SA1_perth);
  predicted_surface_asthma_perth = intercept_perth  + baseline_field_perth.array() + logit_pred.array();
  
  vector<Type> predicted_surface_asthma(N_SA1_perth);
  predicted_surface_asthma = predicted_surface_asthma_perth.array();
  predicted_surface_asthma = invlogit(predicted_surface_asthma).array();
  
  // Likelihood
  
  for (int i=0; i<N_SA1_perth; i++) {
    nll -= MUA[i]*dbinom(npos[i],npos[i]+nneg[i],predicted_surface_asthma[i],true);
  }
  
  // Reporting
  
  REPORT(predicted_surface_asthma);
  
  return nll;
}
