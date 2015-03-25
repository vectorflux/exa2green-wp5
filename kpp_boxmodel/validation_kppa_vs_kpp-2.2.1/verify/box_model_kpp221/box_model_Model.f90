MODULE box_model_Model

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Completely defines the model box_model
!    by using all the associated modules
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  USE box_model_Precision
  USE box_model_Parameters
  USE box_model_Global
  USE box_model_Function
  USE box_model_Integrator
  USE box_model_Rates
  USE box_model_Jacobian
  USE box_model_Hessian
  USE box_model_Stoichiom
  USE box_model_LinearAlgebra
  USE box_model_Monitor
  USE box_model_Util

END MODULE box_model_Model

