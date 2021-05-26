ABX_LIST <- c(
  'AMP',
  'AMC',
  'AXO',
  'FOX',
  'TIO',
  'STR',
  'GEN',
  'KAN',
  'NAL',
  'CIP',
  'FIS',
  'COT',
  'AZI',
  'CHL',
  'TET'
)

EC_Rnet <- Rnet(
  NARMS_EC_DATA,
  L1 = 0.25,
  V_set = ABX_LIST
)

Assign_Vmetadata(EC_Rnet, V_ATTRS)

ECall_Rnet <- Rnet(
  NARMS_EC_DATA,
  L1 = 0.25,
  Stratify = 'Year',
  V_set = ABX_LIST
)

Assign_Vmetadata(ECall_Rnet, V_ATTRS)

EC08_Rnet <- Rnet(
  NARMS_EC_DATA,
  L1 = 0.25,
  Stratify = NARMS_EC_DATA$Year == 2008,
  V_set = ABX_LIST
  )

Assign_Vmetadata(EC08_Rnet, V_ATTRS)


#DATA FRAME
EC08_edgeFrame <- cbind(as.data.frame(as_edgelist(EC08_Rnet@R)), omega = E(EC08_Rnet@R)$omega)

EC08_edgeFrame$Class1 <- V_ATTRS$Class[match(EC08_edgeFrame$V1, V_ATTRS$Code)]
EC08_edgeFrame$Class2 <- V_ATTRS$Class[match(EC08_edgeFrame$V2, V_ATTRS$Code)]

signedModularity(EC08_edgeFrame, 'Class')
signedModularity(EC08_edgeFrame, 'Class', weight = 'omega')


#MATRIX
EC08_A <- EC08_Rnet@A
attr(EC08_A, 'Class') <- V(EC08_Rnet@R)$Class

EC08_omega <- EC08_Rnet@Omega
attr(EC08_omega, 'Class') <- V(EC08_Rnet@R)$Class


signedModularity(EC08_A, 'Class')
signedModularity(EC08_omega, 'Class')


#IGRAPH
signedModularity(EC08_Rnet@R, 'Class')

signedModularity(EC08_Rnet@R, 'Class', 'omega')


#RNET-BASIC
signedModularity(EC_Rnet, 'Class')
signedModularity(EC_Rnet, 'Class', 'omega')


#RNET-STRATA
signedModularity(EC08_Rnet, 'Class')
signedModularity(EC08_Rnet, 'Class', 'omega')


#RNET-MULTI
signedModularity(ECall_Rnet, 'Class')
signedModularity(ECall_Rnet, 'Class', 'omega')
