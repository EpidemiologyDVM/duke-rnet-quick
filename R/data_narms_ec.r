#'E. coli Isolate information from Nat'l Antimicrobial Resistance Monitoring System.
#'
#'A dataset of isolate resistance and metadata for 14,418 E. coli isolates from commercial chicken breasts and chicken carcasses rinsates. Resistance data stored log2 transformed minimum inhibitory concentration (MIC). Columns beyond SAMPLE_ID, Year, and SOURCE are log2(MICs). See source for key for antimicrobials.
#'@name NARMS_EC_DATA
#'
#'@docType data
#'
#'@usage data(NARMS_EC_DATA)
#'
#'@format A dataframe from with 14,418 rows and 28 columns
#' \describe{
#'	\item{SAMPLE_ID}{NARMS-assigned isolate code}
#'	\item{Year}{Year collected}
#'	\item{SOURCE}{Source of isolate: Retail (chicken breasts) or Slaughter (carcass rinsates)}
#'	\item{AMC - TIO}{log2(MIC) of respective antibiotics (see source for drug codes)}
#'
#' }
#'@source \url{https://www.fda.gov/AnimalVeterinary/SafetyHealth/AntimicrobialResistance/NationalAntimicrobialResistanceMonitoringSystem/ucm416741.htm}
#'
NULL