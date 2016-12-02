#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List trainFM(List j14924k) { int j17031k=(j14924k["iIter"]); double j14311k=(j14924k["dStdev"]); std::vector<int> j12681k = as< std::vector<int> >(j14924k["vK"]); bool j11769k=(j14924k["bIntercept"]); std::vector<double> j19342k = as< std::vector<double> >(j14924k["vLambda"]); NumericMatrix j17949k = as< NumericMatrix >(j14924k["mX"]); std::vector<double> j19422k = as< std::vector<double> >(j14924k["vY"]); if(j12681k.size()<10) j12681k.resize(10,0); int *j14133k; double* j11938k; double* j14250k; double* j11476k; double* j17049k; std::vector<double>* j17565k; double j19107k, j17166k, j11365k; int j14913k; int j16079k=j12681k.size(); std::vector<double> j14850k(j17949k.nrow()); std::vector< std::vector<double> > j11344k(j16079k); std::vector< std::vector<double> > j11546k(j16079k); for(int j11407k=0;j11407k<j16079k;j11407k++){ j11344k[j11407k].resize(j12681k[j11407k]); j11546k[j11407k].resize(j12681k[j11407k]); } for(unsigned int j11407k=0;j11407k<j11344k.size();j11407k++){ for(unsigned int j17883k=0;j17883k<j11344k[j11407k].size();j17883k++){ j11344k[j11407k][j17883k]=0.0; j11546k[j11407k][j17883k]=0.0; } } double j18591k, j14250k2, j11938k2, j10337k, j11818k, j16026k, j17655k; int j17156k=0; int j12924k=0; for(int j17168k = 0; j17168k < j17949k.nrow(); j17168k++ ){ if(j17156k<j17949k(j17168k,0)) j17156k=j17949k(j17168k,0); if(j12924k<j17949k(j17168k,1)) j12924k=j17949k(j17168k,1); } j17156k++; j12924k++; std::vector< std::vector<int> >j17696k(j12924k); std::vector< std::vector<double> >j15146k(j12924k); for(int j17168k = 0; j17168k < j17949k.nrow(); j17168k++ ){ j17696k[j17949k(j17168k,1)].push_back(j17949k(j17168k,0)); j15146k[j17949k(j17168k,1)].push_back(j17949k(j17168k,2)); } double j15306k=*(std::min_element(j19422k.begin(),j19422k.end())); double j19648k=*(std::max_element(j19422k.begin(),j19422k.end())); j19107k=0; for(int j12046k=0;j12046k<j16079k;j12046k++) j19107k+=j12681k[j12046k]; std::vector<double> j11469k(j19107k*j12924k+1,0); std::vector<double> j16615k(j11469k.size(),0); for(unsigned int j11407k=0;j11407k<j11469k.size();j11407k++){ j16026k=0; j17655k=j14311k; do { do { j18591k = R::runif(0,1); } while (j18591k == 0.0); j14250k2 = 1.7156 * (R::runif(0,1) - 0.5); j11938k2 = j18591k - 0.449871; j10337k = std::abs(j14250k2) + 0.386595; j11818k = j11938k2*j11938k2 + j10337k*(0.19600*j10337k-0.25472*j11938k2); if (j11818k < 0.27597) { break; } } while ((j11818k > 0.27846) || ((j14250k2*j14250k2) > (-4.0*j18591k*j18591k*std::log(j18591k)))); j11469k[j11407k]=j16026k+(j17655k*(j14250k2/j18591k)); } std::vector< std::vector< std::vector< std::vector<double> > > > j12192k; j12192k.resize(j16079k); for(int j11407k = 0; j11407k < j16079k; j11407k++ ) { j12192k[j11407k].resize(j12681k[j11407k]); for(int j17883k = 0; j17883k < j12681k[j11407k]; j17883k++ ) { j12192k[j11407k][j17883k].resize(j17156k); for(int j10054k = 0; j10054k < j17156k; j10054k++ ){ j12192k[j11407k][j17883k][j10054k].resize(j11407k+1); for(int j19554k = 0; j19554k < j11407k+1; j19554k++ ) j12192k[j11407k][j17883k][j10054k][j19554k]=0; } } } for(unsigned int j11407k = 0; j11407k < j12192k.size(); j11407k++ ) { for(unsigned int j17883k = 0; j17883k < j12192k[j11407k].size(); j17883k++ ) { for(unsigned int j10054k = 0; j10054k < j12192k[j11407k][j17883k].size(); j10054k++ ){ for(unsigned int j19554k = 0; j19554k < j12192k[j11407k][j17883k][j10054k].size(); j19554k++ ) j12192k[j11407k][j17883k][j10054k][j19554k]=0; } } } j14913k=0; for(int j12046k = 0; j12046k < j16079k; j12046k++ ) { for( int j19452k = 0; j19452k < j12681k[j12046k]; j19452k++ ) { for( int j16856k = 0; j16856k < j12924k; j16856k++ ) { j14913k++; j14250k=&j11469k[j14913k]; for( unsigned int j17168k = 0; j17168k < j17696k[j16856k].size(); j17168k++ ) { j14133k=&j17696k[j16856k][j17168k]; j11938k=&j15146k[j16856k][j17168k]; j17565k=&j12192k[j12046k][j19452k][*j14133k]; for( unsigned int j19543k = 1; j19543k <= (*j17565k).size(); j19543k++ ) { (*j17565k)[j19543k-1]+=pow(*j11938k,j19543k)*pow(*j14250k,j19543k); } } } } } std::vector< std::vector<double> >* j19954k; std::vector<double> j15571k(j17156k,j11469k[0]); for(int j12046k = 0; j12046k < j16079k; j12046k++ ) { for( int j19452k = 0; j19452k < j12681k[j12046k]; j19452k++ ) { j19954k=&j12192k[j12046k][j19452k]; if(j12046k==0) for(unsigned int j17168k = 0; j17168k < (*j19954k).size(); j17168k++ ) j15571k[j17168k]+=(*j19954k)[j17168k][0]; if(j12046k==1) for(unsigned int j17168k = 0; j17168k < (*j19954k).size(); j17168k++ ) j15571k[j17168k]+=(1.0/2.0)*((1*pow((*j19954k)[j17168k][0],2))+(-1*pow((*j19954k)[j17168k][1],1))); if(j12046k==2) for(unsigned int j17168k = 0; j17168k < (*j19954k).size(); j17168k++ ) j15571k[j17168k]+=(1.0/6.0)*((1*pow((*j19954k)[j17168k][0],3))+(-3*pow((*j19954k)[j17168k][1],1)*pow((*j19954k)[j17168k][0],1))+(2*pow((*j19954k)[j17168k][2],1))); } } std::vector<double> j16390k(j17156k,0); for(int j17168k=0;j17168k<j17156k;j17168k++) j16390k[j17168k]=j15571k[j17168k]-j19422k[j17168k]; for(int j13788k=0; j13788k<j17031k; j13788k++){ if(j11769k){ j11365k=0.0; for( unsigned int j17168k=0; j17168k<j16390k.size(); j17168k++ ) j11365k+=(j16390k[j17168k]-j11469k[0]); j11365k/=(-double(j16390k.size())); if(j11365k!=j11365k) j11365k=0; for( unsigned int j17168k=0; j17168k<j16390k.size(); j17168k++ ) j16390k[j17168k]+=(j11365k-j11469k[0]); j11469k[0]=j11365k; } j14913k=0; j17049k=&j19342k[0]; for( int j19452k = 0; j19452k < j12681k[0]; j19452k++ ) { for( int j16856k = 0; j16856k < j12924k; j16856k++ ) { j14913k++; j14250k=&j11469k[j14913k]; j19107k=0; j17166k=0; for( unsigned int j17168k = 0; j17168k < j17696k[j16856k].size(); j17168k++ ) { j14133k=&j17696k[j16856k][j17168k]; j11938k=&j15146k[j16856k][j17168k]; j17565k=&j12192k[0][j19452k][*j14133k]; j11476k=&j16390k[*j14133k]; j14850k[j17168k]=(*j11938k); j19107k+=(((*j11476k)-((*j14250k)*j14850k[j17168k]))*j14850k[j17168k]); j17166k+=pow(j14850k[j17168k],2)+(*j17049k); } j11365k=(-j19107k/j17166k); if(std::isinf(j11365k) || std::isnan(j11365k)) j11365k=0; for( unsigned int j17168k = 0; j17168k < j17696k[j16856k].size(); j17168k++ ) { j14133k=&j17696k[j16856k][j17168k]; j11938k=&j15146k[j16856k][j17168k]; j17565k=&j12192k[0][j19452k][*j14133k]; j11476k=&j16390k[*j14133k]; (*j11476k)+=(j11365k-(*j14250k))*j14850k[j17168k]; } (*j14250k)=j11365k; } } j17049k=&j19342k[1]; for( int j19452k = 0; j19452k < j12681k[1]; j19452k++ ) { for( int j16856k = 0; j16856k < j12924k; j16856k++ ) { j14913k++; j14250k=&j11469k[j14913k]; j19107k=0; j17166k=0; for( unsigned int j17168k = 0; j17168k < j17696k[j16856k].size(); j17168k++ ) { j14133k=&j17696k[j16856k][j17168k]; j11938k=&j15146k[j16856k][j17168k]; j17565k=&j12192k[1][j19452k][*j14133k]; j11476k=&j16390k[*j14133k]; j14850k[j17168k]=(*j11938k)*((1*pow((*j17565k)[0],1))+(-1*(*j11938k)*(*j14250k))); j19107k+=(((*j11476k)-((*j14250k)*j14850k[j17168k]))*j14850k[j17168k]); j17166k+=pow(j14850k[j17168k],2)+(*j17049k); } j11365k=(-j19107k/j17166k); if(std::isinf(j11365k) || std::isnan(j11365k)) j11365k=0; for( unsigned int j17168k = 0; j17168k < j17696k[j16856k].size(); j17168k++ ) { j14133k=&j17696k[j16856k][j17168k]; j11938k=&j15146k[j16856k][j17168k]; j17565k=&j12192k[1][j19452k][*j14133k]; j11476k=&j16390k[*j14133k]; (*j11476k)+=(j11365k-(*j14250k))*j14850k[j17168k]; (*j17565k)[0]+=(pow(j11365k,1)-pow(*j14250k,1))*pow(*j11938k,1); } (*j14250k)=j11365k; } } j17049k=&j19342k[2]; for( int j19452k = 0; j19452k < j12681k[2]; j19452k++ ) { for( int j16856k = 0; j16856k < j12924k; j16856k++ ) { j14913k++; j14250k=&j11469k[j14913k]; j19107k=0; j17166k=0; for( unsigned int j17168k = 0; j17168k < j17696k[j16856k].size(); j17168k++ ) { j14133k=&j17696k[j16856k][j17168k]; j11938k=&j15146k[j16856k][j17168k]; j17565k=&j12192k[2][j19452k][*j14133k]; j11476k=&j16390k[*j14133k]; j14850k[j17168k]=(*j11938k)*((0.5*pow((*j17565k)[0],2))+((-0.5*pow((*j17565k)[1],1))+(-1*(*j11938k)*(*j14250k)*(*j17565k)[0])+(1*pow((*j11938k)*(*j14250k),2)))); j19107k+=(((*j11476k)-((*j14250k)*j14850k[j17168k]))*j14850k[j17168k]); j17166k+=pow(j14850k[j17168k],2)+(*j17049k); } j11365k=(-j19107k/j17166k); if(std::isinf(j11365k) || std::isnan(j11365k)) j11365k=0; for( unsigned int j17168k = 0; j17168k < j17696k[j16856k].size(); j17168k++ ) { j14133k=&j17696k[j16856k][j17168k]; j11938k=&j15146k[j16856k][j17168k]; j17565k=&j12192k[2][j19452k][*j14133k]; j11476k=&j16390k[*j14133k]; (*j11476k)+=(j11365k-(*j14250k))*j14850k[j17168k]; (*j17565k)[0]+=(pow(j11365k,1)-pow(*j14250k,1))*pow(*j11938k,1); (*j17565k)[1]+=(pow(j11365k,2)-pow(*j14250k,2))*pow(*j11938k,2); } (*j14250k)=j11365k; } } } for(unsigned int j11407k=j12681k.size()-1;j11407k>0;j11407k--) if(j12681k[j11407k]==0) j12681k.resize(j11407k); else break; List j11556k=Rcpp::List::create(Rcpp::Named("weights") = j11469k, Rcpp::Named("factors") = j12681k, Rcpp::Named("variables") = j12924k, Rcpp::Named("traincases") = j17156k, Rcpp::Named("min.target") = j15306k, Rcpp::Named("max.target") = j19648k); j11556k.attr("class") = "FMmodel"; return j11556k; }
// [[Rcpp::export]]
NumericVector predictFM(List j14924k) { NumericMatrix j17949k = as< NumericMatrix >(j14924k["mX"]); std::vector<int> j12681k = as< std::vector<int> >(j14924k["factors"]); std::vector<double> j11469k = as< std::vector<double> >(j14924k["weights"]); bool j18933k=(j14924k["truncate"]); double j15306k=(j14924k["min.target"]); double j19648k=(j14924k["max.target"]); double j12924k=(j14924k["variables"]); std::vector<double>* j17565k; int j16079k=j12681k.size(); int *j14133k; double* j11938k; double* j14250k; int j14913k; int j17156k=0; for(int j17168k = 0; j17168k < j17949k.nrow(); j17168k++ ){ if(j17156k<j17949k(j17168k,0)) j17156k=j17949k(j17168k,0); } j17156k++; std::vector< std::vector<int> >j17696k(j12924k); std::vector< std::vector<double> >j15146k(j12924k); for(int j17168k = 0; j17168k < j17949k.nrow(); j17168k++ ){ j17696k[j17949k(j17168k,1)].push_back(j17949k(j17168k,0)); j15146k[j17949k(j17168k,1)].push_back(j17949k(j17168k,2)); } std::vector< std::vector< std::vector< std::vector<double> > > > j12192k; j12192k.resize(j16079k); for(int j11407k = 0; j11407k < j16079k; j11407k++ ) { j12192k[j11407k].resize(j12681k[j11407k]); for(int j17883k = 0; j17883k < j12681k[j11407k]; j17883k++ ) { j12192k[j11407k][j17883k].resize(j17156k); for(int j10054k = 0; j10054k < j17156k; j10054k++ ){ j12192k[j11407k][j17883k][j10054k].resize(j11407k+1); for(int j19554k = 0; j19554k < j11407k+1; j19554k++ ) j12192k[j11407k][j17883k][j10054k][j19554k]=0; } } } for(unsigned int j11407k = 0; j11407k < j12192k.size(); j11407k++ ) { for(unsigned int j17883k = 0; j17883k < j12192k[j11407k].size(); j17883k++ ) { for(unsigned int j10054k = 0; j10054k < j12192k[j11407k][j17883k].size(); j10054k++ ){ for(unsigned int j19554k = 0; j19554k < j12192k[j11407k][j17883k][j10054k].size(); j19554k++ ) j12192k[j11407k][j17883k][j10054k][j19554k]=0; } } } j14913k=0; for(int j12046k = 0; j12046k < j16079k; j12046k++ ) { for( int j19452k = 0; j19452k < j12681k[j12046k]; j19452k++ ) { for( int j16856k = 0; j16856k < j12924k; j16856k++ ) { j14913k++; j14250k=&j11469k[j14913k]; for( unsigned int j17168k = 0; j17168k < j17696k[j16856k].size(); j17168k++ ) { j14133k=&j17696k[j16856k][j17168k]; j11938k=&j15146k[j16856k][j17168k]; j17565k=&j12192k[j12046k][j19452k][*j14133k]; for( unsigned int j19543k = 1; j19543k <= (*j17565k).size(); j19543k++ ) { (*j17565k)[j19543k-1]+=pow(*j11938k,j19543k)*pow(*j14250k,j19543k); } } } } } NumericVector j15571k(j17156k,j11469k[0]); std::vector< std::vector<double> >* j19954k; for(int j12046k = 0; j12046k < j16079k; j12046k++ ) { for( int j19452k = 0; j19452k < j12681k[j12046k]; j19452k++ ) { j19954k=&j12192k[j12046k][j19452k]; if(j12046k==0) for(unsigned int j17168k = 0; j17168k < (*j19954k).size(); j17168k++ ) j15571k[j17168k]+=(*j19954k)[j17168k][0]; if(j12046k==1) for(unsigned int j17168k = 0; j17168k < (*j19954k).size(); j17168k++ ) j15571k[j17168k]+=(1.0/2.0)*((1*pow((*j19954k)[j17168k][0],2))+(-1*pow((*j19954k)[j17168k][1],1))); if(j12046k==2) for(unsigned int j17168k = 0; j17168k < (*j19954k).size(); j17168k++ ) j15571k[j17168k]+=(1.0/6.0)*((1*pow((*j19954k)[j17168k][0],3))+(-3*pow((*j19954k)[j17168k][1],1)*pow((*j19954k)[j17168k][0],1))+(2*pow((*j19954k)[j17168k][2],1))); } } if(j18933k){ for(int j17168k=0;j17168k<j17156k;j17168k++) { if(j15571k[j17168k]>j19648k) j15571k[j17168k]=j19648k; if(j15571k[j17168k]<j15306k) j15571k[j17168k]=j15306k; } } return (j15571k); }
