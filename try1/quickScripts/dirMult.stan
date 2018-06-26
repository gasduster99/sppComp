data{
        int<lower=0> N;
	int<lower=0> MN;
	int<lower=0> P;
	int<lower=0> spp[N*P];
	int<lower=0> nSpp[N, P];
	vector[P] as;
        //matrix[N,P] dat;
	//int dat[N,P];
}

parameters{
	simplex[P] theta;
	simplex[P] alpha;
}

transformed parameters{
	vector[N] log_lik;
	for (n in 1:N){
    		log_lik[n] = multinomial_lpmf(nSpp[n] | theta);
  	}
}

model{
	//vector[P] as;
	//for(j in 1:P){
	//	as[j] = 1;
	//}
	alpha ~ dirichlet(as);
	theta ~ dirichlet(alpha);
	for(i in 1:N){
		nSpp[i] ~ multinomial(theta);
	}

}
