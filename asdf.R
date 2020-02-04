library(tidyverse)
Rcpp::sourceCpp(here::here("src","popmodel.cpp"))

theme_pres = theme_minimal(base_size = 22)


linf = 100

t0 = -.1

wa = 1e-4

wb = 3

k = 0.2

m = 1.5 * k

lmat = 0.66 * linf

ages = 1:150

sel_50 <- 66

sel_delta <- 4

length_at_age <- linf * (1 - exp(-k * (ages - t0)))

sel_at_age <- 1 / (1 + exp(-log(19) * (length_at_age -sel_50) / sel_delta))

weight_at_age <- wa * length_at_age ^ wb

maturity_at_age <- 1 / (1 + exp(-log(19) * (length_at_age -lmat) / 4))

years <- 500

n_ages = length(ages) - 1

msyfoo <- function(f,steepness, use = 1){
  

  blah = popmodel(length_at_age = length_at_age,
                  weight_at_age = weight_at_age,
                  maturity_at_age = as.numeric(maturity_at_age),
                  selectivity_at_age = sel_at_age,
                  rec_devs = rep(1,years),
                  m = m,
                  n_ages = n_ages,
                  age_vector = (1:n_ages) - 1,
                  sim_years = years - 100,
                  burn_years = 100,
                  rec_form = 1,
                  steepness = steepness,
                  r0 = 100,
                  f = f)
  
  ssbt <-  last(rowSums(blah$ssb_t))
  
  rec_t = last(blah$rec_t)
  
  c_t = last(rowSums(blah$c_t))
  
  spr = ssbt / blah$ssb0
  
  n = last(rowSums(blah$b_t))
  
  n0 = first(rowSums(blah$b_t))
  
  dep = n / n0
  
  out = tibble(ssb = ssbt, rec = rec_t, catch = c_t, spr = spr, n = n, n0 = n0, dep = dep)

if (use == 1){
    out = -c_t
} 
  
  return(out)
  
}

msyfoo(1,.6)

hs <- seq(0.2,1, length.out = 10)
x = NA
o = list()
for (h in 1:length(hs)){

x[h] <- nlminb(0.001, msyfoo, steepness = hs[h])$par
 

o[[h]] = msyfoo(x[h], hs[h], use = 0)
 
}

plot(hs,x)

wtf <- tibble(h = hs, o = o) %>% 
  unnest(cols =o)

wtf %>% 
  ggplot(aes(h,dep)) + 
  geom_point()
