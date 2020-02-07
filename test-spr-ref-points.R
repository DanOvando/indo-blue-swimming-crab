library(tidyverse)
library(ggridges)
Rcpp::sourceCpp(here::here("src","popmodel.cpp"))

theme_pres = theme_minimal(base_size = 22)



msyfoo <- function(log_f,steepness = 0.6, m_k = 1.5, k  = 0.133, lmat = 0.66, lsel = 0.5,use = 1){
  
  
  
  f = exp(log_f)
  
  linf = 100
  
  t0 = -.1
  
  wa = 1e-4
  
  wb = 3
  
  k = k
  
  m = m_k * k
  
  lmat = lmat * linf
  
  ages = 1:50
  
  sel_50 <- lsel * linf
  
  sel_delta <- 2
  
  length_at_age <- linf * (1 - exp(-k * (ages - t0)))
  
  sel_at_age <- 1 / (1 + exp(-log(19) * (length_at_age -sel_50) / sel_delta))
  
  weight_at_age <- wa * length_at_age ^ wb
  
  maturity_at_age <- 1 / (1 + exp(-log(19) * (length_at_age -lmat) / 4))
  
  years <- 200
  
  n_ages = length(ages) - 1

  blah = popmodel(length_at_age = length_at_age,
                  weight_at_age = weight_at_age,
                  maturity_at_age = as.numeric(maturity_at_age),
                  selectivity_at_age = sel_at_age,
                  rec_devs = rep(1,years),
                  m = m,
                  n_ages = n_ages,
                  age_vector = (1:n_ages) - 1,
                  sim_years = years,
                  burn_years = 0,
                  rec_form = 1,
                  steepness = steepness,
                  r0 = 100,
                  f = f)
  
  # blah$n_t %>% 
  #   as_tibble() %>% 
  #   mutate(year = 1:nrow(.)) %>% 
  #   filter(year %% 10 == 0) %>% 
  #   pivot_longer(-year, names_to = "age", values_to = "numbers",
  #              names_prefix = "V",
  #              names_ptypes = list(age = integer())) %>% 
  #   filter(age < 20) %>% 
  #   ggplot(aes(age, factor(year), height = numbers)) + 
  #   geom_density_ridges(stat = "identity", alpha = 0.75)
  # 
  # blah$b_t %>% 
  #   as_tibble() %>% 
  #   mutate(year = 1:nrow(.)) %>% 
  #   filter(year %% 10 == 0) %>% 
  #   pivot_longer(-year, names_to = "age", values_to = "biomass",
  #                names_prefix = "V",
  #                names_ptypes = list(age = integer())) %>% 
  #   filter(age < 20) %>% 
  #   ggplot(aes(age, factor(year), height = biomass)) + 
  #   geom_density_ridges(stat = "identity", alpha = 0.75)
  # 
  # blah$ssb_t %>% 
  #   as_tibble() %>% 
  #   mutate(year = 1:nrow(.)) %>% 
  #   filter(year %% 10 == 0) %>% 
  #   pivot_longer(-year, names_to = "age", values_to = "ssb",
  #                names_prefix = "V",
  #                names_ptypes = list(age = integer())) %>% 
  #   filter(age < 20) %>% 
  #   ggplot(aes(age, factor(year), height = ssb)) + 
  #   geom_density_ridges(stat = "identity", alpha = 0.75)
  # 
  # blah$ssb_t %>% rowSums() %>% plot()
  
  ssbt <-  last(rowSums(blah$ssb_t))
  
  rec_t = last(blah$rec_t)
  
  c_t = last(rowSums(blah$c_t))
  
  spr = blah$spr
  
  n = last(rowSums(blah$b_t))
  
  n0 = first(rowSums(blah$b_t))
  
  dep = n / n0
  
  out = tibble(ssb = ssbt, rec = rec_t, catch = c_t, spr = spr, n = n, n0 = n0, dep = dep)

if (use == 1){
    out = -c_t
} 
  
  return(out)
  
}


hs <- seq(0.3,.9, by = .1)


wtf <- tidyr::expand_grid(h = hs, m_k = seq(0.1, 2, by = .25), 
                          k = seq(0.05, 1.5, by = .25),
                          lmat = seq(.1, .8, by = .25),
                          lsel = seq(.1, .8, by = .25))

wtf <- wtf %>%
  # sample_n(10) %>%
  mutate(fmsy = pmap_dbl(list(
    steepness = h,
    m_k = m_k,
    lmat = lmat,
    lsel
  ), ~ exp(
    nlminb(
      log(0.001),
      msyfoo,
      steepness = ..1,
      m_k = ..2,
      lmat = ..3,
      lsel = ..4
    )$par
  ))) %>%
  mutate(msy_pop = pmap(
    list(
      steepness = h,
      m_k = m_k,
      lmat = lmat,
      lsel,
      log_f = log(fmsy)
    ),
    ~  msyfoo(
      steepness = ..1,
      m_k = ..2,
      lmat = ..3,
      lsel = ..4,
      log_f = ..5,
      use = 0
    )
  ))

wtf <- wtf %>% 
  unnest(cols =msy_pop)

wtf %>% 
  ggplot(aes(dep, y = h, group  = h)) + 
  geom_density_ridges() + 
  labs(x = "Bmsy/B0", y = "Steepness")

wtf %>% 
  ggplot(aes(spr, y = h, group  = h)) + 
  geom_density_ridges() + 
  labs(x = "SPR at MSY", y = "Steepness") + 
  theme_pres


wtf %>% 
  ggplot(aes(m_k, spr, group = m_k)) + 
  geom_violin() + 
  facet_wrap(~h)
  
