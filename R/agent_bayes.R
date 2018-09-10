### functions concerning the Bayesian belief updating

prior_uniform_ship_sizes <- function(testing = FALSE) {
  ## it seems impossible to have a prior that is uniform over ship sizes because
  ## of the interdependence of ship size and frequency across boards

  ## best we can achieve is a prior where the ship size frequency unbalance is
  ## attenuated (i.e., moving the flat prior as far as possible towards uniform
  ## over ship sizes)

  ## frequency unbalance
  boards %>%
    group_by(size) %>%
    summarize(freq = n()) %>%
    ungroup %>%
    mutate(rel_freq = normalize(freq) %>% round(2))

  ## mini example
  # (cc <- boards[1:100,])
  # (cc <- data_frame(id = 1:3, A = c(1, 1, 2), B = c(1, 2, 1)) %>% gather(ship, size, A:B) %>% arrange(id))
  # (dd <- cc  %>%
  #   group_by(ship, size) %>%
  #   mutate(flat = 1,
  #          x = 1/ n()) %>%
  #   ungroup %>%
  #   mutate(flat = normalize(flat),
  #          x = normalize(x)))
  # (ee <- dd %>%
  #   group_by(id) %>%
  #   mutate(flat = sum(flat),
  #             x = sum(x)) %>%
  #   ungroup %>%
  #     mutate(flat = normalize(flat),
  #            x = normalize(x)))
  # (ff <- ee %>%
  #   group_by(size) %>%
  #   summarize(flat = sum(flat),
  #             x = sum(x)) %>%
  #   ungroup %>%
  #   mutate(flat = normalize(flat),
  #          x = normalize(x)))

  if (testing) boards <- boards[1:1002,]

  ##  compute prior
  dd <- boards %>%
      group_by(ship, size) %>%
      mutate(flat = 1,
             x = 1/ n()) %>%
      ungroup %>%
      mutate(flat = normalize(flat),
             x = normalize(x)) %>%
      group_by(id) %>%
      mutate(flat = sum(flat),
             x = sum(x))

  # dd$flat %>% table
  # dd$x %>% table

  # dd %>%
  #   group_by(size) %>%
  #   summarize(flat = sum(flat),
  #             x = sum(x)) %>%
  #   ungroup %>%
  #   mutate(flat = normalize(flat),
  #          x = normalize(x))

  boards2 <- dd %>%
    group_by(id) %>%
    summarize(flat = sum(flat),
              x = sum(x)) %>%
    ungroup %>%
    mutate(flat = normalize(flat),
           x = normalize(x)) %>%
    rename(uniform_prior = flat, uniform_shipsizes_prior = x)

  # devtools::use_data(boards2, overwrite = TRUE, compress = 'gzip')

  boards2
}
