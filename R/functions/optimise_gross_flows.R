# fit in- and out-flows consistent with a target net value and typical gross flows

optimise_gross_flows_non_vec <- function(base_in, base_out, target_net, jump_scale = 4) {

  # lower values of jump scale should be faster, but potentially less reliable.
  # Always use a value greater than 1

  base_out <- abs(base_out)
  base_in <- abs(base_in)
  base_net <- round(base_in - base_out, 0)
  target_net = round(target_net, 0)
  change_net = target_net - base_net

  max_iterations <- abs(change_net)

  new_in <- round(base_in, 0)
  new_out <- round(base_out, 0)

  j <- 1
  while((floor(abs(target_net - (new_in - new_out))) > 0) & (j < max_iterations)){

    distance_from_target <- target_net - (new_in - new_out)
    direction_to_target <- distance_from_target/abs(distance_from_target)
    int_adjust <- direction_to_target * ceiling(abs(distance_from_target/jump_scale))

    p_in_adjust <- dpois(new_in + int_adjust, base_in, log = TRUE) + dpois(new_out, base_out, log = TRUE)
    p_out_adjust <- dpois(new_in, base_in, log = TRUE) + dpois(new_out - int_adjust, base_out, log = TRUE)

    if(p_in_adjust > p_out_adjust) {
      new_in <- new_in + int_adjust
    } else {
      new_out <- new_out - int_adjust
    }

    j <- j + 1
  }

  c_out <- list(c("inflow" = new_in, "outflow" = new_out))

  return(c_out)
}

optimise_gross_flows = Vectorize(optimise_gross_flows_non_vec, SIMPLIFY = TRUE)
