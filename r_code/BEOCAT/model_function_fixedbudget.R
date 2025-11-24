#Load in the program that determines the action-based payment to equalize total program cost with results-based program
#source("act_based_payment_finder.R")

toy_model_run <- function(run_num, jur_size, e_nr_sd, e_c0_sd, #Function arguments determining jurisdiction size and number - also determines population size
                          cc_nr_mean, cc_nr_sd, #Function arguments determining net returns for cover-cropping
                          c_outc_distr_choice, unif_outc_lb, unif_outc_ub, norm_outc_mean, norm_outc_sd, #Function arguments determining sequestration potential
                          e_nr_mean, e_c0_mean, #Function arguments determining policy-maker's ability to estimate net returns and carbon stocks
                          ppco2_payment, theta, #Function arguments determining results-based compensation
                          use_unbiased_bl, use_reverse_auction, ...) { #value indicating whether to use the unbiased jurisdictional baselines


  #--------------- Initialize model -----------------------#
  #Generate dataframe with observation id's
  #CHANGE BACK TO 10,000
  num_jur = 10000/jur_size
  init_df <- data.frame(matrix(ncol=1,nrow=jur_size*num_jur,
                               dimnames=list(NULL, c("obs_id"))))
  init_df <- init_df %>% dplyr::transmute(obs_id = 1:nrow(init_df))

  #Generate jurisdiction id's
  jurs_id_list <- list(seq(1:num_jur))
  jurs_df <- data.frame(matrix(ncol=1,nrow=0, dimnames=list(NULL, c("jurs_id"))))
  for (i in seq(1:num_jur)) {
    jurs_id <- c(rep(i, jur_size))
    jurs_id <- as.data.frame(jurs_id)
    jurs_df <- rbind(jurs_df, jurs_id)
  }
  #Add to init df
  init_df <- cbind(init_df, jurs_df)

  #Producer Variables:
  #Create net return variables for the practice, as well as baseline behavior
  init_df <- init_df %>% dplyr::mutate(nr = rnorm(num_jur*jur_size, mean = cc_nr_mean, sd = cc_nr_sd ),
                                       bl = 1*(nr > 0))
  #Create baseline outcome
  if (c_outc_distr_choice==1) {
    init_df <- init_df %>% dplyr::mutate(pot_outc = runif(num_jur*jur_size, min = unif_outc_lb, max = unif_outc_ub),
                                         bl_outc = pot_outc*bl)
  }
  if (c_outc_distr_choice==2) {
    init_df <- init_df %>% dplyr::mutate(pot_outc = rnorm(num_jur*jur_size, mean = norm_outc_mean, sd = norm_outc_sd),
                                         bl_outc = pot_outc*bl)
  }

  #Now sum the baseline outcomes for all producers in each jurisdiction to get the true jurisdictional baseline
  init_df <- init_df %>% dplyr::group_by(jurs_id) %>% summarise(jur_bl_outc = sum(bl_outc)) %>%
    dplyr::left_join(init_df, by = "jurs_id")

  #Policy Maker Variables:
  #Create policy makers' estimates of the net returns to each practice and their estimates of baseline behavior
  init_df <- init_df %>% dplyr::mutate(nr_hat = nr + rnorm(num_jur*jur_size, mean = e_nr_mean, sd = e_nr_sd ),
                                       bl_hat = 1*(nr_hat > 0))

  #Create Policy Maker's estimates of the outcomes they predict due to each practice's baseline use
  init_df <- init_df %>% dplyr::mutate(pot_outc_hat = pot_outc + rnorm(num_jur*jur_size, mean = e_c0_mean, sd = e_c0_sd ),
                                       bl_outc_hat = pot_outc_hat*bl_hat)

  #Now sum the estimated outcomes for all producers in each jurisdiction to get the jurisdictional estimates
  init_df <- init_df %>% dplyr::group_by(jurs_id) %>% summarise(jur_bl_outc_hat = sum(bl_outc_hat)) %>%
    dplyr::left_join(init_df, by = "jurs_id")

  #Create real and estimated jurisdiction level baseline practice use
  init_df <- init_df %>% dplyr::group_by(jurs_id) %>%
    summarise(jur_bl_hat = sum(bl_hat),
              jur_bl = sum(bl)) %>%
    dplyr::left_join(init_df, by = "jurs_id")

  #---- Find action-based program incentive which equats total cost -----#
  crt_val <- uniroot(ppp_payment_finder, c(0.5, 1.5),
                     input_df=init_df,
                     tol=0.001, maxiter=10000, extendInt="yes")
  ppp_payment <- crt_val$root

  #--------------- Define programs and incentives -----------------------#
  ##################################
  ### Payment-for-practice (PPP) ###
  ##################################
  # # # PPP Individual level variables # # #
  init_df <- init_df %>% dplyr::mutate( #First create variables showing all producers net returns with the ppp payment, regardless of enrollment eligibility
    pot_nr_act = ppp_payment + nr,
    #Now create indicator for whether they would participate, and for each type of producer
    pot_part_act = 1*(pot_nr_act > 0 ),  #Indicator of whether individual would enroll in cc with ppp
    pot_add_part_act = 1*(pot_part_act==1 & nr<0), #Additional producer
    pot_typ2_part_act = 1*(pot_part_act==1 & nr>0), #Indicator of whether individual enrolling in cover-crop would NOT be additional
    #Show the outcome that is estimated, and that would truly occur, when action-based program is available
    pot_est_outc_act = (pot_nr_act > 0)*pot_outc_hat,
    pot_true_outc_act = (pot_nr_act > 0)*pot_outc,
    #Real and estimated individual CO2 sequestration that is spurious because the producer would have adopted anyways
    pot_est_outc_typ2_act = pot_est_outc_act*pot_typ2_part_act,
    pot_true_outc_typ2_act = pot_true_outc_act*pot_typ2_part_act,
    #Individual cost of adoption (meaning individual's net return to adoption is negative without the policy)
    cost_part_act = nr*(nr<0 & pot_nr_act>0),
    #Create variable showing the potential efficiency gain for each individual if they efficiently adopt
    ind_pot_effgain_part_act = (ppp_payment+nr)*(nr<0 & pot_nr_act>0))

  # # PPP Jurisdictional level variables # #
  init_df <- init_df %>% dplyr::group_by(jurs_id) %>% summarise(#Now create variables showing the total number of producers who want to enroll within each jurisdiction, regardless of their individual baselines
    jur_total_pot_part_act = sum(pot_part_act),
    jur_total_pot_typ2_part_act = sum(pot_typ2_part_act),
    #Aggregate carbon outcomes if ppp program is available (true and estimated) as well as the amount that is spurious
    jur_pot_true_outc_act = sum(pot_true_outc_act),
    jur_pot_est_outc_act = sum(pot_est_outc_act),
    jur_pot_est_outc_typ2_act = sum(pot_est_outc_typ2_act),
    jur_pot_true_outc_typ2_act = sum(pot_true_outc_typ2_act),
    #Sum individual costs at the jurisdiction level for producers who would efficiently enroll
    jur_total_cost_act = sum(cost_part_act),
    #Sum up potential efficiency gains for jurisdictions
    jur_pot_effgain_act = sum(ind_pot_effgain_part_act)) %>%
    dplyr::left_join(init_df, by = "jurs_id")

  # PPP Jurisdictional Outcomes #
  init_df <- init_df %>% dplyr::mutate(#Create indicator showing if jurisdiction opts in - They will opt in if the return to enrollment (number enrolled above baseline times ppp payment) is
    #Greater than the total costs of adoption (sum of negative returns for efficiently enrolled individuals)
    jur_part_act = 1*(ppp_payment*(jur_total_pot_part_act - jur_bl_hat) + jur_total_cost_act > 0),
    #Create indicator showing whether jurisdictions are efficiently participating
    jur_eff_part_act = jur_part_act*(ppp_payment*(jur_total_pot_part_act - jur_bl) + jur_total_cost_act > 0),
    #Create indicator showing if jurisdiction is efficiently NOT participating
    jur_eff_nonpart_act = (1-jur_part_act)*(ppp_payment*(jur_total_pot_part_act - jur_bl) + jur_total_cost_act <= 0),
    #Create indicator showing if jurisdictions were inefficiently excluded because of a
    #estimated baseline that is too high
    jur_ineff_excl_act = 1*(ppp_payment*(jur_total_pot_part_act - jur_bl) + jur_total_cost_act > 0)*(ppp_payment*(jur_total_pot_part_act - jur_bl_hat) + jur_total_cost_act < 0),
    #Create indicator if jurisdiction was inefficiently allowed to participate
    jur_ineff_incl_act = jur_part_act*(ppp_payment*(jur_total_pot_part_act - jur_bl) + jur_total_cost_act <= 0),
    #Create total number of producers enrolled within each jurisdiction if the jurisdiction opted in
    jur_total_part_act =  jur_total_pot_part_act*(jur_part_act==1),
    #Create the within-jurisdiction total expense of enrolling all of the potential producers
    jur_total_exp_act =  ppp_payment*jur_total_pot_part_act*(jur_part_act==1),
    #Now create total number of producers within each jurisdiction who ARE REALLY ADDITIONAL w.r.t. true baseline
    jur_total_eff_adopt_act = (jur_total_part_act - jur_bl)*(jur_part_act==1),
    #Create total number of adoptions based on ASSUMED additional adoption w.r.t estimated jurisdictional baseline
    jur_total_est_adopt_act = (jur_total_part_act - jur_bl_hat)*(jur_part_act==1),
    #Create number of enrolled jurisdictions' adoptions that are spurious
    jur_total_typ2_part_act = (jur_total_pot_typ2_part_act)*(jur_part_act==1),
    #Create jurisdiction level compensation
    jur_total_comp_act =  ppp_payment*(jur_total_pot_part_act - jur_bl_hat)*(jur_part_act==1),
    #Create total number of carbon credits issued based on estimated carbon sequestration due to practice use above baseline
    jur_total_credits_act = (jur_pot_est_outc_act - jur_bl_outc_hat)*(jur_part_act==1),
    #Create number of jurisdictions' credits that are spurious
    jur_spur_credits_act = (jur_bl_outc - jur_bl_outc_hat)*(jur_bl_outc > jur_bl_outc_hat)*(jur_part_act==1),
    #Create total amount of additional carbon sequestered based on producers behavior when the program is available w.r.t the true baseline
    jur_total_add_outc_act = (jur_pot_true_outc_act - jur_bl_outc)*(jur_part_act==1),
    #Create jurisdiction level efficiency gain
    jur_total_effgain_act = jur_pot_effgain_act*(jur_part_act==1),
    #Jurisdictions total sequestration occurring, regardless of enrollment
    jur_total_c_act = jur_bl_outc + jur_total_add_outc_act)

  ###############################################
  # # # Payment-per-unit-sequesterd (PPCO2) # # #
  ###############################################
  # # PPCO2 Individual level variables # #
  init_df <- init_df %>% dplyr::mutate( #First create variables showing all producers net returns with the ppco2 payment, regardless of enrollment eligibility
    pot_nr_res = ppco2_payment*pot_outc_hat + nr,
    pot_nr_zeroerror_res = ppco2_payment*pot_outc + nr,
    #Now create indicators for if each producer would adopt in jurisdictional ppco2 program and if they would be additional (nr<0 in baseline)
    pot_part_jur_res = 1*(pot_nr_res > 0 & pot_nr_res > nr),  #Indicator of whether individual would enroll in action-baed program
    pot_part_jur_zeroerror_res = 1*(pot_nr_zeroerror_res > 0 & pot_nr_zeroerror_res > nr),
    pot_typ2_part_jur_res = 1*(pot_part_jur_res==1 & nr>0), #Indicator of whether individual enrolling would NOT be additional
    pot_overest_part_jur_res = 1*(pot_part_jur_res==1 & ppco2_payment*pot_outc + nr <= 0),
    #Carbon outcome individually (estimated and real) if ppco2 program is available
    pot_true_outc_res = ifelse(nr > 0, pot_outc, (pot_nr_res > 0)*pot_outc),
    pot_est_outc_res = ifelse(nr > 0, pot_outc_hat, (pot_nr_res > 0)*pot_outc_hat),
    pot_true_outc_zeroerror_res = ifelse(nr > 0, pot_outc, (pot_nr_zeroerror_res > 0)*pot_outc),
    #Estimated individual CO2 sequestration by potential participants if their jurisdiction enrolls
    pot_est_outc_part_res = pot_est_outc_res*pot_part_jur_res,
    pot_est_outc_zeroerror_part_res = pot_true_outc_zeroerror_res*pot_part_jur_zeroerror_res,
    #Estimated and real individual CO2 sequestration that would be spurious because the producer would have adopted anyways
    pot_est_outc_typ2_res = pot_est_outc_res*pot_typ2_part_jur_res,
    pot_true_outc_typ2_res = pot_true_outc_res*pot_typ2_part_jur_res,
    #Real and estimated outcomes for participants who are only participating because of overestimated outcomes
    pot_true_outc_overest_res = pot_true_outc_res*pot_overest_part_jur_res,
    pot_est_outc_overest_res = pot_est_outc_res*pot_overest_part_jur_res,
    #Individual cost of adoption for ppco2 (meaning individual's net return to adoption is negative without the policy)
    cost_part_res = nr*(nr<0 & pot_nr_res>0), #First create cost at individual level, it's the amount which would make someone with a negative return to adoption indifferent between adopting and not adopting
    cost_part_zeroerror_res = nr*(nr<0 & pot_nr_zeroerror_res>0),
    #Create variable showing the potential efficiency gain for each individual if they efficiently adopt
    ind_pot_effgain_part_res = (ppco2_payment*pot_outc + nr)*(nr<0 & pot_nr_res>0))


  # # PPCO2 Jurisdictional level variables # #
  init_df <- init_df %>% dplyr::group_by(jurs_id) %>% summarise(#Aggregate carbon outcome if ppco2 program is available, as well as the amount that is spurious
    jur_pot_true_outc_res = sum(pot_true_outc_res),
    jur_pot_est_outc_res = sum(pot_est_outc_res),
    jur_pot_zeroerror_outc_res = sum(pot_true_outc_zeroerror_res),
    jur_pot_true_outc_typ2_res = sum(pot_true_outc_typ2_res),
    jur_pot_est_outc_typ2_res = sum(pot_est_outc_typ2_res),
    #Overestimated values
    jur_pot_true_outc_overest_res = sum(pot_true_outc_overest_res),
    jur_pot_est_outc_overest_res = sum(pot_est_outc_overest_res),
    #Now create variables showing the total number of producers who want to enroll within each jurisdiction, regardless of their individual baselines
    jur_total_pot_part_res = sum(pot_part_jur_res),
    jur_total_pot_typ2_part_res = sum(pot_typ2_part_jur_res),
    jur_total_pot_overest_part_res = sum(pot_overest_part_jur_res),
    #Then, sum individual costs at the jurisdiction level for producers who would efficiently enroll
    jur_total_cost_res = sum(cost_part_res),
    jur_total_cost_zeroerror_res = sum(cost_part_zeroerror_res),
    #Total up the estimated carbon sequestration of all producers who would participate if the jurisdiction enrolls
    jur_total_pot_est_outc_part_res = sum(pot_est_outc_part_res),
    #Sum up potential efficiency gains for jurisdictions
    jur_pot_effgain_res = sum(ind_pot_effgain_part_res, na.rm = TRUE)) %>%
    dplyr::left_join(init_df, by = "jurs_id")

  # PPCO2 Jurisdictional outcomes #
  init_df <- init_df %>% dplyr::mutate(#Create indicator showing if jurisdiction opts in for cc and nt
    jur_part_res = 1*(ppco2_payment*(jur_pot_est_outc_res - jur_bl_outc_hat) + jur_total_cost_res > 0),
    #Create indicators for whether the jurisdiction efficiently participated, or efficiently did not participate
    jur_eff_part = jur_part_res*(ppco2_payment*(jur_pot_true_outc_res - jur_bl_outc) + jur_total_cost_res > 0),
    #Create total number of producers enrolled within each jurisdiction if the jurisdiction opted in
    jur_total_part_res = jur_total_pot_part_res*(jur_part_res==1),
    #Create indicator showing if they are efficiently participating
    jur_eff_part_res = 1*jur_part_res*(ppco2_payment*(jur_pot_zeroerror_outc_res - jur_bl_outc) + jur_total_cost_zeroerror_res > 0),
    #Create indicator showing if they are efficiently NOT participating
    jur_eff_nonpart_res = (jur_part_res==0)*(ppco2_payment*(jur_pot_zeroerror_outc_res - jur_bl_outc) + jur_total_cost_zeroerror_res <= 0),
    #Create indicator showing if they were inefficiently excluded because of a
    #estimated baseline that is too high or underestimated outcomes
    jur_ineff_excl_res = 1*(jur_part_res==0)*(ppco2_payment*(jur_pot_zeroerror_outc_res - jur_bl_outc) + jur_total_cost_zeroerror_res > 0),
    #Create indicator if jurisdiction was inefficiently allowed to participate
    jur_ineff_incl_res = (jur_part_res==1)*(ppco2_payment*(jur_pot_zeroerror_outc_res - jur_bl_outc) + jur_total_cost_zeroerror_res <= 0),
    #Create the within-jurisdiction total expense of enrolling all of the potential producers
    jur_total_exp_res = ppco2_payment*jur_total_pot_est_outc_part_res*(jur_part_res==1),
    #Now create total number of producers within each jurisdiction who ARE REALLY ADDITIONAL w.r.t. true baseline
    jur_total_eff_adopt_res = (jur_total_part_res - jur_bl)*(jur_part_res==1),
    #Create total number of adoptions based on ASSUMED additional adoption w.r.t estimated jurisdictional baseline
    jur_total_est_adopt_res = (jur_total_part_res - jur_bl_hat)*(jur_part_res==1),
    #Create number of enrolled jurisdictions' adoptions that are spurious
    jur_total_typ2_part_res = jur_total_pot_typ2_part_res*(jur_part_res==1),
    #Create number of enrolled jurisdictions' adoptions that are due to overestimated outcomes
    jur_total_overest_part_res = jur_total_pot_overest_part_res*(jur_part_res==1),
    #Create total number of carbon credits issued based on ASSUMED additional adoption w.r.t estimated jurisdictional baseline
    jur_total_credits_res = (jur_pot_est_outc_res - jur_bl_outc_hat)*(jur_part_res==1),
    #Create jurisdiction level compensation
    jur_total_comp_res =  ppco2_payment*(jur_pot_est_outc_res - jur_bl_outc_hat)*(jur_part_res==1),
    #Create total amount of ADDITIONAL carbon sequestered based on producers behavior when the program is available w.r.t the true baseline
    jur_total_add_outc_res = (jur_pot_true_outc_res - jur_bl_outc)*(jur_part_res==1),
    #Create number of jurisdictions' credits that are spurious
    jur_typ2_spur_credits_res = jur_pot_est_outc_typ2_res*(jur_part_res==1),
    jur_spur_credits_res = (jur_bl_outc - jur_bl_outc_hat)*(jur_bl_outc > jur_bl_outc_hat)*(jur_part_res==1),
    #Create jurisdiction level efficiency gain
    jur_total_effgain_res = jur_pot_effgain_res*(jur_part_res==1),
    #Jurisdictions total sequestration occurring from each practice, regardless of enrollment
    jur_total_c_res = jur_bl_outc + jur_total_add_outc_res,
    #Carbon from non-additional participants
    jur_total_c_typ2_res = jur_pot_true_outc_typ2_res*(jur_part_res==1),
    #Carbon from overestimated participants
    jur_total_c_overest_res = jur_pot_true_outc_overest_res*(jur_part_res==1)
  )

  #--------------- Output jurisdictional outcomes -----------------------#
  jur_output_df <- init_df %>% dplyr::select(starts_with("jur")) %>% distinct()
  #Create output values
  #Total jurisdictions enrolled
  total_jur_enrolled_act <- sum(jur_output_df$jur_part_act)
  total_jur_enrolled_res <- sum(jur_output_df$jur_part_res)
  #Create total plots enrolled
  total_plots_enrolled_act <- sum(jur_output_df$jur_total_part_act)
  total_plots_enrolled_res <- sum(jur_output_df$jur_total_part_res)
  #Total number of jurisdictions who are efficiently included
  total_jur_eff_incl_act = sum(jur_output_df$jur_eff_part_act)
  total_jur_eff_incl_res = sum(jur_output_df$jur_eff_part_res)
  #Total number of jurisdictions who are efficiently not participating
  total_jur_eff_excl_act = sum(jur_output_df$jur_eff_nonpart_act)
  total_jur_eff_excl_res = sum(jur_output_df$jur_eff_nonpart_res)
  #Total number of jurisdictions who were inefficiently excluded
  total_jur_ineff_excl_act = sum(jur_output_df$jur_ineff_excl_act)
  total_jur_ineff_excl_res = sum(jur_output_df$jur_ineff_excl_res)
  #Total number of jurisdictions who were inefficiently allowed to participate
  total_jur_ineff_incl_act = sum(jur_output_df$jur_ineff_incl_act)
  total_jur_ineff_incl_res = sum(jur_output_df$jur_ineff_incl_res)
  #Create total carbon credits issued for sequestration
  total_credits_act <- sum(jur_output_df$jur_total_credits_act)
  total_credits_res <- sum(jur_output_df$jur_total_credits_res)
  #Create total carbon credits issued that are produced by producers who would have adopted anyways
  total_typ2_credits_act <- sum(jur_output_df$jur_spur_credits_act)
  total_typ2_credits_res <- sum(jur_output_df$jur_spur_credits_res)
  #Create total adoptions the program believes it is incentivising
  total_adopt_act <- sum(jur_output_df$jur_total_est_adopt_act)
  total_adopt_res <- sum(jur_output_df$jur_total_est_adopt_res)
  #Create total number of practice adoptions that are spurious
  total_spur_adopt_act <- sum(jur_output_df$jur_total_typ2_part_act)
  total_spur_adopt_res <- sum(jur_output_df$jur_total_typ2_part_res)
  #Create total number of adopters who only participate because they have overestimated outcomes,
  #as well as their true carbon contributions
  total_overest_adopt_res <- sum(jur_output_df$jur_total_overest_part_res)
  total_overest_true_outc_res <- sum(jur_output_df$jur_total_c_overest_res)
  #Create total number of efficiently enrolled individuals
  total_eff_enr_act <- sum(jur_output_df$jur_total_eff_adopt_act)
  total_eff_enr_res <- sum(jur_output_df$jur_total_eff_adopt_res)
  #Create total payments received for participation by practice and in total
  total_payments_rec_act <- sum(jur_output_df$jur_total_comp_act)
  total_payments_rec_res <- sum(jur_output_df$jur_total_comp_res)
  #Create total expenditures
  total_jur_exp_act <- sum(jur_output_df$jur_total_exp_act)
  total_jur_exp_res <- sum(jur_output_df$jur_total_exp_res)
  #Create total efficiency
  total_eff_gain_act <- sum(jur_output_df$jur_total_effgain_act)
  total_eff_gain_res <- sum(jur_output_df$jur_total_effgain_res)
  #Show the final carbon levels across all jurisdictions, then
  #create total change in carbon for each practice in baseline, ppp, and ppco2 schemes.
  total_bl_outc <- sum(jur_output_df$jur_bl_outc)
  total_add_outc_act <- sum(jur_output_df$jur_total_add_outc_act)
  total_add_outc_res <- sum(jur_output_df$jur_total_add_outc_res)
  #Show total baseline adoption, real and estimated
  total_bl_adopt <- sum(jur_output_df$jur_bl)
  total_bl_adopt_hat <- sum(jur_output_df$jur_bl_hat)

  #Create output row
  run_num <- run_num
  output_vec <- c(run_num, num_jur, jur_size,
                  cc_nr_mean, cc_nr_sd,
                  e_c0_mean, e_c0_sd,
                  e_nr_mean, e_nr_sd,
                  c_outc_distr_choice,
                  unif_outc_lb, unif_outc_ub,
                  norm_outc_mean, norm_outc_sd,
                  ppp_payment, ppco2_payment, theta,
                  use_unbiased_bl,
                  total_bl_adopt, total_bl_adopt_hat,
                  total_bl_outc,
                  ###PPP outputs
                  total_add_outc_act,
                  total_jur_enrolled_act,
                  total_plots_enrolled_act,
                  total_credits_act,
                  total_spur_adopt_act,
                  total_eff_gain_act,
                  total_typ2_credits_act,
                  total_payments_rec_act,
                  total_jur_exp_act,
                  total_jur_eff_incl_act,
                  total_jur_eff_excl_act,
                  total_jur_ineff_incl_act,
                  total_jur_ineff_excl_act,
                  ###PPCO2 outputs
                  total_add_outc_res,
                  total_jur_enrolled_res,
                  total_plots_enrolled_res,
                  total_credits_res,
                  total_spur_adopt_res,
                  total_eff_gain_res,
                  total_typ2_credits_res,
                  total_overest_adopt_res,
                  total_overest_true_outc_res,
                  total_payments_rec_res,
                  total_jur_exp_res,
                  total_jur_eff_incl_res,
                  total_jur_eff_excl_res,
                  total_jur_ineff_incl_res,
                  total_jur_ineff_excl_res
  )

  output_df <- data.frame(matrix(ncol=49,nrow=1,
                                 dimnames=list(NULL, c(
                                   #Input parameters and baseline outcomes
                                   "run_num", "num_jur", "jur_size",
                                   "cc_nr_mean", "cc_nr_sd",
                                   "e_c0_mean", "e_c0_sd",
                                   "e_nr_mean", "e_nr_sd",
                                   "c_outc_distr_choice",
                                   "unif_outc_lb", "unif_outc_ub",
                                   "norm_outc_mean", "norm_outc_sd",
                                   "ppp_payment", "ppco2_payment", "theta",
                                   "use_unbiased_bl",
                                   "total_bl_adopt", "total_bl_adopt_hat",
                                   "total_bl_outc",
                                   #PPP outputs
                                   "total_add_outc_act",
                                   "total_jur_enrolled_act",
                                   "total_plots_enrolled_act",
                                   "total_credits_act",
                                   "total_spur_adopt_act",
                                   "total_eff_gain_act",
                                   "total_typ2_credits_act",
                                   "total_payments_rec_act",
                                   "total_jur_exp_act",
                                   "total_jur_eff_incl_act",
                                   "total_jur_eff_excl_act",
                                   "total_jur_ineff_incl_act",
                                   "total_jur_ineff_excl_act",
                                   #PPCO2 outputs
                                   "total_add_outc_res",
                                   "total_jur_enrolled_res",
                                   "total_plots_enrolled_res",
                                   "total_credits_res",
                                   "total_spur_adopt_res",
                                   "total_eff_gain_res",
                                   "total_typ2_credits_res",
                                   "total_overest_adopt_res",
                                   "total_overest_true_outc_res",
                                   "total_payments_rec_res",
                                   "total_jur_exp_res",
                                   "total_jur_eff_incl_res",
                                   "total_jur_eff_excl_res",
                                   "total_jur_ineff_incl_res",
                                   "total_jur_ineff_excl_res"
                                 ))))

  output_df <- rbind(output_df, output_vec)
  output_df <- output_df %>% dplyr::filter(!is.na(run_num))
}
#----------------------------END FUNCTION -------------------------------------#
