#' Estimation of latent class mixed multinomial probit models via Gibbs sampling
#' @description Function that performs Gibbs sampling and returns model results.
#' @details The model specifications are ordered in named lists.
#'   You can either specify none, all, or only selected parameters.
#'   Unspecified parameters are set to default values.
#'   See the README file for details.
#' @param model A list of model information.
#' @param data A list of data information.
#' @param parm A list of true parameter values.
#' @param lcus A list of latent class updating scheme parameters.
#' @param init A list of initial values for the Gibbs sampler.
#' @param prior A list of prior parameters.
#' @param mcmc A list of Markov chain Monte Carlo parameters.
#' @param norm A list of normalization information.
#' @param out A list of output settings.
#' @return No return value by default.
#'   Function returns a list of estimates if \code{out[["return"]] = TRUE}.
#'   Results saved in folder "\code{out[["rdir"]]/out[["id"]]}".
#' @examples
#' \dontrun{
#' ### fit a multinomial probit model to simulated data with default parameters
#' ### computation time: < 1 min
#' rpb()
#' }
#' @export

rpb = function(model = NULL,
               data  = NULL,
               parm  = NULL,
               lcus  = NULL,
               init  = NULL,
               prior = NULL,
               mcmc  = NULL,
               norm  = NULL,
               out   = NULL) {

  tryCatch(
    {

    ### transform empirical data
    ### sim (boolean) determines whether data is simulated or empirical
    if(!is.null(data)){
      data = transform_data(data_raw = data$data_raw,
                             cov_col = data$cov_col,
                             cov_ord = data$cov_ord,
                             cov_zst = data$cov_zst)
      sim = FALSE
    }

    ### perform pre-checks
    model = check_model(model,data)
    norm  = check_norm(norm,model)
    parm  = check_parm(parm,model,data,norm)
    check_data_out  = check_data(data,model,parm,norm)
    data  = check_data_out$data
    parm  = check_data_out$parm
    lcus  = check_lcus(lcus,model)
    init  = check_init(init,model,parm,lcus)
    prior = check_prior(prior,model)
    mcmc  = check_mcmc(mcmc)
    out   = check_out(out)

    ### save model parameters
    check_saving(out,list("model" = model,
                          "data"  = data,
                          "parm"  = parm,
                          "lcus"  = lcus,
                          "init"  = init,
                          "prior" = prior,
                          "mcmc"  = mcmc,
                          "norm"  = norm,
                          "out"   = out))

    ### compute sufficient statistics
    suff_statistics = compute_suff_statistics(model,data)

    ### start collecting prints
    sink(paste0(out$rdir,"/",out$id,"/protocol.txt"),split = TRUE)

      ### print model settings
      print_settings(model,lcus,init,mcmc,norm,out)

      ### perform Gibbs sampling
      gibbs_loop_out = gibbs_loop(mcmc$R,mcmc$B,mcmc$nprint,
                                  model$N,model$J-1,model$P_f,model$P_r,model$C,
                                  lcus,suff_statistics,prior,init)

      ### normalize, burn and thin Gibbs samples
      gibbs_samples = gibbs_transform(gibbs_loop_out,model,mcmc,norm)

      ### compute point estimates and standard deviations
      estimates = gibbs_estimates(gibbs_samples,model,parm)

    sink()

    ### save model results
    check_saving(out,list("gibbs_loop_out" = gibbs_loop_out,
                          "gibbs_samples"  = gibbs_samples,
                          "estimates"      = estimates))

    ### compute WAIC
    if(out$waic){
      WAIC = compute_waic(model,gibbs_samples,data)
      write(sprintf("WAIC: %.4f",WAIC),
            file=paste0(out$rdir,"/",out$id,"/protocol.txt"),append=TRUE)
    }

    ### make plots
    cat("Visualising...\r")
      gibbs_trace(gibbs_samples,model,mcmc,out)
      gibbs_acf(gibbs_samples,model,mcmc,out)
      gibbs_marginals(gibbs_samples,model,estimates,parm,out)
      gibbs_contour(gibbs_samples,model,estimates,parm,mcmc,lcus,out)

    if(out$rdir=="."){
      cat("Results folder: current directory")
    } else {
      cat(paste0("Results folder: ",out$rdir))
    }

    if(out$return){
      ### return estimates
      return(estimates)
    }

    },

  error = function(cond) message(paste0(cond),appendLF=FALSE),

  finally = {
    for(i in seq_len(sink.number())) sink()
    while(!is.null(dev.list())) dev.off()
  }

  )

}
