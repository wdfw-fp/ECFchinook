#' fill_func
#'
#' @param opt_dat
#' @param starting_quota
#' @param increment
#'
#' @return
#' @export
#'
#' @examples
fill_func<-function(opt_dat,do_quota,starting_quota=500,increment=10){
  opt_dat2<-opt_dat

  catch_predict_opt<-do.call(catch_predict,opt_dat)


  if(do_quota){

  chk_scaler<-catch_predict_opt$stock_group_total %>% dplyr::filter(Stock_group%in%c("Bright","Tule")) %>% dplyr::group_by(gear) %>% dplyr::summarise(out=1/sum(kept_adults_per_license,na.rm=T)) |>
    dplyr::mutate(out=ifelse(is.infinite(out),0,out))



  qta<-starting_quota
  seen_FALSE<-FALSE
  fits <-TRUE
  while(!(fits & seen_FALSE)){
    opt_dat2$schedule<-tibble::tibble(opt_dat$schedule[,1:2],Beach=opt_dat$schedule$Beach*chk_scaler$out[1]*qta,Purse=opt_dat$schedule$Purse*chk_scaler$out[3]*qta,Pound=opt_dat$schedule$Pound* chk_scaler$out[2]*qta)

    catch_predict_opt_next<-do.call(catch_predict,opt_dat2)

    fits<- !any(catch_predict_opt_next$allocation_and_impacts$over_allocation[1:3])

    if(fits&seen_FALSE){
      catch_predict_opt<-catch_predict_opt_next
    }else{
      if(fits&!seen_FALSE){
        qta<-qta+increment
      }else{
        qta<-qta-increment
        seen_FALSE<-TRUE
      }
    }

  }
  }else{
    qta<-NA
}
  return(list(catch_predict_opt=catch_predict_opt,
              qta=qta))
}
