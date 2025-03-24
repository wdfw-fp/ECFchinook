

#' find_quotas
#'
#' @param dat
#' @param find_quota
#'
#' @return
#' @export
#'
#' @examples
find_quotas<-function(dat,find_quota){



dat$Seasons |> dplyr::mutate(results=
                                     purrr::map(data,~fill_func(c(list(schedule=.x),dat[-1]),find_quota)))
}



#' summarize_results
#'
#' @param quota_out
#'
#' @return
#' @export
#'
#' @examples
summarize_results<-function(quota_out,year){

  weekdates<-tibble::tibble(date=seq.Date(from=as.Date(paste0(year,"-08-01")),to=as.Date(paste0(year,"-12-01")),by=1)) |> dplyr::mutate(week=calweek(date)) |> dplyr::group_by(week) |> dplyr::filter(date==min(date))


  dtls<- quota_out |> dplyr::mutate(
      detail=purrr::map(results,~.x$catch_predict_opt$detail)
    )  |> dplyr::select(
      Option=Option_name,
      detail
    ) |>
    tidyr::unnest(cols = c(detail)) |>
    dplyr::mutate(#Option=fct_relevel(Option,rev(option_names)),
                                                                                           gear=forcats::fct_relevel(gear,c("Beach","Purse","Pound")))



  stock_grp_tots<-quota_out |> dplyr::mutate(
    stock_group_total=purrr::map(results,~.x$catch_predict_opt$stock_group_total)
  )  |> dplyr::select(
    Option=Option_name,
    stock_group_total
  ) |>
    tidyr::unnest(cols = c(stock_group_total)) |>
    dplyr::mutate(
      gear=forcats::fct_relevel(gear,c("Beach","Purse","Pound")))


  schdls<-quota_out |> dplyr::mutate(
    schedule=purrr::map(results,~.x$catch_predict_opt$schedule)
  )  |> dplyr::select(
    Option=Option_name,
    schedule
  ) |>
    tidyr::unnest(cols = c(schedule))



  allctn<-quota_out |> dplyr::mutate(
    allocation_and_impacts=purrr::map(results,~.x$catch_predict_opt$allocation_and_impacts)
  )  |> dplyr::select(
    Option=Option_name,
    allocation_and_impacts
  ) |>
    tidyr::unnest(cols = c(allocation_and_impacts))%>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),\(x)round(x*100,2)))

# this is for the steelhed models
  stlhead<-dtls |> dplyr::filter(Species=="Steelhead") |> dplyr::group_by(Stock,week,gear,`Mort rate`) |> dplyr::summarise(dplyr::across(c(kept_adults,Release_adult , Release_adult_morts,total_adult_mort_rate),\(x)round(as.numeric(sum(x,na.rm=TRUE)),3))) |> dplyr::left_join(weekdates)

  ##|> write_csv("weekly_steelhead_impacts.csv")

#this is for the MR model
  impacts_for_MR<-dtls |> dplyr::filter(Species=="Chinook") |>
    dplyr::mutate(Stock=ifelse(Stock=="PUB","PUBLRB",Stock)) |>
    dplyr::mutate(Stock=forcats::fct_relevel(Stock,c("LRH1","LRH2","LRW","BPH","URB","PUBLRB","SAB","LCR wild","SRW"
    ))) |>
    dplyr::group_by(gear,Stock) |> dplyr::summarise(dplyr::across(c(kept_adults,Release_adult),\(x)sum(x,na.rm=TRUE))) |> tidyr::pivot_longer(c(kept_adults,Release_adult),names_to = "Mark",values_to = "Handle") |>
    dplyr::mutate(Mark=ifelse(Mark=="kept_adults","Mrkd","Umrkd"),
           Kept=ifelse(Mark=="Mrkd",Handle,0),
           Released=ifelse(Mark=="Umrkd",Handle,0)) |> tidyr::pivot_longer(c(Handle,Kept,Released),names_to = "Cat",values_to = "Fish") |> dplyr::mutate(Fish=round(Fish,3)) |>
    tidyr::pivot_wider(names_from = c(Stock,Mark),values_from = c(Fish))
  ## |> write.csv("impacts_for_preseason_gear.csv")

  list(dtls = dtls,
       stock_grp_tots = stock_grp_tots,
       schdls = schdls,
       allctn = allctn,
       stlhead = stlhead,
       impacts_for_MR = impacts_for_MR,
       weekdates=weekdates)

}



