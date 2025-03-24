#' function to predict catch by stock, gear and week based on predictions from an encounter rate model and a bunch of user inputs
#'
#' @param schedule
#' @param mort_rates
#' @param licenses
#' @param sets_per_day
#' @param mark_rates
#' @param run_size
#' @param weekly_HR
#' @param adult_props
#' @param allocation
#'
#' @return
#' @export
#'
#' @examples
catch_predict<-function(schedule,mort_rates,licenses,sets_per_day,mark_rates,run_size,weekly_HR,adult_props,allocation){


  #Join all the inputs

  # estimated encounter rate (adult fish per set for seines and day for pound nets) by stock, gear, week, and zone
  out<-weekly_HR %>%
    # join run size by stock
    dplyr::left_join(
      run_size
    ) %>%
    # join percent adults in catch
    dplyr::left_join(adult_props) %>%
    #join mark rates by stock
    dplyr::left_join(mark_rates) %>%
    #join days per week
    dplyr::left_join(
      schedule %>% tidyr::pivot_longer(c(Beach,Purse,Pound),names_to="gear",values_to = "days_per_week")
    ) %>%
    dplyr::mutate(days_per_week=tidyr::replace_na(days_per_week,0)) %>%
    #join sets per day
    dplyr::left_join(sets_per_day) %>%
    #join licences per gear %>%
    dplyr::left_join(licenses) %>%
    #joint release mortality rates
    dplyr::left_join(mort_rates) %>%
    #------------------------------------------------------
  # calculate values
  dplyr::mutate(
    # keeper rate per set = encounter rate (HR) * mark rate or 0 for steelhead, which are all released.
    Keeper_rate_per_set = ifelse(Species=="Steelhead",0,
                                 HR * `Mark rate`),
    # releases per set = encounter rate - keepers per set
    release_rate_per_set = HR - Keeper_rate_per_set,
    # sets per week = days per week * sets per day * licenses per gear
    sets_per_week = days_per_week * sets_per_day * Licenses,
    # keeper rate adults per week = keepers per set * sets per week
    keeper_adult_rate = Keeper_rate_per_set * sets_per_week,
    # keeper rate adults and jacks per week = kept adults per week / proportion adult
    keeper_adult_and_jack_rate = keeper_adult_rate /prop_adult,
    # released adults per week = released per set * sets per week
    releas_adult_rate = release_rate_per_set * sets_per_week,
    # release adult morts per week = released fish per week * release mort rate
    release_adult_mort_rate = releas_adult_rate * `Mort rate`,
    # total adult morts  per week = kept adults per week + release morts per week
    total_adult_mort_rate = keeper_adult_rate + release_adult_mort_rate,

    # kept adults = keeper adult rate * run size
    kept_adults = keeper_adult_rate * `Run size`,
    #kept adults and jacks = keeper adult and jack rate * run size
    kept_adults_and_jacks = keeper_adult_and_jack_rate * `Run size`,
    Release_adult=releas_adult_rate* `Run size`,
    Release_adult_morts= release_adult_mort_rate * `Run size`
  )

  #------------------------------------------------------
  # some summaries

  #kept fish by stock group and week
  stock_group_weekly<-out %>%
    #exclude some Chinook sub-stocks that are included in other stocks but also broken out for calculating impacts used in harvest control rules. Also exclude steelhead because none kept in commercial fisheries.
    dplyr::filter(!Stock%in%c("LCR wild","SRW","LRH")) %>%
    dplyr::group_by(week,gear,Stock_group,days_per_week,zone,sets_per_day,Licenses) %>%
    dplyr::summarise(dplyr::across(c(kept_adults,Release_adult,Release_adult_morts),\(x)sum(x,na.rm=T))) %>%
    dplyr::mutate(dplyr::across(c(kept_adults,Release_adult,Release_adult_morts),~. /Licenses,.names = "{.col}_per_license"))

  #total kept fish by stock group
  stock_group_total<-stock_group_weekly %>% dplyr::group_by(gear,Stock_group,Licenses) %>%
    # summarise(across(contains("kept_adults")|contains("Release_adult"),\(x)round(sum(x)))) %>%
    dplyr::arrange(gear)




  #join impacts and allocation
  impacts<-allocation %>% dplyr::left_join(
    out %>% dplyr::filter(Stock%in%c("URB","LCR wild","SRW","B Run wild","A Run wild")) %>% dplyr::group_by(Stock,gear) %>% dplyr::summarize(HR=sum(total_adult_mort_rate,na.rm=T)) %>%
      tidyr::pivot_wider(values_from = HR,names_from = gear) %>%
      dplyr::mutate(Total = Beach+Pound+Purse,
             Stock=dplyr::case_when(Stock=="A Run wild"~"Wild A Sthd",
                             Stock=="B Run wild"~"Wild B Sthd",
                             Stock=="LCR wild"~"LCR Wild Tules",
                             Stock=="SRW"~"SR Wild Brights",
                             Stock=="URB"~"Upriver Brights")
      )) %>%
    dplyr::mutate(over_allocation= Total >`Total ECF`)

  # if( impacts %>% pull(over_allocation) %>% sum %>% `>`(0)){
  #   print("The ECF is expected to exceed its allocation of at least one stock based on this plan")
  # }else{
  #   print("The ECF is expected to fit within its allocation based on this plan")
  # }

  # print warning if over allocation and return allocation table

  return(list(detail=out,
              stock_group_weekly=stock_group_weekly,
              stock_group_total=stock_group_total,
              allocation_and_impacts=impacts,
              schedule=schedule))

}

