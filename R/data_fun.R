

data_fun<-function(path=system.file("extdata","Inputs.xlsx", package = "ECFchinook") ){
  ecf_data<-(here::here(path))
  # inputs to catch prediction and impact model
  Seasons<-readxl::read_xlsx(ecf_data,sheet="Seasons")|> tidyr::nest(.by=Option_name)

  # release mort rates
  mort_rates<-readxl::read_xlsx(ecf_data,sheet="Mort rates",range="A1:C10")
  # sets per seine type per day
  ##note: one set of pound net represents and entire day of fishing
  sets_per_day<-readxl::read_xlsx(ecf_data,sheet="Sets per day",range="A1:C7")
  #adipose clip rate by stock
  mark_rates<-readxl::read_xlsx(ecf_data,sheet="Mark rates",range="B1:C15")
  # run size by stock
  run_size<-readxl::read_xlsx(ecf_data,sheet="Run size",range="A1:B15",col_names = c("Stock","Run size"))
  # predicted encounter rates per set by gear (this is the output of a script called 'Alt gear mod new' currently located at: "C:\Users\sorelmhs\OneDrive - Washington State Executive Branch Agencies\Documents\Alt gear\implementation model\Alt_gear_model\Alt gear mod new.R")
  weekly_HR<-readxl::read_xlsx(ecf_data,sheet="Weekly_HR") |>
    dplyr::mutate(Stock_group=dplyr::case_when(grepl("coho",Stock2)~"Coho",
                                 grepl("Run",Stock2)~"Steelhead",                                      TRUE~Stock2)) %>% dplyr::select(-Stock2) |>
    dplyr::filter(Stock!="LRH") |>
    dplyr::mutate(HR=as.numeric(HR))
  # proportion of catch that are adults, for expanding to include jacks
  adult_props<-readxl::read_xlsx(ecf_data,sheet="Percent_jacks") |>
    dplyr::filter(lifestage=="Adult") %>% dplyr::select(Stock_group=Stock,prop_adult=prop) %>% dplyr::bind_rows(tibble::tibble(Stock_group="Steelhead",prop_adult=1))
  ## allocation of impacts for the ECF for a few stocks that tend to be limiters
  allocation<-readxl::read_xlsx(ecf_data,sheet="Allocations") %>% tidyr::drop_na() %>% dplyr::select(Stock,`Total ECF`) |> dplyr::mutate(`Total ECF`=`Total ECF`)

  # Assumptions
  licenses<-readxl::read_xlsx(ecf_data,sheet="Licenses per gear",range="A1:B4") %>% dplyr::rename(gear=Gear)




  list(Seasons = Seasons,
       mort_rates = mort_rates,
       sets_per_day = sets_per_day,
       mark_rates = mark_rates,
       run_size = run_size,
       weekly_HR = weekly_HR,
       adult_props = adult_props,
       allocation = allocation,
       licenses = licenses
       )
}
