


#' tabs_fun
#'
#' @param summarized_results
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
tabs_fun<-function(summarized_results,dat){

  option_names<-unique(summarized_results$stock_grp_tots$Option)

  options(knitr.kable.NA = '')

  run_size_group_tab<-dat$run_size %>% dplyr::left_join(dat$weekly_HR %>% dplyr::select(Stock,Stock_group) %>% dplyr::distinct()) %>% dplyr::mutate(`Stock group`=ifelse(Stock_group%in%c("Tule","Bright"),Stock_group,Stock)) %>% dplyr::left_join(dat$mark_rates) %>%
    dplyr::group_by(`Stock group`) %>%  dplyr::summarise(`Mark rate`=round(weighted.mean(`Mark rate`,`Run size`),3),`Run size`=sum(`Run size`)) %>% dplyr::filter(!grepl("wild",`Stock group`)) %>% dplyr::select(`Stock group`,`Run size`, `Mark rate` ) %>%
    dplyr::mutate(`Stock group`=forcats::fct_relevel(`Stock group`,c("Early_coho", "Late_coho","Bright","Tule","A Run","B Run"))) %>% dplyr::arrange(`Stock group`) %>%
       knitr::kable() %>% kableExtra::kable_styling(full_width=FALSE)

  run_size_tab<-dat$run_size %>% dplyr::left_join(dat$mark_rates) %>%
    dplyr::mutate(`Mark rate`=round(`Mark rate`,3)) |>
    knitr::kable() %>% kableExtra::kable_styling(full_width=FALSE)


  licences_tab<-dat$licenses|>
    dplyr::rename(Gear=gear,"# fishers"=Licenses) |>
    knitr::kable() %>% kableExtra::kable_styling(full_width=FALSE)


  schle_tab<-summarized_results$schdls %>% dplyr::mutate(dplyr::across(c(Beach,Purse,Pound),\(x) round(tidyr::replace_na(x,0),1)))%>% dplyr::filter((Beach+Purse+Pound)>0)|> tidyr::pivot_wider(id_cols = c(week,zone),values_from = c(Beach,Purse,Pound),names_from = Option) |> dplyr::left_join(summarized_results$weekdates)%>% dplyr::select(-week) |>  dplyr::relocate(date,.before = everything())|> dplyr::mutate(date= format(date, "%b %d")) |> dplyr::filter(!is.na(date)) |>  knitr::kable("html",col.names=c("Week","Zone",rep(option_names,times=3))) %>% kableExtra::add_header_above(c(" "=2,"Beach"=length(option_names),"Purse"=length(option_names),"Pound"=length(option_names)), align = "c") %>% kableExtra::kable_styling(full_width=FALSE)

  quotas_tab<-summarized_results$stock_grp_tots %>%dplyr::filter(Stock_group%in%c("Bright","Tule")) %>% dplyr::group_by(Option,gear)%>%  dplyr::summarise(`Chinook IFQ`=round(sum(kept_adults_per_license))) |> tidyr::pivot_wider(names_from = gear,values_from = `Chinook IFQ`)%>%
    # dplyr::ungroup() %>% dplyr::arrange(dplyr::desc(Option)) %>%
    dplyr::rename("Concept"=Option) %>%# dplyr::mutate(Concept=rep(option_names),Concept=forcats::fct_relevel(Concept,option_names)) %>%
    #dplyr::select(Concept,`Chinook IFQ`) %>%
    knitr::kable("html") %>% kableExtra::kable_styling(full_width=FALSE)

  total_per_fisher_sumry_tab<-summarized_results$dtls %>% dplyr::filter(!grepl("wild",Stock))%>%dplyr::ungroup() |>  dplyr::mutate(Group=forcats::fct_relevel(Stock_group,c("Bright","Tule","Coho","Steelhead"))) |>
    # case_when(Stock_group%in%c("Bright","Tule")~"Chinook",
    # TRUE~Stock_group)) %>%
    dplyr::group_by(Concept=Option,gear,Group) %>% dplyr::summarise(dplyr::across(c(kept_adults,Release_adult,Release_adult_morts),\(x)sum(x,na.rm=T))) %>% dplyr::mutate(dplyr::across(where(is.numeric),\(x)round(x,0))) |> dplyr::left_join(dat$licenses) |> dplyr::mutate(dplyr::across(kept_adults:Release_adult_morts,\(x)round(x/Licenses))) |> dplyr::select(-Licenses) |> knitr::kable("html") %>%  kableExtra::kable_styling(full_width=FALSE)


  allctn_tab<-knitr::kable(summarized_results$allctn %>% dplyr::filter(Stock%in%c("Upriver Brights","LCR Wild Tules","Wild B Sthd")),"html",col.names = c("Concept","Stock", "Total ECF allowed", "Beach","Pound","Purse","Total","Over allocation"))%>% kableExtra::add_header_above(c(" "=3,"Predicted impact"=5), align = "c") %>% kableExtra::kable_styling(full_width=FALSE)

  color_vec<-rep(c("lightblue","white"),times=4)


  for(i in 1:length(option_names)){
    allctn_tab<-allctn_tab %>% kableExtra::row_spec(seq(from=((i-1)*3+1),length.out=3) , background = color_vec[i])
  }

  total_sumry_tab<-summarized_results$dtls %>% dplyr::filter(!grepl("wild",Stock))%>% dplyr::mutate(Group=dplyr::case_when(Stock_group%in%c("Bright","Tule")~"Chinook",
                                                                               TRUE~Stock_group)) %>% dplyr::group_by(Option,Group) %>% dplyr::summarise(dplyr::across(c(kept_adults,Release_adult,Release_adult_morts),\(x)sum(x,na.rm=T))) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric),\(x)round(x,0))) %>% dplyr::rename(Concept=Option) %>%  dplyr::ungroup() %>% dplyr::mutate(Concept=forcats::fct_relevel(Concept,option_names))%>% dplyr::arrange(Concept) |> knitr::kable("html") %>% kableExtra::kable_styling(full_width=FALSE)


  list(run_size_group_tab = run_size_group_tab,
       run_size_tab = run_size_tab,
       licences_tab = licences_tab,
       schle_tab = schle_tab,
       quotas_tab = quotas_tab,
       total_per_fisher_sumry_tab = total_per_fisher_sumry_tab,
       allctn_tab = allctn_tab,
       total_sumry_tab = total_sumry_tab)

}



#' figs_fun
#'
#' @param stock_grp_tots
#' @param schdls
#'
#' @return
#' @export
#'
#' @examples
figs_fun<-function(stock_grp_tots,schdls){

  catch<-  stock_grp_tots %>% dplyr::ungroup() %>% tidyr::pivot_longer(c(kept_adults_per_license,Release_adult_per_license),names_to = "param",values_to = "Fish")%>% dplyr::mutate(param= gsub("_per_license", "", param),param= gsub("_", " ", param),Stock_group=forcats::fct_relevel(Stock_group,c("Steelhead","Coho","Tule","Bright"))) %>% ggplot2::ggplot(ggplot2::aes(y=gear,x=Fish,fill=Stock_group)) + ggplot2::geom_col(orientation="y")+ggplot2::facet_grid(Option     ~param,scales = "free_x")+ggplot2::ylab("Gear")+ggplot2::scale_fill_brewer(type="qual",palette="Paired")+ggplot2::xlab("Adults") + ggplot2::theme(text = ggplot2::element_text(size = 18)) + ggplot2::theme(panel.spacing = ggplot2::unit(1, "cm", data = NULL))


  cperday<-  stock_grp_tots %>% dplyr::ungroup() %>% dplyr::filter(Stock_group!="Steelhead") %>% dplyr::mutate(Stock_group=forcats::fct_relevel(Stock_group,c("Coho","Tule","Bright"))) %>% dplyr::left_join(schdls%>% dplyr::group_by(Option) %>%  dplyr::summarize(dplyr::across(c(Beach,Pound,Purse),\(x)sum(x,na.rm=TRUE))) %>% tidyr::pivot_longer(-Option,names_to = "gear",values_to = "days")) %>% #dplyr::mutate(gear=str_to_title(gear))) %>%
  dplyr::mutate(`Kept adults per day`=kept_adults_per_license/days,gear=forcats::fct_relevel(gear,c("Beach","Purse","Pound")))%>% ggplot2::ggplot(ggplot2::aes(y=gear,x=`Kept adults per day`,fill=Stock_group))+ ggplot2::geom_col(orientation="y")+ggplot2::facet_wrap(~Option,scales = "free_x")+
    ggplot2::scale_fill_manual(values=c('#1f78b4','#b2df8a','#33a02c'))+ggplot2::ylab("Gear")+ ggplot2::theme(text = ggplot2::element_text(size = 18))



list(catch = catch,
     cperday = cperday)
}


#' weekly_impacts_fig_fun
#'
#' @param dat
#' @param weekdates
#'
#' @return
#' @export
#'
#' @examples
weekly_impacts_fig_fun<-function(dat,weekdates){
  weekdates[1,1]<-weekdates[2,1]-7

sched<-dat$Seasons$data[[1]] |> dplyr::mutate(Beach=1,Purse=1,Pound=1)
  dat<-do.call(catch_predict,(c(list(schedule=sched),dat[-1])))

z13 <-  dat$detail%>% dplyr::filter(zone==13) |> dplyr::filter(dplyr::between(week,31,43),!Stock%in%c("LRH1","LRH2","LCR wild","SRW")) |> dplyr::mutate(Stock_group=ifelse(Stock_group=="Steelhead",Stock,Stock_group)) %>% dplyr::mutate(kept = (keeper_adult_rate),release_morts=(release_adult_mort_rate)) %>% dplyr::group_by(week,Stock_group,gear) %>% dplyr::summarize(dplyr::across(c(kept,release_morts),mean)) %>% tidyr::pivot_longer(c(kept,release_morts)) %>%dplyr::ungroup() %>%  dplyr::mutate(name=forcats::fct_relevel(name,c("release_morts","kept")),Stock_group=forcats::fct_relevel(Stock_group,c("Bright","Tule","Coho","A Run","A Run wild", "B Run", "B Run wild" )),gear=forcats::fct_relevel(gear,c("Beach","Purse","Pound"))) %>% dplyr::left_join(weekdates) %>%  ggplot2::ggplot(ggplot2::aes(x=date,y=value*100,fill=name))+ggplot2::geom_bar(stat="identity")+ggplot2::facet_grid(Stock_group~gear,scales = "free_y")+ggplot2::scale_fill_manual(values=c("red","black"))+ggplot2::ylab("Daily impact rate per fisher %")

z45 <-  dat$detail%>% dplyr::filter(zone==45)%>% dplyr::filter(dplyr::between(week,31,43),!Stock%in%c("LRH1","LRH2","LCR wild","SRW")) |> dplyr::mutate(Stock_group=ifelse(Stock_group=="Steelhead",Stock,Stock_group)) %>% dplyr::mutate(kept = (keeper_adult_rate),release_morts=(release_adult_mort_rate)) %>% dplyr::group_by(week,Stock_group,gear) %>% dplyr::summarize(dplyr::across(c(kept,release_morts),mean)) %>% tidyr::pivot_longer(c(kept,release_morts)) %>%dplyr::ungroup() %>%  dplyr::mutate(name=forcats::fct_relevel(name,c("release_morts","kept")),Stock_group=forcats::fct_relevel(Stock_group,c("Bright","Tule","Coho","A Run","A Run wild", "B Run", "B Run wild" )),gear=forcats::fct_relevel(gear,c("Beach","Purse","Pound"))) %>% dplyr::left_join(weekdates) %>%  ggplot2::ggplot(ggplot2::aes(x=date,y=value*100,fill=name))+ggplot2::geom_bar(stat="identity")+ggplot2::facet_grid(Stock_group~gear,scales = "free_y")+ggplot2::scale_fill_manual(values=c("red","black"))+ggplot2::ylab("Daily impact rate per fisher %")

list(z13=z13,
     z45=z45)

}
