

#' ECF_mod
#'
#' @param path Character. The file path to the input Excel file.
#' @param year Number. The year to model
#'
#' @return
#' @export
#'
#' @examples
ECF_mod<-function(path=system.file("extdata","Inputs.xlsx", package = "ECFchinook") ,year=2025,find_quota=TRUE){
  dat<-data_fun(path)
  quotas<-find_quotas(dat,find_quota)
  sum_res<-summarize_results(quotas,year)
  tabs<-tabs_fun(sum_res,dat)
  figs<-figs_fun(sum_res$stock_grp_tots,sum_res$schdls)
  weekly_impacts_fig=weekly_impacts_fig_fun(dat,sum_res$weekdates)
  list(dat = dat,
       sum_res = sum_res,
       tabs = tabs,
       figs = c(figs,list(weekly_impacts_fig=weekly_impacts_fig)))
}

#' Make ECF Chinook quota planning report
#'
#' @description
#' A call to this function finds the Chinook quotas associated with the values in an input Excel file. It saves an HTML report with information on the inputs and the resulting quotas and expected harvest and impacts. It also saves two CSV files, one with Chinook impact values for use in the pre-season Chinook MR and one for the pre-season steelhead model.
#'
#'
#' @param year Number. The year to model
#' @param output_file Charachter. The file name for the report.
#' @param output_dir The output directory for the report. Defaults to the current working directory.
#' @param Input_path
#' @param find_quota Boolean. TRUE to find the maximum number of kept Chinook for fishing with the distribution of effort specified in the schedule tab. FALSE to find the expected catch with days of week specified in the schedule tab.
#' @param ...
#'
#'
#' @return
#' @export
#'
#' @examples
ECF_mod_report<-function(Input_path=system.file("extdata","Inputs.xlsx", package = "ECFchinook") ,
                         year=2025,
                         find_quota=TRUE,
                         output_file = "ECF Chinook quota planning report",
                         output_dir = getwd(),
                         ...){


  ECF_Mod_out<-ECF_mod(path=Input_path,year,find_quota)

  Sys.Date()

  readr::write_csv(ECF_Mod_out$sum_res$stlhead,file.path(output_dir,paste0(output_file,"_steelhead_impacts_for_preseason_model",".csv")))

  readr::write_csv(ECF_Mod_out$sum_res$impacts_for_MR,file.path(output_dir, paste0(output_file,"Chinook_impacts_for_preseason_model",".csv")))


  template_path <- system.file("rmarkdown","templates","ecf-model-report" ,"skeleton","skeleton.Rmd", package = "ECFchinook")

  if (template_path == "") {
    stop("Template file not found. Ensure it exists in the package.")
  }

  rmarkdown::render(
    input = template_path,
    output_file = file.path(output_dir, paste0(output_file,".html")),
    params = list(
      tabs=ECF_Mod_out$tabs,
      figs=ECF_Mod_out$figs
    )

    ,
    envir = new.env() # Avoids variable conflicts
  )


}
