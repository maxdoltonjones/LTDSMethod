#' LTDS summary
#'
#' Uses LTDS field data and estimation results to create a report summary.
#' @param effort Total effort in meters from full LTDS survey
#' @param nburr Number of burrows detected
#' @param burr.w.mn Mean burrow width
#' @param burr.w.min Minimum burrow width
#' @param burr.w.max Maximum burrow width
#' @param comm.comm Most common commensal species
#' @param occ Number of occupied burrows discovered
#' @param unocc Number of unoccupied burrows discovered
#' @param unk Number of unknown burrows discovered
#' @param site.name Name of recipient site
#' @param tort.dens Tortoise density per acre at recipient site
#' @param tort.pop Tortoise population estimate at recipient site
#' @return A PDF report of LTDS data
#' @examples
#' #Read in csv file showing full LTDS survey data
#' ltds_data <- read_csv("./LTDS_example_data.csv")
#'
#' #Input raw data into function
#' new_ltds_data <- ltds_summary(ltds_data = ltds_data, mort_data = mort_data, save = T)
#'
#' @export
LTDS_summary <- function(effort = "NA", nburr = "NA",
                         burr.w.mn = "NA", burr.w.min = "NA",
                         burr.w.max = "NA", comm.comm = "NA",
                         occ = "NA", unocc = "NA", unk = "NA",
                         site.name = "NA", tort.dens = "NA", tort.pop = "NA"){

  rmarkdown::render('./vignettes/LTDS_summary.Rmd',
                    output_file = paste0('LTDS_report.pdf'),
                    params = list(
                      effort = effort,
                      nburr = nburr,
                      burr.w.mn = burr.w.mn,
                      burr.w.min = burr.w.min,
                      burr.w.max = burr.w.max,
                      comm.comm = comm.comm,
                      occ = occ,
                      unocc = unocc,
                      unk = unk,
                      site.name = site.name,
                      tort.dens = tort.dens,
                      tort.pop = tort.pop
                    ))

}
