pwdout <- "/data/users/hadea/PAGE21_sites/ancillaries/"

# vn3 - updated to add in the vertical profile for the soil clay content 
#        and multiplying by 2 for carbon to OM conversion
# vn4 - updated to include profile information from soil organic carbon obs
soilprofiles = TRUE  #keep this as true if possible
silt_docm=FALSE
mineralsoil_output=TRUE

siteid.all <- c("Abiskomire", "Merbleue", "Zackenberg", "Lompolojankka", 
                "Samoylov", "CA_WP1", "Degero", "EML", "Kytalyk", 
                "Seidamin", "Seidapeat", "Siikaneva",
                "Spasskaya_Pad", "PleistocenePark", "ImnavaitFen",
                "ImnavaitRidge", "ImnavaitTussock", "Abisko", "Chersky",
                "Twitchell", "Iskoras", "Auchencorth", "Kopytkowo", "Hainich",
                "Scottycreek","Brasschaat","Abisko","Scottycreek","Brasschaat",
                "Congo", "Svalbard_Ny","Carlow")
#siteid.all <- c("Inirida" ,"Amacayacu" ,"Iquitos" ,"Mpologoma","Opala" ,"Sumatra")
simname  <- "20layers" # used to define model depths
#simname  <- "20layers" # used to define model depths
if (simname=="14layers"){strlyr <- "14"}
if (simname=="20layers"){strlyr <- "20"}
if (simname=="14layers20"){strlyr <- "20"}
version_no="v4"

# make dummy ctracer file to start things off
ctracer <- rep(1,strtoi(strlyr))
for (siteid in siteid.all) {
   write.table(t(ctracer), row.names=FALSE, col.names=FALSE,
     file=paste(pwdout,siteid,"/",siteid,"_ctracer_",strlyr,".dat", sep=""))
}

source("make_soil_params.R")
source("depths_jules.R")
source("siteProps.R")

#------------------------------------
# organic soil properties initialised
b_org <- c(2.7, 6.1, 12.0)
sathh_org <- c(0.0103, 0.0102, 0.0101)
ks_org <- c(0.28, 0.002, 0.0001) # potentially reduce surface value here?
sm_sat_org <- c(0.93, 0.88, 0.83)
hcap_org <- 0.58e+6
hcon_org <- 0.06
rho_org <- 0.8*0.001/0.01/0.01/0.01
levs_org <- 3

#------------------------------------
#------------------------------------
# assign model depths
soil_thick <- depths_jules(simname)$soil_thick
zz <- cumsum(soil_thick)

for (siteid in siteid.all) {
  
  # output soil properties initialised to NULL
  b_both <- NULL
  sathh_both <- NULL
  k_s_both <- NULL
  sm_sat_both <- NULL
  sm_crit_both <- NULL
  sm_wilt_both <- NULL
  hcap_both <- NULL
  hcon_both <- NULL

  cat(siteid, "\n")
  if(!soilprofiles) {
    fileout=paste(pwdout, siteid, "/", siteid, "_orgprops_",strlyr,"_v3.dat", sep="")
    # fileout=paste("./", siteid, "_orgprops_14_v3.dat", sep="")
  } else {
    if (!silt_docm ) {
      fileout=paste(pwdout, siteid, "/", siteid, "_orgprops_",strlyr,"_",
                  version_no,".dat", sep="")
      } else {
        fileout=paste(pwdout, siteid, "/", siteid, "_orgprops_",strlyr,"_",version_no,"_silt.dat", sep="")
  # fileout=paste("./", siteid, "_orgprops_14_v4.dat", sep="")
      }
  }
  if (mineralsoil_output) {
     fileout=paste(pwdout, siteid, "/", siteid, "_minprops_",strlyr,"_",version_no,".dat", sep="")
   }

  # assign particle size distribution
  sand <- siteProps(siteid, soil_thick, soilprofiles)$sand
  silt <- siteProps(siteid, soil_thick, soilprofiles)$silt
  clay <- siteProps(siteid, soil_thick, soilprofiles)$clay
  # define site specific organic profile information
  depths_org <- c(siteProps(siteid, soil_thick, soilprofiles)$dlev1,
                  siteProps(siteid, soil_thick, soilprofiles)$dlev2, 1000)

  porgz <- NULL
  # define which organic level is being used
  for (iloop in 1:length(zz))           {
    if(zz[iloop] < depths_org[1]) {
      idx <- 1
    } else if (zz[iloop] >= depths_org[1] &&  zz[iloop] < depths_org[2]) {
      idx <- 2
    } else {
      idx <- 3
    }
    porgz[iloop] <- sm_sat_org[idx]  # pure organic matter
  }
  #------------------------------------

  #------------------------------------
  # define organic matter content for the site
  if( is.null( siteProps(siteid, soil_thick[1], soilprofiles)$bulkdens )) { 
    soc_prop = (siteProps(siteid, soil_thick, soilprofiles)$orgsoil * 2/
                  (rho_org*(1-porgz))) # OLD WAY
    # factor of two because organic matter is only about 50 % carbon
  } else { # NEW WAY (requires bulk density)
    forg_weight = 2 * siteProps(siteid, soil_thick, soilprofiles)$orgsoil / 
                    siteProps(siteid, soil_thick, soilprofiles)$bulkdens
    soc_prop = 1 - (siteProps(siteid, soil_thick, soilprofiles)$bulkdens / 1700) *
                           (1 - forg_weight)
    # soc_bd = siteProps(siteid, soil_thick)$bulkdens * forg_weight / soc_prop
    # Here we can calculate the bulk density of the organic matter 
    # if we want to use it to calculate soil properties using functions.
  }
  cat(soc_prop, "\n")
  soc_prop[soc_prop>1.0] <- 1.0
  soc_prop[soc_prop<0.0] <- 0.0
  cat(soc_prop, "\n")
  # below from original paper
  # if(!soilprofiles) {
    # if (siteid =="Samoylov") {soc_prop[]<-0.520833}  # site dependent
    # if (siteid =="Abiskomire") {soc_prop[]<-0.78125}  # site dependent
    # if (siteid =="Zackenberg") {soc_prop[]<-0.15625}  # site dependent
    # if (siteid =="Svalbard_Ny") {soc_prop[]<-0.0885}  # site dependent
  # }
  # above from original paper
  #-----------------------------------
  if (mineralsoil_output) {
    soc_prop[]=0.0
  }
  #------------------------------------
  # loop through the soil layers
  for (iloop in 1:length(zz)) {
    # which level of soil organic properties to use?
    if(zz[iloop] < siteProps(siteid, soil_thick, soilprofiles)$dlev1) {
       idx <- 1
    } else if (zz[iloop] >= siteProps(siteid, soil_thick, soilprofiles)$dlev1 && 
       zz[iloop] < siteProps(siteid, soil_thick, soilprofiles)$dlev2) {
    idx <- 2
    } else {
      idx <- 3
    }
    if (length(zz) == 1) {idx <- 2}

    # calculate soil properties
    soil_props <- list(hcap_org=hcap_org, hcon_org=hcon_org, b_org=b_org[idx], 
      sathh_org=sathh_org[idx], ks_org=ks_org[idx], sm_sat_org=sm_sat_org[idx])
    soil_params <- make_soil_params(soc_prop[iloop], sand, clay, silt,
                                    soil_props, rasterf=FALSE)
    # print out 
    #  cat("depth=", zz[iloop], idx, "\n")
    #  cat(soil_params$b_both, soil_params$sathh_both, soil_params$k_s_both, 
    #      soil_params$sm_sat_both, soil_params$sm_crit_both, 
    #      soil_params$sm_wilt_both, 
    #      soil_params$hcap_both, soil_params$hcon_both, "\n")

    b_both[iloop] <- soil_params$b_both
    sathh_both[iloop] <- soil_params$sathh_both
    k_s_both[iloop] <- soil_params$k_s_both
    sm_sat_both[iloop] <- soil_params$sm_sat_both
    sm_crit_both[iloop] <- soil_params$sm_crit_both
    sm_wilt_both[iloop] <- soil_params$sm_wilt_both
    hcap_both[iloop] <- soil_params$hcap_both
    hcon_both[iloop] <- soil_params$hcon_both
  } # end loop through the soil layers

  # write out
  clay_out <- rep(clay, length(soil_thick))
  silt_out <- rep(silt, length(soil_thick))
  albsoil<-0.2
  if (!silt_docm ) {
       write.table(t(as.data.frame(c(b_both, sathh_both, k_s_both, sm_sat_both, 
                          sm_crit_both, sm_wilt_both, hcap_both, hcon_both, 
                          albsoil, clay_out))), 
                          file=fileout, row.names=FALSE, col.names=FALSE)
} else {
  write.table(t(as.data.frame(c(b_both, sathh_both, k_s_both, sm_sat_both, 
                     sm_crit_both, sm_wilt_both, hcap_both, hcon_both, 
                     albsoil, clay_out, silt_out))), 
                     file=fileout, row.names=FALSE, col.names=FALSE)
}

} # end loop for site
