make_soil_params <- function(soc_prop,sand,clay,silt,soil_props,rasterf=TRUE,mask=NULL)
{

#http://www-nwp.metoffice.com/~frsurf/ANCIL/view/dev/doc/AncilDoc_Cosby.html
b <- 3.10 + 15.70*clay - 0.3*sand
b_both <- soc_prop * soil_props$b_org + (1-soc_prop) * b
if (rasterf) {values(b_both)[values(mask)>1.5]<-0.0}
#b.data[ice] = 0
#b.lbfc = 1381

sathh <- 0.01 * 10^(2.17 - 1.58*sand - 0.63*clay)
sathh_both <- (sathh)^(1-soc_prop)*(soil_props$sathh_org)^(soc_prop)
if (rasterf) {values(sathh_both)[values(mask)>1.5]<-0.0}
#sathh.data[ice] = 0
#sathh.lbfc = 342

k_s <- 10^(-2.75 - 0.64*clay + 1.26*sand)
k_s_both <- (k_s)^(1-soc_prop)*(soil_props$ks_org)^(soc_prop)
if (rasterf) {values(k_s_both)[values(mask)>1.5]<-0.0}
#k_s.data[ice] = 0
#k_s.lbfc = 333

sm_sat <- 0.505 - 0.037*clay - 0.142*sand
sm_sat_both <- soc_prop * soil_props$sm_sat_org + (1-soc_prop) * sm_sat
if (rasterf) {values(sm_sat_both)[values(mask)>1.5]<-0.0}
#sm_sat.data[ice] = 0
#sm_sat.lbfc = 332

sm_crit <- sm_sat* ((sathh/3.364)^(1/b))
sm_crit_both <- sm_sat_both*(sathh_both/3.364)^(1.0/b_both)
if (rasterf) {values(sm_crit_both)[values(mask)>1.5]<-0.0}
#sm_crit.data[ice] = 0
#sm_crit.lbfc = 330

sm_wilt <- sm_sat* ((sathh/152.9)^(1/b))
sm_wilt_both <- sm_sat_both*(sathh_both/152.9)^(1.0/b_both)
if (rasterf) {values(sm_wilt_both)[values(mask)>1.5]<-0.0}
#sm_wilt.data[ice] = 0
#sm_wilt.lbfc = 329

#http://www-nwp.metoffice.com/~frsurf/ANCIL/view/dev/doc/AncilDoc_SoilThermal.html
hcap <- (1-sm_sat) * (clay*2.373E+6 + sand*2.133E+6 + silt*2.133E+6)
hcap_both <- soc_prop * soil_props$hcap_org + (1-soc_prop) * hcap
if (rasterf) {values(hcap_both)[values(mask)>1.5]<-0.63E+6}
#hcap.data[ice] = 0.63E+6
#hcap.lbfc = 335

hcon = 0.025^sm_sat * 1.16025^((1-sm_sat)*clay) * 1.57025^((1-sm_sat)*sand) * 1.57025^((1-sm_sat)*silt)
hcon_both <- (hcon)^(1-soc_prop)*(soil_props$hcon_org)^(soc_prop)
if (rasterf) {values(hcon_both)[values(mask)>1.5]<-0.265}
#hcon.data[ice] = 0.265
#hcon.lbfc = 336

return(list(b_both=b_both,sathh_both=sathh_both,k_s_both=k_s_both,
   sm_sat_both=sm_sat_both,sm_crit_both=sm_crit_both,
   sm_wilt_both=sm_wilt_both,hcap_both=hcap_both,hcon_both=hcon_both))
}
