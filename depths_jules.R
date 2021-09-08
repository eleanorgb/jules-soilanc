################soil levels = middle of layer
depths_jules <- function(simname)
{
  if (simname=="1cmto4m") {
    #1st layer is actually 0.01 but that breaks the code
     soil_thick <- rep(0.01,400)
                         }
  if (simname=="20layers") {
    #1st layer is actually 0.01 but that breaks the code
     soil_thick <- c(0.05,0.03,0.08,0.13,0.19,0.28,0.35,0.40,0.45,0.50,0.55,0.60,
              0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00)
                         }

 if (simname=="14layers20") {
   # 0.05 * n^(0.75)
   soil_thick<-c(0.05,0.08408964,0.11397535,0.14142136,0.16718508,0.19168293,
         0.21517585,0.23784142,0.25980762,0.28117066,0.30200527,
         0.32237098,0.34231625,0.36188121,0.8,0.8,0.8,0.8,0.8,0.8)
                       }
                       
if (simname=="14layers") {
  # 0.05 * n^(0.75)
  soil_thick<-c(0.05,0.08408964,0.11397535,0.14142136,0.16718508,0.19168293,
        0.21517585,0.23784142,0.25980762,0.28117066,0.30200527,
        0.32237098,0.34231625,0.36188121)
                      }

   depths <- NULL  #these depths are for the middle of each layer
   # these are not strictly right.
   depths[1]<-soil_thick[1]/2.0
   for (ijk in 2:length(soil_thick)) {
      depths[ijk]=depths[ijk-1]+soil_thick[ijk-1]/2.0+soil_thick[ijk]/2.0
                                     }
  return(list(depths=depths,soil_thick=soil_thick))
}
