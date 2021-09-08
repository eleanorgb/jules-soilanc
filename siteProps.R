siteProps <- function(siteid, soil_thick, soilprofiles=FALSE)
{
  
# concentration of carbon in peat soils ranges 30–70 kg/m3 
# Sapric peat is decomposed peat with a fibre content less than 15%
# Hemic peat is half-decomposed peat with fibre content 15–75%
# Fibric peat is immature peat with fibre content greater than 75%
sand <- NULL
silt <- NULL
clay <- NULL
zz  <-  cumsum(soil_thick)
orgsoil <- NULL
bulkdens <- NULL
soc_obs <- 0.0
depth_change <- 0.0  #top of soil layer

# below from original paper
# if (siteid =="Samoylov") {soc_prop <- 0.520833}  # site dependent
# if (siteid =="Abisko") {soc_prop <- 0.78125}  # site dependent
# if (siteid =="Zackenberg") {soc_prop <- 0.15625}  # site dependent
# if (siteid =="Svalbard_Ny") {soc_prop <- 0.0885}  # site dependent
# above from original paper

# These are the new sites from Angela 
# we need further information to make the soil properties better
# Ive assumed they are peaty for the top 3 m

# Inirida: peatland site on sand soils (Guianan shield), likely not 
# very deep (varying from 30 cm to 1m or so). pH = approx.4.  
# Conductivity = approx. 30-60 microsiemens. 

# Amacayacu:  deeper peat (>2m), it is affected by the amazon river,
# so the peat has probably got a bit more sediment in it 
     
# Mpologoma wetland, Uganda –This is a papyrus peatland.
# Depth of peat 30-150 cm.

# Opala, Democratic Republic of Congo 
  
# South East Asian sites
# Sumatra, April tower site: there is a paper: 
# https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15019
# It is the pristine site, on and ombrotrophic, acidic and nutrient-poor, peat swamp forest. 
# The surface peat type is fibric and the average peat thickness is ~9 ± 1 m 
# in the area surrounding the tower. 
# Surface (0–50 cm) peat bulk density (g/cm3) 	0.08 ± 0.03
# Surface (0–50 cm) peat pH	3.6 ± 0.10	

if (siteid=="Inirida" || siteid == "Amacayacu" || siteid == "Iquitos" ||
       siteid=="Mpologoma" || siteid=="Opala" || siteid=="Sumatra") {
        sand <- 0.3
	silt <- 0.5
	clay <- 0.2
  depth_change <- c(0, 1)
  soc_obs <-c(1000, 1000)
}

#------------------------------------
# these sites all have peat soils in the top 3m
# and currently have non precise definitions of soc and psd
if (siteid=="Siikaneva" || siteid == "Auchencorth"||
       siteid=="Degero" || siteid=="CA_WP1" || siteid=="Iskoras" ||
       siteid=="Kopytkowo" || siteid=="Congo") {
  sand <- 0.3
	silt <- 0.4
	clay <- 0.3
  depth_change <- c(0, 1)
  soc_obs <-c(1000, 1000)
  if (soilprofiles && siteid=="Degero") {
    soilprofile <- read.table("soilprofile/Degero_soil_profile.dat", header=TRUE)
    depth_change <- c(diff(soilprofile$depth)/2,0)+soilprofile$depth
    bd_obs <- as.numeric(soilprofile$bdens)*1000
    soc_obs  <- as.numeric(soilprofile$c_kg_m3)
  }
  if (soilprofiles && siteid=="Siikaneva") {
    soilprofile <- read.table("soilprofile/SiiF.dat", header=TRUE)
    depth_change <- c(diff(soilprofile$depth)/2,0)+soilprofile$depth
    bd_obs <- as.numeric(soilprofile$bdens)*1000
    soc_obs  <- as.numeric(soilprofile$c_kg_m3)
  }
}

if (soilprofiles && siteid=="Merbleue") {
  sand <- 0.92
	silt <- 0.04
	clay <- 0.04
  soilprofile <- read.table("soilprofile/Merbleue_soil_profile.dat", skip=2, header=TRUE)
  depth_change <- soilprofile$min_depth
  bd_obs <- as.numeric(soilprofile$bd_g_cm3)*1000
  soc_obs  <- as.numeric(soilprofile$c_kg_m3)
}

#------------------------------------


#------------------------------------
if (siteid == "ImnavaitFen") {
# http://onlinelibrary.wiley.com/doi/10.1890/ES11-00202.1/full
# thawdepth 60cm, 16 kg/m2 in active layer; 27 kg/m3
# silt underneath
	sand <- 0.2
	silt <- 0.7
	clay <- 0.1
  depth_change <- c(0, 0.34, 0.6)
  soc_obs <- c(40, 13, 10) 
}
#------------------------------------


#------------------------------------
if (siteid == "ImnavaitTussock") {
# http://onlinelibrary.wiley.com/doi/10.1890/ES11-00202.1/full
# thawdepth 70cm, 11 kg/m2 in active layer; 16 kg/m3
# silt underneath
	sand <- 0.2
	silt <- 0.7
	clay <- 0.1
  depth_change <- c(0, 0.15, 0.7)
  soc_obs <- c(40, 13, 10) 
}
#------------------------------------


#------------------------------------
if (siteid == "ImnavaitRidge") {
  # http://onlinelibrary.wiley.com/doi/10.1890/ES11-00202.1/full
  # thawdepth 40cm, 11 kg/m2 in active layer; 28 kg/m3
  # silt underneath
	sand <- 0.2
	silt <- 0.7
	clay <- 0.1
  depth_change <- c(0, 0.051, 0.4)
  soc_obs <- c(40, 26, 10)
}
#------------------------------------


#------------------------------------
if (siteid == "Abiskomire") {
	sand <- 0.1
	silt <- 0.9
	clay <- 0.0
  if (soilprofiles) {
    # Abisko mire site, using Hugelius typical soil profiles
    abisko2 = read.table("soilprofile/Abisko_typical_soil_profiles_Hugelius_20151001.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
    depth_change <- (1:165)/100 - 0.01
    soc_obs <- as.numeric(abisko2[2:166,18])*1000
    bd_obs  <- as.numeric(abisko2[2:166,11])*1000
  } else {
    # Abisko mire site 50cm organic layer 14 kg/m2 below
    depth_change <- c(0, 0.5)
    soc_obs <-c(64, 14)
  }
}
#------------------------------------


#------------------------------------
if (siteid == "Abisko") {
	sand <- 0.1
	silt <- 0.8
	clay <- 0.1
  depth_change <- c(0, 0.5)
  soc_obs <-c(0, 0)
}
#------------------------------------
if (siteid == "Carlow") {
#https://www.biogeosciences.net/10/1675/2013/bg-10-1675-2013-supplement.pdf
#cn ratio = 10
	sand <- 0.55
	silt <- 0.22
	clay <- 0.23
   if (soilprofiles) {
     depth_change <-c(0,0.05,0.1,0.2,0.3,0.4,0.5)
     soc_obs <- c(1.4,1.3,2.7,2.0,0.9,0.6,0.4)  #kg/m2
     soc_obs <- soc_obs/c(0.05,0.05,0.1,0.1,0.1,0.1,0.1)  #kg/m3
     # C in g/kg soil *bulkensity in kg/m3 / 1000.0 = soc_obs in kg/m3
     bd_obs <- soc_obs/c(24,24,25,19,9,11,4)*1000.0 #I think
  } else {
	cat("not coded yet\n")
	browser()
  }
}

if (siteid == "Brasschaat") {
  #Janssens et al., 1998
  sand <- 0.84
	silt <- 0.1
	clay <- 0.06
   if (soilprofiles) {
     depth_change <-c(0,0.02,0.34,0.5,0.9)
     bd_obs <- c(1.21,1.363,1.543,1.549,1.555)*1000.
     soc_obs <- c(2.1,4.89,1.85,2.7,0.48) #kg/m2
     soc_obs <- soc_obs/c(0.02,0.33,0.16,0.4,0.1)
   } else {
 	cat("not coded yet\n")
 	browser()
   }
 }


#------------------------------------
if (siteid == "Hainich") {
	sand <- 0.03
	silt <- 0.38
	clay <- 0.59
   if (soilprofiles) {
	hainich <-read.table("soilprofile/Hainich_soil_org.dat",sep=",",header=TRUE,stringsAsFactors=FALSE,skip=1)
        depth_change <- hainich[,1]
        soc_obs <-  hainich[,3]/( hainich[,2]-hainich[,1]) #convert kg/m2 to kg/m3
 	hainich <-read.table("soilprofile/Hainich_soil.dat",sep=",",header=TRUE,stringsAsFactors=FALSE,skip=1)
 	bd_obs <- hainich$Bulk.density..g.cm.3.[1:7]*1000.  #kg/m3
  } else {
	cat("not coded yet\n")
	browser()
  }
}
#------------------------------------
if (siteid == "Turkeypt") {
#https://www.sciencedirect.com/science/article/pii/S0168192306002188?via%3Dihub
#sandy/sandy loam
#c content t/ha
#0-15 15
#15-35 9
#35-55 6
if (soilprofiles) {
  depth_change=c(0,0.15,0.35)
  soc_obs <- c(15,9,6)/10./c(0.15,0.2,0.2) #convert t/ha to kg/m3
}
sand <- 0.65
silt <- 0.25
clay <- 0.10
}

#------------------------------------
if (siteid == "Seidapeat") {
	sand <- 0.58
	silt <- 0.32
	clay <- 0.10
  # Seida peat. 63kg/m^3 in top 2m! 5 below.
  depth_change <- c(0, 2)
  soc_obs <-c(63, 5)
}
#------------------------------------


#------------------------------------
if (siteid == "Seidamin") {
	sand <- 0.58
	silt <- 0.32
	clay <- 0.10
  # Seida upland tundra. 40kg/m^3 in top 15ish cm. 5 below.
  depth_change <- c(0, 0.15)
  soc_obs <-c(40, 5)
}
#------------------------------------


#------------------------------------
if (siteid == "Kytalyk") {
	sand <- 0.17
	silt <- 0.7
	clay <- 0.13
  if (soilprofiles) {
    # Kytalyk typical soil profiles
    kytalyk2 = read.table("soilprofile/Kytalyk_typical_soil_profiles_Hugelius_20151001.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
    depth_change = (1:110)/100 - 0.01
    soc_obs <- as.numeric(kytalyk2[2:111,18])*1000
    bd_obs  <- as.numeric(kytalyk2[2:111,11])*1000
  } else {
    # Kytalyk carbon below organic layer - 17 kg/m3
    depth_change <- c(0, 0.20)
    soc_obs <-c(77, 17)
  }
}
#------------------------------------


#------------------------------------
if (siteid == "Svalbard_Ny") {
	sand <- 0.17
	silt <- 0.7
	clay <- 0.13
	if (soilprofiles) {
	  # Svalbard_Ny typical profile for barren ground. bc essentially should have mineral properties.
	  depth_change <- (0:77)/100
	  bayelva_barren = read.table("soilprofile/Ny_Alesund_typical_soil_profiles_Hugelius_20151008_barren.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
	  soc_obs <- as.numeric(bayelva_barren[3:80,18])*1000
	  bd_obs  <- as.numeric(bayelva_barren[3:80,11])*1000
  } else {
    # Svalbard_Ny carbon below organic layer - 0 kg/m3, no organic layer
	  depth_change <- c(0, 1.0)
	  soc_obs <-c(8, 8)
	}
}
#------------------------------------

#------------------------------------
if (siteid == "Twitchell") {
	# assume silt loam
	sand <- 0.1
	silt <- 0.7
	clay <- 0.20
	soc_obs = c(30.5,63.4,81.2) # kg/m3
	depth_change = c(0,0.15,0.25)
	bd_obs <- c(88, 277, 402)
}
#------------------------------------

#------------------------------------
if (siteid == "Scottycreek") {
  #silty clayr
	sand <- 0.1
	silt <- 0.45
	clay <- 0.45
	if (soilprofiles) {
	  # Samoylov typical soil profile
    require(gdata)
    
	  data = read.xls("soilprofile/Data_SC_Ecosys.xlsx",
                  sheet="Soil profile bog",header=TRUE,skip=1)
    # C in g/kg soil *bulkensity in kg/m3 / 1000.0 = soc_obs in kg/m3
    bd_obs  = data$g.cm.3
	  soc_obs = data$g.kg.1   #C in g/kg soil
    soc_obs = soc_obs * bd_obs / 1000.0
    depth_change = data$cm/100.0
    bd_obs <- bd_obs[!is.na(soc_obs)] * 1000.
    depth_change <- depth_change[!is.na(soc_obs)]
    soc_obs <- soc_obs[!is.na(soc_obs)]
    soc_obs[bd_obs > 1.0] <- 0.0
  } else {
  cat("not coded yet\n")
  browser()
  }
}
#------------------------------------

#------------------------------------
if (siteid == "Samoylov") {
# https://www.biogeosciences.net/10/3507/2013/bg-10-3507-2013.pdf
	sand <- 0.58
	silt <- 0.32
	clay <- 0.10
	if (soilprofiles) {
	  # Samoylov typical soil profile
	  samoylov2 = read.table("soilprofile/LenaDelta_typical_soil_profiles_Hugelius_20151001.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
	  soc_obs = as.numeric(samoylov2[2:101,18])*1000
	  bd_obs  = as.numeric(samoylov2[2:101,11])*1000
	  depth_change = (1:100)/100 - 0.01
  } else {
	  # Samoylov carbon in top 1m - 25 kg/m3
	  depth_change <- c(0, 0.30)
	  soc_obs <-c(35, 20)
  }
}
#------------------------------------


#------------------------------------
if (siteid == "Zackenberg") {
	sand <- 0.8
	silt <- 0.1
	clay <- 0.1
	if (soilprofiles) {
	  # Zackenberg typical soil profile
    depth_change <- (0:200)/100
    zackenberg = read.table("soilprofile/Zackenberg_typical_soil_profiles_Hugelius_20151008.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
    soc_obs <- as.numeric(zackenberg[2:202,18])*1000
    bd_obs  <- as.numeric(zackenberg[2:202,11])*1000
	} else {
    # Zackenberg carbon below organic layer - 10 kg/m3
    depth_change <- c(0, 0.06)
    soc_obs <-c(70, 10)
  }
}
#------------------------------------


#------------------------------------
if (siteid == "Chersky") {
	# Says 'silty clay'.https://www.biogeosciences.net/13/4219/2016/bg-13-4219-2016.pdf 
	# I think the fractions I've put might be a bit extreme on the clay though.
	sand <- 0.06
	silt <- 0.47
	clay <- 0.47
	if (soilprofiles) {
	  # Organic info sent by Mathias Goeckede. chiara_soilprofile.xlsx combined with BD estimate that he sent separately.
	  depth_change <- c(0,0.11,0.24,0.315,0.39,0.525,0.64,0.78,0.94,1.065,1.165,1.29,1.515,1.725,1.9,2.135,2.41,2.71,2.91,3.065,3.28,3.49,3.7,3.875,4.0,4.115,4.415,4.7,4.85,5.0)
	  soc_obs <-c(75.78,62.0,83.66,15.08,37.58,31.83,36.18,60.75,54.16,52.36,26.85,26.07,28.03,23.67,31.9,17.9,25.39,31.06,19.27,32.46,23.6,9.0,4.7,31.4,9.76,20.62,10.78,25.8,9.4,11.2)
	  bd_obs <- c(0.2,0.2,0.33,0.72,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5)*1000
	} else {
		# organic horizon 0.15-0.20 m, C
		depth_change <- c(0, 0.2)
		soc_obs <-c(26, 3)		
	}
}
#------------------------------------

#------------------------------------
if (siteid == "EML") {
	sand <- 0.4
	silt <- 0.4
	clay <- 0.2
	# organic horizon 0.45-0.65 m (55-69 kg/m2 in top 1m)
	depth_change <- c(0, 0.45, 1.0)
	soc_obs <-c(70, 30, 5)
}
#------------------------------------

#------------------------------------
if (siteid == "DUKE") {
	sand <- 0.49
	silt <- 0.42
	clay <- 0.09
}
#------------------------------------

#------------------------------------
if (siteid == "ORNL") {
	sand <- 0.21
	silt <- 0.55
	clay <- 0.25
}
#------------------------------------


#------------------------------------
if (siteid == "Lompolojankka") {
  # no references for these but it is a peat soil so not very much mineral
	sand <- 0.20
	silt <- 0.55
	clay <- 0.25
	if (soilprofiles) {
		# upper layers are from Metzger.
    # https://www.biogeosciences.net/12/125/2015/bg-12-125-2015.html
		# Lower layers are mathijssen. 
    # I can find several papers by mathijssen on google scholar 
    # but can't find the actual data in any of them!
		depth_change <- c(0, 0.1, 0.3, (5:22)/10)
		lompo = read.table("soilprofile/Lompolo_soilcarb_mathijssen.csv",sep=",",header=TRUE, stringsAsFactors=FALSE)
		soc_obs <- c(24, 30, 51, as.numeric(lompo$g.C.cm.3)*1000)
		bd_obs  <- c(60, 60, 100, as.numeric(lompo$Peat.density..g.cm.3.)*1000)		
	} else {
    # Lompolojankka peat. 24 kg/m3 top 10 cm, 30 next 20cm and 51 rest
    # try with 51 in top 3m!
    depth_change <- c(0, 3)
    soc_obs <-c(51, 5)
	}
}
#------------------------------------


#------------------------------------
if (siteid == "PleistocenePark") {
  # http://onlinelibrary.wiley.com/doi/10.1002/2016GL068874/full
  # Yedoma - There is little humus but substantial labile C - mienral soils
  # The average C content of yedoma is ∼40 kg m−3
  # lots of ice wedges
  # 2.5 % organic carbon pretty uniform profile
  # http://onlinelibrary.wiley.com/doi/10.1029/2006GL027484/abstract
  # https://www.biogeosciences.net/13/4219/2016/bg-13-4219-2016.pdf for cherskiy
  # says organic layer 15-20cm on top of silty clay not exactly the same site?
	sand <- 0.20
	silt <- 0.55
	clay <- 0.25
  depth_change <- c(0, 3)
  soc_obs <-c(10, 10)
}
#------------------------------------


#------------------------------------
if (siteid == "Spasskaya_Pad") {
# http://www.tandfonline.com/doi/pdf/10.1080/00380768.2013.772495?needAccess=true
# sandy loam or sandy clay loam
	sand <- 0.65
	silt <- 0.15
	clay <- 0.20
  # 8 cm organic layer
  depth_change <- c(0, 0.08)
  soc_obs <-c(64, 9)
}
#------------------------------------

#------------------------------------

ltops = zz
lbots = c(0,zz)[1:length(zz)]

# Generic function to work out soilcarb_layers
getvals <- function(lbot,ltop,data_obs) {
  ndep = length(data_obs)
  cslbot = (depth_change)
  csltop = c(depth_change[2:ndep],max(c(zz,cslbot+0.1)))
  cs = data_obs*(csltop - cslbot)
  cstot = 0

  for(i in 1:ndep) {
    if(cslbot[i] <= ltop & csltop[i] >= lbot) {
      cstmp = cs[i]
      if (cslbot[i] < lbot) {
        cstmp = cstmp - (lbot - cslbot[i])/(csltop[i] - cslbot[i])*cs[i]
      }
      if (csltop[i] > ltop) {
        #browser("fer")
        cstmp = cstmp - (csltop[i] - ltop)/(csltop[i] - cslbot[i])*cs[i]
      }
      cstot = cstot + cstmp
    }
  }
  return(cstot)
}

orgsoil <- apply(cbind(lbots,ltops),1,function(x) 
                     getvals(x[1],x[2],soc_obs))/soil_thick
if(length(orgsoil)>1 && orgsoil[2]==0)  {
   orgsoil[2] <- orgsoil[3]
                   }
if(orgsoil[1]==0)  {
      orgsoil[1] <- orgsoil[2]
                      }
if( exists("bd_obs") ) {
  bulkdens <- apply(cbind(lbots,ltops),1,function(x) 
                     getvals(x[1],x[2],bd_obs))/soil_thick
  if(bulkdens[1]==0)  {
      bulkdens[1] <- bulkdens[2]
                      }
}
#------------------------------------

#------------------------------------
# error trapping
if (is.null(sand)) {
	stop("need sand, silt and clay for sites")
}
if (is.null(soc_obs)) {
	stop("need observed soc for sites")
}
if (abs(sand+silt+clay-1) > 0.001) {
	stop("sum psd !=1")
}
# if (length(soc_obs) > 3 ) {
#  stop("not coded for this many observed changes in soc")
#}
if (length(soc_obs)==1 && soc_obs ==0.0 ) {
	readline("press enter if you do not want to include organic matter")
}
#------------------------------------


#------------------------------------
# levels for changing in soil properties of prescribed organic matter
dlev1<-0.3
dlev2<-1.0
if (siteid == "Seidapeat") { 
   dlev1 <- 1.8
   dlev2 <- 3.0
}
#------------------------------------

return(list(clay=clay, sand=sand, silt=silt, dlev1=dlev1, dlev2=dlev2, 
	              orgsoil=orgsoil, bulkdens=bulkdens))
}
