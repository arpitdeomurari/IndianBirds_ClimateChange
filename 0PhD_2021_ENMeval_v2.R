utils::memory.limit(size = 60817408)
options(java.parameters = "-XX:PermSize=512M -XX:MaxPermSize=2048m -XX:MaxMetaspaceSize=2048m -Xms4096m -Xmx50024m" )
library("rJava")
library("rgdal")
library("ENMeval")
library("raster")
library("dismo")
library("rgeos")
library("sp")
library("tictoc")

library('data.table')
library("stringr")
library("usdm")
library('spThin')
library("dplyr")
library("ggplot2")
library("virtualspecies")
library("dismotools")
library("ecospat")
library("PresenceAbsence")
library("ntbox")
library("ENMGadgets")
library("enmSdm")
library("biomod2")
library("sf")
library("sqldf")
library("rmaxent")
library("adehabitatHR")
library("parallel")
library("CoordinateCleaner")
library("tmap")
library("tmaptools")
library("tibble")
# library("terra")

source("E:/PHD_DATA/Scripts/Appendix2_prepPara.R")

########Set Global Settings

WorkDir = 'G:/PHD_Output'
if(file.exists(WorkDir) == FALSE){
   dir.create(WorkDir)
}
DataDir = 'E:/PHD_DATA'
tmpdir_name <- "E:/RTEMP/"
Birdlife_Data="E:/PHD_DATA/Data/SHAPE/ISR_BoTW/"
CL_BL_Match <-read.csv(file="E:/PHD_DATA/Data/CL_BL_Match.csv", header=TRUE, sep=",", fileEncoding="UTF-8-BOM")

########Set Global Settings


jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")

ncore <- 3/4*detectCores()
# beginCluster(ncore)

options(rasterTmpTime = 12)

if(file.exists(tmpdir_name) == FALSE){
   dir.create(tmpdir_name)
}

rasterOptions(datatype = "FLT4S", 
              progress = "", 
              tmpdir = tmpdir_name, 
              tmptime = 12, 
              timer = FALSE,
              tolerance = 0.5,
              chunksize = 3e+09,
              maxmemory = 55e+09,
              memfrac = 0.8,
              todisk=TRUE)
##Common GIS Data
isrf <- "E:/PHD_DATA/Data/SHAPE/Ind_region.shp"
isr <- st_read(dsn=isrf,layer="Ind_region")

indf <- "E:/PHD_DATA/Data/SHAPE/ind.shp"
ind <- st_read(dsn=indf,layer="ind")



##################################



OrdsRemain =c()

# Threat_Ord_1=c("Fregatidae","Heliornithidae","Procellariidae","Megapodiidae","Otididae","Hydrobatidae","Locustellidae","Indicatoridae","Fringillidae","Pteroclidae","Emberizidae","Vireonidae","Haematopodidae","Tytonidae","Podargidae")
# Threat_Ord_2=c("Pnoepygidae","Certhiidae","Turdidae","Aegithalidae","Pellorneidae","Glareolidae","Caprimulgidae","Pittidae","Sylviidae","Timaliidae","Trogonidae","Gruidae","Falconidae","Laridae")
# Threat_Ord_3=c("Sittidae","Burhinidae","Strigidae","Phylloscopidae","Alaudidae","Leiothrichidae","Phoenicopteridae","Picidae","Paridae","Phasianidae","Campephagidae","Scolopacidae","Muscicapidae","Anatidae","Pelecanidae","Bucerotidae","Vangidae","Apodidae")
# Threat_Ord_4=c("Acrocephalidae","Chloropseidae","Podicipedidae","Ploceidae","Accipitridae","Dicaeidae","Rallidae","Charadriidae","Passeridae","Motacillidae","Columbidae","Rhipiduridae","Hirundinidae")
# Threat_Ord_5=c("Ciconiidae","Estrildidae","Cuculidae","Nectariniidae","Pycnonotidae","Alcedinidae","Sturnidae","Cisticolidae","Psittaculidae","Corvidae","Threskiornithidae","Aegithinidae","Ardeidae","Megalaimidae","Anhingidae","Dicruridae")
# 
Ord_1=c("Tytonidae","Podargidae")
Ord_2=c("Prunellidae","Pnoepygidae","Certhiidae","Ibidorhynchidae","Turdidae","Troglodytidae","Aegithalidae","Turnicidae","Pellorneidae","Glareolidae","Caprimulgidae","Cinclidae","Pittidae","Tichodromidae","Sylviidae","Timaliidae","Trogonidae","Gruidae","Falconidae","Laridae")
Ord_3=c("Alaudidae","Rostratulidae","Leiothrichidae","Phoenicopteridae","Picidae","Paridae","Phasianidae","Campephagidae","Hemiprocnidae","Scolopacidae","Muscicapidae","Anatidae","Pelecanidae","Bucerotidae","Vangidae","Apodidae")
Ord_4=c("Acrocephalidae","Zosteropidae","Pandionidae","Chloropseidae","Podicipedidae","Ploceidae","Laniidae","Accipitridae","Dicaeidae","Irenidae","Rallidae","Artamidae","Charadriidae","Passeridae","Motacillidae","Columbidae","Rhipiduridae","Stenostiridae","Hirundinidae","Monarchidae")
Ord_5=c("Ciconiidae","Coraciidae","Estrildidae","Cuculidae","Oriolidae","Nectariniidae","Jacanidae","Pycnonotidae","Alcedinidae","Sturnidae","Cisticolidae","Psittaculidae","Recurvirostridae","Meropidae","Corvidae","Threskiornithidae","Aegithinidae","Ardeidae","Megalaimidae","Anhingidae","Dicruridae","Upupidae","Phalacrocoracidae")



ords = Ord_3


for(orders in ords)
{
   
   setwd(WorkDir)
   
   dir.create("Finals")
   dir.create("Finals/45_50")
   dir.create("Finals/45_70")
   dir.create("Finals/85_50")
   dir.create("Finals/85_70")
   dir.create("Finals/2050")
   dir.create("Finals/2070")
   dir.create("Finals/Current")
   dir.create("Finals/LGM")
   dir.create("Finals/MID")
   dir.create("Finals/ClimRefugia")
   
   wd <- getwd()
   
   dir.create(paste(orders,sep=''))
   wd=paste(WorkDir,"/",orders,sep='')
   setwd(wd)
   
   #Start Log file
   #sink(file = paste(WorkDir,"/",orders,"_Log.txt",sep=''), append = TRUE, type = c("output"), split = FALSE)
   
   cat('\n',"##############################: ",orders," :##############################",'\n')
   
   data <-read.csv(file=paste(DataDir,"/Data/0FamilyWise/",orders,".csv",sep=''), header=TRUE, sep=",", fileEncoding="UTF-8-BOM")
   table(data$SN)
   especies <- unique(data$SN)
   #Sort species data based on occurence records
   write.csv(table(data$SN),paste(wd, "/", orders ,"_Frequency.csv", sep="" ))
   sp <-read.table(file=paste(wd, "/", orders ,"_Frequency.csv", sep="" ), header=TRUE, sep=",")
   sp.sort <- sp[order(sp$Var1) , ]
   write.csv(sp.sort,paste(wd, "/", orders ,"_Frequency_sort.csv", sep="" ))
   cat('\n',"##########################################################################################",'\n')
   
   
   for(especie in sp.sort$Var1)
   {
      wd=paste(WorkDir,"/",orders,sep='')
      setwd(wd)
      
      
      template <- stack('E:/PHD_DATA/Data/Topo1km/ele_Elevation.tif')
      biaslayer1 <- stack('E:/PHD_DATA/Data/MaxentBias_Feb2021.tif')
      NullLayer <- stack('E:/PHD_DATA/Data/ISR_Null.tif')
      DistLayer <- stack('E:/PHD_DATA/Data/DistToCoast.tif')
      names(DistLayer)<-"DistToCoast"
      stk_Current1<- stack('E:/PHD_DATA/Data/GRDs1KM/Current.grd')
      # stk_Current1 <- stack(stk_Current1,DistLayer)
      BL_SN_Name <- sqldf(sprintf("select New_BL_SN from CL_BL_Match where CLE2019_SP_SN  = '%s'",especie))
      
      
      if (file.exists(paste(Birdlife_Data, especie ,".shp", sep="" ))) {
         #Check file if it exists
         BL_f <- paste(Birdlife_Data, BL_SN_Name ,".shp", sep="" )
         BL_Map <- st_read(dsn=BL_f,layer=especie)
      }
      
      
      dir.create(paste(especie,sep=''))
      wd=paste(wd,"/",especie,sep='')
      setwd(wd)
      cat('\n',"##############################: ",especie," :##############################",'\n')
      #setwd(WorkDir)
      
      if (file.exists(paste(wd, "/", especie ,"_Processed", sep="" )))
      {
         cat('\n',"Alread Processed",especie,'\n')  
         
      }
      
      else {
         
         pres.data <- data[data$SN == especie, c("SN","longitude", "latitude")]
         pres.data.or <- pres.data
         write.csv(pres.data,paste(wd, "/", especie ,"_InputPresence.csv", sep="" ))
         
         stat.pres.data.input <-nrow(pres.data)
         
         pres.data.clean = pres.data
         
         if (nrow(pres.data) >= 1)
         {
            ###Outlier detection
            tryCatch( pres.data <- elimCellDups(pres.data[,c("longitude", "latitude")], stk_Current1, longLat=c(1, 2)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
            pres.data.clean = pres.data
            
            reso1= 0.0083
            
            if (nrow(pres.data) > 20000 ) {
               DistThin=5 
               ThinRep=100 
               ThinFiles=5
            }
            
            if (nrow(pres.data) < 20000 & nrow(pres.data) >= 10000 ) {
               DistThin=3 
               ThinRep=100 
               ThinFiles=5
            }
            
            if (nrow(pres.data) < 10000 & nrow(pres.data) >= 1000 ) {
               DistThin=3 
               ThinRep=100 
               ThinFiles=5
            }
            
            if (nrow(pres.data) < 1000 & nrow(pres.data) >= 50 ) {
               DistThin=2 
               ThinRep=100 
               ThinFiles=5
            }
            
            
            if (nrow(pres.data) < 50 ) {
               DistThin=1
               ThinRep=500 
               ThinFiles=5
            }
            
            reso = reso1 * DistThin
            
            pres.envdata = extract(stk_Current1, pres.data[,c("longitude", "latitude")])
            pres.data <- bind_cols(data.frame(pres.data), data.frame(pres.envdata))
            pres.data <- pres.data[complete.cases(pres.data),]
            pres.data <- pres.data[ -c(4:33) ]
            pres.data.clean <- pres.data
            
            if (nrow(pres.data) > 1)
            {
               pres.data["SN"] <- especie
               
               ccc = pres.data
               ccc <- cc_outl(pres.data,lon="longitude",lat="latitude" ,species="SN",thinning = TRUE, thinning_res = reso,min_occs=1  )
               
               
               pres.data.clean = ccc
               
               write.csv(pres.data.clean,paste(wd, "/", especie ,"_CleanedPresence.csv", sep="" ))
               
               
               cat("Data Thinning with DistThin = ")
               cat(DistThin)
               cat("\n")
               
               if (DistThin != 1)
               {
                  df <-pres.data.clean[order(pres.data.clean$longitude,pres.data.clean$latitude),]
                  
                  nos = as.integer(nrow(df)/1000) +1
                  
                  for( i in seq(nos))
                  {
                     smallDF <- df[((i-1)*1000+1):(min(nrow(df), i*1000)), ]
                     spThin::thin(smallDF,
                                  lat.col = "latitude",
                                  long.col = "longitude",
                                  spec.col = "SN",
                                  thin.par = DistThin,
                                  reps = ThinRep,
                                  locs.thinned.list.return = FALSE,
                                  write.files = TRUE,
                                  max.files = ThinFiles,
                                  out.dir = paste(wd, "/temp", sep="" ),
                                  out.base = paste(especie , sep="" ),
                                  write.log.file = TRUE,
                                  log.file = "spatial_thin_log.txt",
                                  verbose = TRUE)
                     
                     dfTemp <-read.csv(file=paste(wd, "/temp/",especie ,"_thin1.csv", sep="" ), header=TRUE, sep=",", fileEncoding="UTF-8-BOM")
                     files <- list.files(paste(wd, "/temp/", sep="" ), full.names = T, pattern = "*")
                     file.remove(files)
                     if (i==1)
                     { 
                        dfMain = dfTemp
                     }
                     else
                     {
                        dfMain <- rbind(dfMain, dfTemp)
                     }
                     
                  }
                  dir.create(paste("thinned",sep=''))
                  keeps <- c("latitude","longitude","SN")
                  dfMain = dfMain[keeps]
                  dfMain = dfMain[c(3,2,1)]
                  write.csv(dfMain,paste(wd, "/thinned/", especie ,"_thin1.csv", sep="" )) 
               }
               
               
               
               else { 
                  spThin::thin(  pres.data.clean,
                                 lat.col = "latitude",
                                 long.col = "longitude",
                                 spec.col = "SN",
                                 thin.par = DistThin,
                                 reps = ThinRep,
                                 locs.thinned.list.return = FALSE,
                                 write.files = TRUE,
                                 max.files = ThinFiles,
                                 out.dir = paste(wd, "/thinned", sep="" ),
                                 out.base = paste(especie , sep="" ),
                                 write.log.file = TRUE,
                                 log.file = "spatial_thin_log.txt",
                                 verbose = TRUE)
               }
               
            }
            
            if(file.exists(paste(wd, "/thinned/",especie ,"_thin1.csv", sep="" )) == TRUE) 
            {
               pres.data.clean <-read.csv(file=paste(wd, "/thinned/",especie ,"_thin1.csv", sep="" ), header=TRUE, sep=",", fileEncoding="UTF-8-BOM")
               keeps <- c("latitude","longitude","SN")
               pres.data.clean = pres.data.clean[keeps]
               pres.data.clean = pres.data.clean[c(3,2,1)]
               pres.data2 = pres.data.clean
            }  
            pres.data2 = pres.data.clean
         }
         
         
         if (nrow(pres.data.clean) >= 20) {
            
            # Define spatial projection 
            coordinates(pres.data2) <- ~longitude + latitude
            #Create MCP @ 99.5 %
            mcp_est90 <- mcp(pres.data2, percent = 100)
            
            pres.data.clean.spdf <- pres.data.clean
            
            coordinates(pres.data.clean.spdf) <- ~longitude + latitude
            
            #Buffer MCP
            b <- gBuffer(mcp_est90, byid=FALSE, id=NULL, width=2.0, quadsegs=5, capStyle="ROUND",
                         joinStyle="ROUND", mitreLimit=1.0)
            b2 <- as(b, "SpatialPolygonsDataFrame")
            mcp22 <- as(mcp_est90, "SpatialPolygonsDataFrame")
            writeOGR(obj=b2, dsn=paste(wd, "/", especie ,"_Buffer.shp", sep="" ), layer=paste( especie ,"_Buffer", sep="" ), driver="ESRI Shapefile")
            
            writeOGR(obj=mcp22, dsn=paste(wd, "/", especie ,"_MCP.shp", sep="" ), layer=paste(especie ,"_MCP", sep="" ), driver="ESRI Shapefile")
            
            biaslayer <- crop(biaslayer1, b)
            biaslayer <- mask(biaslayer, b)
            stk_Current <- crop(stk_Current1, b)
            stk_Current <- mask(stk_Current, b)
            
            absence <- randomPoints(biaslayer, n = 10050, prob = T)
            abs.data <- as.data.frame(absence)
            names(abs.data)[1]<-paste("longitude")
            names(abs.data)[2]<-paste("latitude")
            
            
            write.csv(abs.data,paste(wd, "/", especie ,"_CleanedInputAbsence.csv", sep="" ))
            abs.data.clean.spdf <- abs.data
            coordinates(abs.data.clean.spdf) <- ~longitude + latitude
            
            dir.create(paste(wd,"/jpegMAPS",sep=''))
            cat("Printing Maps...")
            cat("\n")
            
            map.CleanedLoc <- 
               tm_shape(template,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Elevation") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.7, alpha = 0.7) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black",border.alpha = 0.7, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(mcp_est90) +
               tm_borders(col="red") +
               tm_add_legend('line', col = "red", title="MCP 95%") +
               tm_shape(pres.data.clean.spdf) +
               tm_dots(size = 0.1, shape = 21, col = "blue") +
               tm_graticules() +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nPresence: ",nrow(pres.data.clean),sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_credits("test", size = 0.7, position = c(0.01, 0.01)) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.CleanedBG <- 
               tm_shape(template,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Elevation") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.7, alpha = 0.7) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black",border.alpha = 0.7, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(mcp_est90) +
               tm_borders(col="red") +
               tm_add_legend('line', col = "red", title="MCP 95%") +
               tm_shape(abs.data.clean.spdf) +
               tm_dots(size = 0.1, shape = 21, col = "black") +
               # tm_add_legend('symbol', col = "black", title="Presence") +
               tm_graticules() +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nBackground: ",nrow(abs.data),sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_credits("test", size = 0.7, position = c(0.01, 0.01)) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            
            
            # map.PA<-tmap_arrange(map.CleanedLoc, map.CleanedBG,outer.margins = 0.02)
            
            # tmap_save( tm=map.PA,filename=paste(wd, "/jpegMAPS/", especie ,"_PA.jpg", sep="" ),width=4036, height=2184)
            tmap_save( tm=map.CleanedLoc,filename=paste(wd, "/jpegMAPS/", especie ,"_Presence.jpg", sep="" ),)
            tmap_save( tm=map.CleanedBG,filename=paste(wd, "/jpegMAPS/", especie ,"_Absence.jpg", sep="" ),)
            
            
            ################################Maxent Global 1######################################
            cat("Defult Maxent Model")
            cat("\n")
            # Creates a folder to store the results
            dir.create('MaxEnt_Default')
            # Builds a default Maxent model
            stat.time.maxent.default<-system.time(  xm <- maxent(stk_Current, pres.data.clean[,c("longitude", "latitude")], a=abs.data ,args=c("doclamp=TRUE","responsecurves=TRUE","jackknife=TRUE","maximumiterations=5000","threads=12"), path=paste(wd, "/MaxEnt_Default", sep="" )))
            
            write.csv(xm@results,paste(wd, "/", especie ,"_MaxEnt_Default_Results.csv", sep="" ))
            write.csv(maxent_get_results(xm, 'contribution'),paste(wd, "/", especie ,"_MaxEnt_Default_VarContri.csv", sep="" ))
            write.csv(maxent_get_results(xm, 'importance'),paste(wd, "/", especie ,"_MaxEnt_Default_VarImp.csv", sep="" ))
            
            important.layers <-read.csv(file=paste(wd, "/", especie ,"_MaxEnt_Default_VarImp.csv", sep=""), header=TRUE, sep=",", fileEncoding="UTF-8-BOM")
            names(important.layers)[1]<-paste("layer")
            names(important.layers)[2]<-paste("importance")
            
            contribution.layers <-read.csv(file=paste(wd, "/", especie ,"_MaxEnt_Default_VarContri.csv", sep=""), header=TRUE, sep=",", fileEncoding="UTF-8-BOM")
            names(contribution.layers)[1]<-paste("layer")
            names(contribution.layers)[2]<-paste("contribution")
            
            contribution.layers$layer <- gsub('.contribution', '', contribution.layers$layer)
            
            total <- merge(important.layers,contribution.layers,by=c("layer","layer"))
            
            remove.layers=as.data.frame(total[total$importance == '0' & total$contribution == '0',])
            
            # stk_Current2 <- dropLayer(stk_Current, remove.layers$layer)
            # 
            # stk_Current <- stack(stk_Current2)
            
            
            ################################Maxent Global 2######################################
            
            
            # #VIF check
            # v1 <- vifstep(stk_Current, th=10)
            # 
            # stk_Current <- exclude(stk_Current, v1)
            
            ################################ENMEval######################################         
            cat("ENMEval for Best Model Tuning")
            cat("\n")
            
            stat.time.enmeval<-system.time( eval.results <- ENMevaluate(occ=pres.data.clean[,c("longitude", "latitude")], env=stk_Current, a=abs.data, RMvalues=c(0.50, 1.00, 1.50, 2.00, 2.50, 3.00, 3.50, 4.00, 4.50, 5),fc=c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),method = "randomkfold", kfolds = 5,rasterPreds=TRUE, algorithm='maxent.jar',bin.output = TRUE, clamp = TRUE,parallel = TRUE,numCores = ncore,progbar = F))
            
            write.csv(eval.results@results, file = paste(wd, "/", especie ,"_ENMEval_Results.csv", sep="" ))
            
            jpeg(file=paste(especie,"_EnmEval.jpg",sep=''),width = 2018 , height = 2184, units = "px", pointsize = 12,
                 quality = 100, res=300)
            par(mfrow=c(2,2))
            eval.plot(eval.results@results, legend.position="topright")
            eval.plot(eval.results@results, "avg.test.AUC",variance="var.test.AUC")
            eval.plot(eval.results@results, "avg.diff.AUC", variance="var.diff.AUC")
            eval.plot(eval.results@results, "avg.test.orMTP", variance="var.test.orMTP")
            mtext(paste("EnmEval Results: ",stringr::str_replace(especie,"_"," "), sep=''), side = 3, line = -21, outer = TRUE)
            dev.off()
            
            
            
            
            
            #Use the setting from the most parsimonious model to build a new Maxent model.
            aicmods <- which(eval.results@results$AICc == min(na.omit(eval.results@results$AICc)))[1] # AIC model
            aicmods <- eval.results@results[aicmods,]
            FC_best <- as.character(aicmods$features[1]) # Get FCs from the AIC model
            rm_best <- aicmods$rm # Get RM from the AIC model
            
            
            
            ########################BEST 10###########################
            maxent10.args <- prepPara(userfeatures=FC_best,betamultiplier=rm_best,threads = ncore)
            cat("10 CV of Best Models")
            cat("\n")
            dir.create('ENMeval10')
            stat.time.mx_best10<-system.time( maxent10_best <- maxent(stk_Current, pres.data.clean[,c("longitude", "latitude")], a=abs.data,args=maxent10.args,
                                                                      path=paste(wd, "/ENMeval10", sep="" ), overight=T))
            write.csv(maxent10_best@results,paste(wd, "/", especie ,"_Maxent10Best_Results.csv", sep="" ))
            
            eval.maxent10 <- as.data.frame(maxent10_best@results)
            
            eval_maxent10_tr <-read.csv(file=paste(wd, "/", especie ,"_Maxent10Best_Results.csv", sep="" ), header=TRUE, sep=",", fileEncoding="UTF-8-BOM")
            
            names(eval_maxent10_tr) <- c("X","cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9","avg")
            
            
            thre_criteria ='Maximum.training.sensitivity.plus.specificity.logistic.threshold'
            
            tr <- sqldf(sprintf("select avg from eval_maxent10_tr where X  = '%s'",thre_criteria))
            
            
            ########################PREDICT CURRENT###########################
            cat("Model Evaluation")
            cat("\n")
            dir.create(paste(wd,"/maps",sep=''))
            beginCluster(ncore)
            
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_Current,return_lfx=FALSE)
               if (i==1)	{	pred.Current.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.Current.10cv <- stack(pred.Current.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.Current.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            dir.create(paste(wd,"/maps",sep=''))
            dir.create(paste(wd,"/GRD",sep=''))
            
            SplimitFactor <- limiting(stk_Current, maxent10_best@models[[1]])
            writeRaster(SplimitFactor, filename=paste(wd, "/GRD/", especie ,"_SplimitFactor.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            writeRaster(SplimitFactor, filename=paste(wd, "/maps/", especie ,"_SplimitFactor.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            
            # for (i in 1:10)
            # {
            #   pres.temp <-extract(pred.Current.10cv[[i]],pres.data.clean[,c("longitude", "latitude")],na.rm=T)
            #   bg.temp <- extract(pred.Current.10cv[[i]],abs.data[,c("longitude", "latitude")],na.rm=T)
            #   # cbi_enmSdm.temp <- contBoyce(pres.temp, bg.temp)
            #   # cbi_enmSdm.temp <- ecospat::ecospat.boyce(pred.Current.10cv[[i]], pres.data.clean[,c("longitude", "latitude")])
            # 
            #   tryCatch( {
            #     cbi_enmSdm.temp <- contBoyce(pres.temp, bg.temp)
            #     },
            #     error = function(err)
            #     {
            #     cbi_enmSdm.temp <- ecospat::ecospat.boyce(pred.Current.10cv[[i]], pres.data.clean[,c("longitude", "latitude")])
            #     return(cbi_enmSdm.temp)
            #     })
            # 
            #   if (i ==1) {(cbi_enmSdm=cbi_enmSdm.temp)}
            #   if (i >1) {cbi_enmSdm <- cbind(cbi_enmSdm,cbi_enmSdm.temp)}
            # 
            #   tryCatch( {
            #     ic.temp <- ic(pred.Current.10cv[[i]], pres.data.clean[,c("longitude", "latitude")], maxent10_best@models[[i]])
            #   },
            #   error = function(err)
            #   {
            #     ic.temp <- calc.aicc(get.params(maxent10_best@models[[i]]),pres.data.clean[,c("longitude", "latitude")],pred.Current.10cv[[i]])
            #     return(ic.temp)
            #   })
            # 
            #   if (i ==1) {(ic=ic.temp$AICc)}
            #   if (i >1) {ic <- rbind(ic,ic.temp$AICc)}
            # 
            # }
            # 
            for (i in 1:10)
            {
               pres.temp <-extract(pred.Current.10cv[[i]],pres.data.clean[,c("longitude", "latitude")],na.rm=TRUE)
               bg.temp <- extract(pred.Current.10cv[[i]],abs.data[,c("longitude", "latitude")],na.rm=TRUE)
               cbi_enmSdm.temp <- enmSdm::contBoyce2x(pres.temp, bg.temp, numClasses = 10, upweightTails=TRUE,autoWindow=FALSE, na.rm = TRUE)
               if (i ==1) {(cbi_enmSdm=cbi_enmSdm.temp)}
               if (i >1) {cbi_enmSdm <- cbind(cbi_enmSdm,cbi_enmSdm.temp)}
               
            }
            
            endCluster()
            
            
            
            eval.maxent10 <- as.data.frame(maxent10_best@results)
            
            
            maxent10_best.AvgTSS <- maxtss2(paste(wd, "/ENMeval10", sep="" ))
            
            write.csv(maxent10_best.AvgTSS$max_tss,paste(wd, "/", especie ,"_Maxent10Best_TSS.csv", sep="" ))
            
            eval.T <- as.data.frame(rbind(maxent10_best.AvgTSS$max_tss[1:10,1], maxent10_best.AvgTSS$max_tss[1:10,2]))
            row.names(eval.T)<-c("TSS","TSS.thr")
            colnames(eval.T) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            colnames(eval.maxent10) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9","species_average")
            colnames(cbi_enmSdm) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            row.names(cbi_enmSdm)<-c("CBI")
            
            eval.maxent10<-within(eval.maxent10, rm(species_average))
            
            #evalfull.TSS <- rbind(eval.maxent10,eval.T,cbi_enmSdm,t(ic))
            
            evalfull.TSS <- rbind(eval.maxent10,eval.T,cbi_enmSdm)
            
            foo <- function(x, digits = 3L, ...) {
               x <- c(x, recursive = TRUE, use.names = FALSE)
               res <- c(mean = mean(x, ...), sd = sd(x, ...),
                        median = median(x, ...), max = max(x, ...))
               round(res, digits)
            }
            
            evalfull.TSS <-cbind(evalfull.TSS, t(apply(evalfull.TSS, 1, foo, na.rm = TRUE)))    
            
            write.csv(evalfull.TSS,paste(wd, "/", especie ,"_Maxent10CV_Eval.csv", sep="" ))
            
            evalfull.TSS2<-rownames_to_column(evalfull.TSS, var = "index")  
            
            # AICc <- subset(evalfull.TSS2, index=="AICc",select=1:11)
            Training.AUC<- subset(evalfull.TSS2, index=="Training.AUC",select=1:11)
            Test.AUC<- subset(evalfull.TSS2, index=="Test.AUC",select=1:11)
            TSS<- subset(evalfull.TSS2, index=="TSS",select=1:11)
            TSS.thr<- subset(evalfull.TSS2, index=="TSS.thr",select=1:11)
            CBI<- subset(evalfull.TSS2, index=="CBI",select=1:11)
            
            contribution<- evalfull.TSS2[grepl('contribution', evalfull.TSS2$index), ]
            contribution$Species=stringr::str_replace(especie,"_"," ")
            contribution$Family=orders
            colnames(contribution) <- c("Variable","cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9","mean","sd","median","max","Species","Family")
            write.csv(contribution,paste(wd, "/", especie ,"_10CV_VarContri_Master.csv", sep="" ))
            
            
            
            importance<- evalfull.TSS2[grepl('importance', evalfull.TSS2$index), ]
            importance$Species=stringr::str_replace(especie,"_"," ")
            importance$Family=orders
            colnames(importance) <- c("Variable","cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9","mean","sd","median","max","Species","Family")
            write.csv(importance,paste(wd, "/", especie ,"_10CV_VarImp_Master.csv", sep="" ))
            
            # ModelEval.temp <- rbind(Training.AUC,Test.AUC,TSS,TSS.thr,CBI,AIC,BIC,AICc)
            
            
            
            ModelEval.temp <- rbind(Training.AUC,Test.AUC,TSS,TSS.thr,CBI)
            ModelEval.temp <- remove_rownames(ModelEval.temp)
            ModelEval.t <- column_to_rownames(as.data.frame(ModelEval.temp), var = "index")
            ModelEval <- as.data.frame(t(ModelEval.t))
            
            TAuc_DIFF<-ModelEval[1]-ModelEval[2]
            
            colnames(TAuc_DIFF) <- c("AUC_Diff")
            
            ModelEval <- cbind(ModelEval,TAuc_DIFF)
            
            ModelEval$Species=stringr::str_replace(especie,"_"," ")
            ModelEval$Family=orders
            ModelEval$RM=rm_best
            ModelEval$FC=FC_best
            ModelEval$InputPrePt=stat.pres.data.input
            ModelEval$UsedPrePt=nrow(maxent10_best@models[[1]]@presence)
            ModelEval$UsedAbsPt=nrow(maxent10_best@models[[1]]@absence)
            ModelEval$MX_Default=stat.time.maxent.default[1]
            ModelEval$enmeval=stat.time.enmeval[1]
            ModelEval$best10pred=stat.time.mx_best10[1]
            ModelEval$cores=ncore
            ModelEval$Threshold=tr$avg
            
            write.csv(ModelEval,paste(wd, "/", especie ,"_10CV_Temp.csv", sep="" ))
            
            ModelEval2 <-read.csv(file=paste(wd, "/", especie ,"_10CV_Temp.csv", sep="" ), header=TRUE, sep=",", fileEncoding="UTF-8-BOM")
            
            if (file.exists(paste(wd, "/", especie ,"_10CV_Temp.csv", sep="" ))) {
               #Delete file if it exists
               file.remove(paste(wd, "/", especie ,"_10CV_Temp.csv", sep="" ))
            }
            
            colnames(ModelEval2) <- c("RUN","Training.AUC","Test.AUC","TSS","TSS.thr","CBI","AUC_Diff","Species","Family","RM","FC","InputPrePt","UsedPrePt","UsedAbsPt","MX_Default","enmeval","best10pred","cores","Threshold")
            
            write.csv(ModelEval2,paste(wd, "/", especie ,"_10CV_ModelEval_Master.csv", sep="" ))
            
            
            jpeg(file=paste(especie,"_Var_Contribution.jpg",sep=''),width = 2018 , height = 2184, units = "px", pointsize = 12,quality = 100, res=300)
            barplot(contribution$mean,names.arg = stringr::str_replace(contribution$Variable,".contribution",""),xpd=FALSE,main = paste(stringr::str_replace(especie,"_"," "),"\nVariable Contribution", sep=''), ylab= "% Contribution",horiz = FALSE,border = TRUE,las=2,cex.names=.5)
            dev.off()
            
            jpeg(file=paste(especie,"_Var_Importance.jpg",sep=''),width = 2018 , height = 2184, units = "px", pointsize = 12,quality = 100, res=300)
            barplot(importance$mean,names.arg = stringr::str_replace(importance$Variable,".permutation.importance",""),xpd=FALSE,main = paste(stringr::str_replace(especie,"_"," "),"\nVariable Importance", sep=''), ylab= "% Importance",horiz = FALSE,border = TRUE,las=2,cex.names=.5)
            dev.off()
            
            jpeg(file=paste(especie,"_ModelEval_AUC_Diff.jpg",sep=''),width = 2018 , height = 2184, units = "px", pointsize = 12,quality = 100, res=300)
            boxplot(ModelEval[6], main="AUC Difference", xlab="Difference(Training-Testing)" )
            mtext(paste(stringr::str_replace(especie,"_"," "), sep=''), outer = TRUE, cex = 1.5)
            dev.off()
            
            jpeg(file=paste(especie,"_ModelEval_All.jpg",sep=''),width = 2018 , height = 2184, units = "px", pointsize = 12,quality = 100, res=300)
            par(mfrow=c(1,2))
            boxplot(ModelEval[1:2], main="AUC",names=c("Training","Testing") )
            boxplot(ModelEval[3:4], main="TSS" )
            mtext(paste(stringr::str_replace(especie,"_"," "), sep=''), outer = TRUE, cex = 1.5)
            dev.off()
            
            jpeg(file=paste(especie,"_ModelEval_CBI.jpg",sep=''),width = 2018 , height = 2184, units = "px", pointsize = 12,quality = 100, res=300)
            boxplot(ModelEval[5], main="Boyce Index", xlab="CBI" )
            mtext(paste(stringr::str_replace(especie,"_"," "), sep=''), outer = TRUE, cex = 1.5)
            dev.off()
            
            jpeg(file=paste(especie,"_ModelEval_AUC.jpg",sep=''),width = 2018 , height = 2184, units = "px", pointsize = 12,quality = 100, res=300)
            boxplot(ModelEval[1:2], main="AUC",names=c("Training","Testing") )
            mtext(paste(stringr::str_replace(especie,"_"," "), sep=''), outer = TRUE, cex = 1.5)
            dev.off()
            
            jpeg(file=paste(especie,"_ModelEval_TSS.jpg",sep=''),width = 2018 , height = 2184, units = "px", pointsize = 12,quality = 100, res=300)
            boxplot(ModelEval[3:4], main="TSS" )
            mtext(paste(stringr::str_replace(especie,"_"," "), sep=''), outer = TRUE, cex = 1.5)
            dev.off()
            
            
            
            ##############################Projections##########################################
            beginCluster(ncore)
            
            # proj_scen <- c("IPSL_CM5AMR_45_50","IPSL_CM5AMR_45_70","IPSL_CM5AMR_85_50","IPSL_CM5AMR_85_70","FIO_ESM_45_50","FIO_ESM_45_70","FIO_ESM_85_50","FIO_ESM_85_70","CESM1_CAM5_45_50","CESM1_CAM5_45_70","CESM1_CAM5_85_50","CESM1_CAM5_85_70","MIROC_MIROC5_45_50","MIROC_MIROC5_45_70","MIROC_MIROC5_85_50","MIROC_MIROC5_85_70","CSIRO_ACCESS13_45_50","CSIRO_ACCESS13_45_70","CSIRO_ACCESS13_85_50","CSIRO_ACCESS13_85_70","CC_LGM","CC_MID","ME_LGM","ME_MID","MR_LGM","MR_MID")
            
            proj_scen <- c("Current","CESM1_CAM5_45_50","CESM1_CAM5_45_70","CESM1_CAM5_85_50","CESM1_CAM5_85_70","MIROC_MIROC5_45_50","MIROC_MIROC5_45_70","MIROC_MIROC5_85_50","MIROC_MIROC5_85_70","CSIRO_ACCESS13_45_50","CSIRO_ACCESS13_45_70","CSIRO_ACCESS13_85_50","CSIRO_ACCESS13_85_70","CC_LGM","CC_MID","ME_LGM","ME_MID","MR_LGM","MR_MID")
            for(scen in proj_scen){
               cat("\n")
               cat("Preparing scenario :")
               cat(scen)
               cat("\n")
               t=stack(paste('E:/PHD_DATA/Data/GRDs1KM/',scen,'.grd', sep=''))
               # t <- dropLayer(t,c("ele_TRI"))
               # t <- stack(t,DistLayer)
               # t <- dropLayer(t, remove.layers$layer)
               # t <- exclude(t, v1)
               # t <- dropLayer(t, sel.layers2$layer)
               t <-stack(t)
               t <-crop(t, b)
               t <-mask(t, b)
               t <-stack(t)
               assign(paste('stk_',scen, sep=''),t)
            }
            endCluster()
            
            
            
            
            beginCluster(ncore)
            
            
            ####################Current###################################################################################################
            cat("Projecting: Current")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_Current,return_lfx=FALSE)
               if (i==1)	{	pred.Current.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.Current.10cv <- stack(pred.Current.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.Current.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.Current.mean <- clusterR(pred.Current.10cv, calc, args = list(mean, na.rm=T))
            pred.Current.sd <- clusterR(pred.Current.10cv, calc, args = list(sd, na.rm=T))
            pred.Current.binary<-raster_threshold(input_raster = pred.Current.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.Current.10cv, filename=paste(wd, "/GRD/", especie ,"_Current_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.Current.mean, filename=paste(wd, "/GRD/", especie ,"_Current_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.Current.sd, filename=paste(wd, "/GRD/", especie ,"_Current_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.Current.binary, filename=paste(wd, "/GRD/", especie ,"_Current_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            writeRaster(pred.Current.10cv, filename=paste(wd, "/maps/", especie ,"_Current_10cv.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.Current.mean, filename=paste(wd, "/maps/", especie ,"_Current_Mean.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.Current.sd, filename=paste(wd, "/maps/", especie ,"_Current_SD.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.Current.binary, filename=paste(wd, "/maps/", especie ,"_Current_Binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            
            # rm(pred.Current.10cv)
            
            ####################CESM1_CAM5_45_50###################################################################################################
            cat("Projecting: CESM1_CAM5_45_50")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CESM1_CAM5_45_50,return_lfx=FALSE)
               if (i==1)	{	pred.CESM1_CAM5_45_50.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CESM1_CAM5_45_50.10cv <- stack(pred.CESM1_CAM5_45_50.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CESM1_CAM5_45_50.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CESM1_CAM5_45_50.mean <- clusterR(pred.CESM1_CAM5_45_50.10cv, calc, args = list(mean, na.rm=T))
            pred.CESM1_CAM5_45_50.sd <- clusterR(pred.CESM1_CAM5_45_50.10cv, calc, args = list(sd, na.rm=T))
            pred.CESM1_CAM5_45_50.binary<-raster_threshold(input_raster = pred.CESM1_CAM5_45_50.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CESM1_CAM5_45_50.10cv, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_45_50_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_45_50.mean, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_45_50_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_45_50.sd, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_45_50_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_45_50.binary, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_45_50_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CESM1_CAM5_45_50.10cv)
            rm(stk_CESM1_CAM5_45_50)
            ####################CESM1_CAM5_45_70###################################################################################################
            cat("Projecting: CESM1_CAM5_45_70")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CESM1_CAM5_45_70,return_lfx=FALSE)
               if (i==1)	{	pred.CESM1_CAM5_45_70.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CESM1_CAM5_45_70.10cv <- stack(pred.CESM1_CAM5_45_70.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CESM1_CAM5_45_70.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CESM1_CAM5_45_70.mean <- clusterR(pred.CESM1_CAM5_45_70.10cv, calc, args = list(mean, na.rm=T))
            pred.CESM1_CAM5_45_70.sd <- clusterR(pred.CESM1_CAM5_45_70.10cv, calc, args = list(sd, na.rm=T))
            pred.CESM1_CAM5_45_70.binary<-raster_threshold(input_raster = pred.CESM1_CAM5_45_70.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CESM1_CAM5_45_70.10cv, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_45_70_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_45_70.mean, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_45_70_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_45_70.sd, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_45_70_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_45_70.binary, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_45_70_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CESM1_CAM5_45_70.10cv)
            rm(stk_CESM1_CAM5_45_70)
            ####################CESM1_CAM5_85_50###################################################################################################
            cat("Projecting: CESM1_CAM5_85_50")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CESM1_CAM5_85_50,return_lfx=FALSE)
               if (i==1)	{	pred.CESM1_CAM5_85_50.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CESM1_CAM5_85_50.10cv <- stack(pred.CESM1_CAM5_85_50.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CESM1_CAM5_85_50.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CESM1_CAM5_85_50.mean <- clusterR(pred.CESM1_CAM5_85_50.10cv, calc, args = list(mean, na.rm=T))
            pred.CESM1_CAM5_85_50.sd <- clusterR(pred.CESM1_CAM5_85_50.10cv, calc, args = list(sd, na.rm=T))
            pred.CESM1_CAM5_85_50.binary<-raster_threshold(input_raster = pred.CESM1_CAM5_85_50.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CESM1_CAM5_85_50.10cv, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_85_50_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_85_50.mean, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_85_50_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_85_50.sd, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_85_50_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_85_50.binary, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_85_50_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CESM1_CAM5_85_50.10cv)
            rm(stk_CESM1_CAM5_85_50)
            
            ####################CESM1_CAM5_85_70###################################################################################################
            cat("Projecting: CESM1_CAM5_85_70")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CESM1_CAM5_85_70,return_lfx=FALSE)
               if (i==1)	{	pred.CESM1_CAM5_85_70.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CESM1_CAM5_85_70.10cv <- stack(pred.CESM1_CAM5_85_70.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CESM1_CAM5_85_70.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CESM1_CAM5_85_70.mean <- clusterR(pred.CESM1_CAM5_85_70.10cv, calc, args = list(mean, na.rm=T))
            pred.CESM1_CAM5_85_70.sd <- clusterR(pred.CESM1_CAM5_85_70.10cv, calc, args = list(sd, na.rm=T))
            pred.CESM1_CAM5_85_70.binary<-raster_threshold(input_raster = pred.CESM1_CAM5_85_70.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CESM1_CAM5_85_70.10cv, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_85_70_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_85_70.mean, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_85_70_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_85_70.sd, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_85_70_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CESM1_CAM5_85_70.binary, filename=paste(wd, "/GRD/", especie ,"_CESM1_CAM5_85_70_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CESM1_CAM5_85_70.10cv)
            rm(stk_CESM1_CAM5_85_70)
            
            ####################CSIRO_ACCESS13_45_50###################################################################################################
            cat("Projecting: CSIRO_ACCESS13_45_50")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CSIRO_ACCESS13_45_50,return_lfx=FALSE)
               if (i==1)	{	pred.CSIRO_ACCESS13_45_50.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CSIRO_ACCESS13_45_50.10cv <- stack(pred.CSIRO_ACCESS13_45_50.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CSIRO_ACCESS13_45_50.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CSIRO_ACCESS13_45_50.mean <- clusterR(pred.CSIRO_ACCESS13_45_50.10cv, calc, args = list(mean, na.rm=T))
            pred.CSIRO_ACCESS13_45_50.sd <- clusterR(pred.CSIRO_ACCESS13_45_50.10cv, calc, args = list(sd, na.rm=T))
            pred.CSIRO_ACCESS13_45_50.binary<-raster_threshold(input_raster = pred.CSIRO_ACCESS13_45_50.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_45_50.10cv, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_45_50_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_45_50.mean, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_45_50_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_45_50.sd, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_45_50_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_45_50.binary, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_45_50_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CSIRO_ACCESS13_45_50.10cv)
            rm(stk_CSIRO_ACCESS13_45_50)
            
            ####################CSIRO_ACCESS13_45_70###################################################################################################
            cat("Projecting: CSIRO_ACCESS13_45_70")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CSIRO_ACCESS13_45_70,return_lfx=FALSE)
               if (i==1)	{	pred.CSIRO_ACCESS13_45_70.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CSIRO_ACCESS13_45_70.10cv <- stack(pred.CSIRO_ACCESS13_45_70.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CSIRO_ACCESS13_45_70.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CSIRO_ACCESS13_45_70.mean <- clusterR(pred.CSIRO_ACCESS13_45_70.10cv, calc, args = list(mean, na.rm=T))
            pred.CSIRO_ACCESS13_45_70.sd <- clusterR(pred.CSIRO_ACCESS13_45_70.10cv, calc, args = list(sd, na.rm=T))
            pred.CSIRO_ACCESS13_45_70.binary<-raster_threshold(input_raster = pred.CSIRO_ACCESS13_45_70.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_45_70.10cv, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_45_70_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_45_70.mean, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_45_70_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_45_70.sd, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_45_70_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_45_70.binary, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_45_70_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CSIRO_ACCESS13_45_70.10cv)
            rm(stk_CSIRO_ACCESS13_45_70)
            
            ####################CSIRO_ACCESS13_85_50###################################################################################################
            cat("Projecting: CSIRO_ACCESS13_85_50")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CSIRO_ACCESS13_85_50,return_lfx=FALSE)
               if (i==1)	{	pred.CSIRO_ACCESS13_85_50.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CSIRO_ACCESS13_85_50.10cv <- stack(pred.CSIRO_ACCESS13_85_50.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CSIRO_ACCESS13_85_50.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CSIRO_ACCESS13_85_50.mean <- clusterR(pred.CSIRO_ACCESS13_85_50.10cv, calc, args = list(mean, na.rm=T))
            pred.CSIRO_ACCESS13_85_50.sd <- clusterR(pred.CSIRO_ACCESS13_85_50.10cv, calc, args = list(sd, na.rm=T))
            pred.CSIRO_ACCESS13_85_50.binary<-raster_threshold(input_raster = pred.CSIRO_ACCESS13_85_50.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_85_50.10cv, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_85_50_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_85_50.mean, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_85_50_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_85_50.sd, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_85_50_SD.grd", sep="" ), overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_85_50.binary, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_85_50_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CSIRO_ACCESS13_85_50.10cv)
            rm(stk_CSIRO_ACCESS13_85_50)
            
            ####################CSIRO_ACCESS13_85_70###################################################################################################
            cat("Projecting: CSIRO_ACCESS13_85_70")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CSIRO_ACCESS13_85_70,return_lfx=FALSE)
               if (i==1)	{	pred.CSIRO_ACCESS13_85_70.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CSIRO_ACCESS13_85_70.10cv <- stack(pred.CSIRO_ACCESS13_85_70.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CSIRO_ACCESS13_85_70.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CSIRO_ACCESS13_85_70.mean <- clusterR(pred.CSIRO_ACCESS13_85_70.10cv, calc, args = list(mean, na.rm=T))
            pred.CSIRO_ACCESS13_85_70.sd <- clusterR(pred.CSIRO_ACCESS13_85_70.10cv, calc, args = list(sd, na.rm=T))
            pred.CSIRO_ACCESS13_85_70.binary<-raster_threshold(input_raster = pred.CSIRO_ACCESS13_85_70.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_85_70.10cv, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_85_70_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_85_70.mean, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_85_70_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_85_70.sd, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_85_70_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CSIRO_ACCESS13_85_70.binary, filename=paste(wd, "/GRD/", especie ,"_CSIRO_ACCESS13_85_70_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CSIRO_ACCESS13_85_70.10cv)
            rm(stk_CSIRO_ACCESS13_85_70)
            
            ####################MIROC_MIROC5_45_50###################################################################################################
            cat("Projecting: MIROC_MIROC5_45_50")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_MIROC_MIROC5_45_50,return_lfx=FALSE)
               if (i==1)	{	pred.MIROC_MIROC5_45_50.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.MIROC_MIROC5_45_50.10cv <- stack(pred.MIROC_MIROC5_45_50.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.MIROC_MIROC5_45_50.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.MIROC_MIROC5_45_50.mean <- clusterR(pred.MIROC_MIROC5_45_50.10cv, calc, args = list(mean, na.rm=T))
            pred.MIROC_MIROC5_45_50.sd <- clusterR(pred.MIROC_MIROC5_45_50.10cv, calc, args = list(sd, na.rm=T))
            pred.MIROC_MIROC5_45_50.binary<-raster_threshold(input_raster = pred.MIROC_MIROC5_45_50.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.MIROC_MIROC5_45_50.10cv, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_45_50_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_45_50.mean, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_45_50_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_45_50.sd, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_45_50_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_45_50.binary, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_45_50_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.MIROC_MIROC5_45_50.10cv)
            rm(stk_MIROC_MIROC5_45_50)
            
            
            ####################MIROC_MIROC5_45_70###################################################################################################
            cat("Projecting: MIROC_MIROC5_45_70")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_MIROC_MIROC5_45_70,return_lfx=FALSE)
               if (i==1)	{	pred.MIROC_MIROC5_45_70.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.MIROC_MIROC5_45_70.10cv <- stack(pred.MIROC_MIROC5_45_70.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.MIROC_MIROC5_45_70.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.MIROC_MIROC5_45_70.mean <- clusterR(pred.MIROC_MIROC5_45_70.10cv, calc, args = list(mean, na.rm=T))
            pred.MIROC_MIROC5_45_70.sd <- clusterR(pred.MIROC_MIROC5_45_70.10cv, calc, args = list(sd, na.rm=T))
            pred.MIROC_MIROC5_45_70.binary<-raster_threshold(input_raster = pred.MIROC_MIROC5_45_70.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.MIROC_MIROC5_45_70.10cv, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_45_70_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_45_70.mean, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_45_70_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_45_70.sd, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_45_70_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_45_70.binary, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_45_70_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.MIROC_MIROC5_45_70.10cv)
            rm(stk_MIROC_MIROC5_45_70)
            
            ####################MIROC_MIROC5_85_50###################################################################################################
            cat("Projecting: MIROC_MIROC5_85_50")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_MIROC_MIROC5_85_50,return_lfx=FALSE)
               if (i==1)	{	pred.MIROC_MIROC5_85_50.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.MIROC_MIROC5_85_50.10cv <- stack(pred.MIROC_MIROC5_85_50.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.MIROC_MIROC5_85_50.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.MIROC_MIROC5_85_50.mean <- clusterR(pred.MIROC_MIROC5_85_50.10cv, calc, args = list(mean, na.rm=T))
            pred.MIROC_MIROC5_85_50.sd <- clusterR(pred.MIROC_MIROC5_85_50.10cv, calc, args = list(sd, na.rm=T))
            pred.MIROC_MIROC5_85_50.binary<-raster_threshold(input_raster = pred.MIROC_MIROC5_85_50.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.MIROC_MIROC5_85_50.10cv, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_85_50_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_85_50.mean, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_85_50_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_85_50.sd, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_85_50_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_85_50.binary, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_85_50_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.MIROC_MIROC5_85_50.10cv)
            rm(stk_MIROC_MIROC5_85_50)
            
            ####################MIROC_MIROC5_85_70###################################################################################################
            cat("Projecting: MIROC_MIROC5_85_70")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_MIROC_MIROC5_85_70,return_lfx=FALSE)
               if (i==1)	{	pred.MIROC_MIROC5_85_70.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.MIROC_MIROC5_85_70.10cv <- stack(pred.MIROC_MIROC5_85_70.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.MIROC_MIROC5_85_70.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.MIROC_MIROC5_85_70.mean <- clusterR(pred.MIROC_MIROC5_85_70.10cv, calc, args = list(mean, na.rm=T))
            pred.MIROC_MIROC5_85_70.sd <- clusterR(pred.MIROC_MIROC5_85_70.10cv, calc, args = list(sd, na.rm=T))
            pred.MIROC_MIROC5_85_70.binary<-raster_threshold(input_raster = pred.MIROC_MIROC5_85_70.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.MIROC_MIROC5_85_70.10cv, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_85_70_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_85_70.mean, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_85_70_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_85_70.sd, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_85_70_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MIROC_MIROC5_85_70.binary, filename=paste(wd, "/GRD/", especie ,"_MIROC_MIROC5_85_70_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.MIROC_MIROC5_85_70.10cv)
            rm(stk_MIROC_MIROC5_85_70)
            
            
            
            ####################MR_LGM###################################################################################################
            cat("Projecting: MR_LGM")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_MR_LGM,return_lfx=FALSE)
               if (i==1)	{	pred.MR_LGM.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.MR_LGM.10cv <- stack(pred.MR_LGM.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.MR_LGM.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.MR_LGM.mean <- clusterR(pred.MR_LGM.10cv, calc, args = list(mean, na.rm=T))
            pred.MR_LGM.sd <- clusterR(pred.MR_LGM.10cv, calc, args = list(sd, na.rm=T))
            pred.MR_LGM.binary<-raster_threshold(input_raster = pred.MR_LGM.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.MR_LGM.10cv, filename=paste(wd, "/GRD/", especie ,"_MR_LGM_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MR_LGM.mean, filename=paste(wd, "/GRD/", especie ,"_MR_LGM_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MR_LGM.sd, filename=paste(wd, "/GRD/", especie ,"_MR_LGM_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MR_LGM.binary, filename=paste(wd, "/GRD/", especie ,"_MR_LGM_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.MR_LGM.10cv)
            rm(stk_MR_LGM)
            
            ####################MR_MID###################################################################################################
            cat("Projecting: MR_MID")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_MR_MID,return_lfx=FALSE)
               if (i==1)	{	pred.MR_MID.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.MR_MID.10cv <- stack(pred.MR_MID.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.MR_MID.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.MR_MID.mean <- clusterR(pred.MR_MID.10cv, calc, args = list(mean, na.rm=T))
            pred.MR_MID.sd <- clusterR(pred.MR_MID.10cv, calc, args = list(sd, na.rm=T))
            pred.MR_MID.binary<-raster_threshold(input_raster = pred.MR_MID.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.MR_MID.10cv, filename=paste(wd, "/GRD/", especie ,"_MR_MID_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MR_MID.mean, filename=paste(wd, "/GRD/", especie ,"_MR_MID_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MR_MID.sd, filename=paste(wd, "/GRD/", especie ,"_MR_MID_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MR_MID.binary, filename=paste(wd, "/GRD/", especie ,"_MR_MID_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.MR_MID.10cv)
            rm(stk_MR_MID)
            
            ####################CC_LGM###################################################################################################
            cat("Projecting: CC_LGM")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CC_LGM,return_lfx=FALSE)
               if (i==1)	{	pred.CC_LGM.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CC_LGM.10cv <- stack(pred.CC_LGM.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CC_LGM.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CC_LGM.mean <- clusterR(pred.CC_LGM.10cv, calc, args = list(mean, na.rm=T))
            pred.CC_LGM.sd <- clusterR(pred.CC_LGM.10cv, calc, args = list(sd, na.rm=T))
            pred.CC_LGM.binary<-raster_threshold(input_raster = pred.CC_LGM.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CC_LGM.10cv, filename=paste(wd, "/GRD/", especie ,"_CC_LGM_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CC_LGM.mean, filename=paste(wd, "/GRD/", especie ,"_CC_LGM_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CC_LGM.sd, filename=paste(wd, "/GRD/", especie ,"_CC_LGM_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CC_LGM.binary, filename=paste(wd, "/GRD/", especie ,"_CC_LGM_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CC_LGM.10cv)
            rm(stk_CC_LGM)
            
            ####################CC_MID###################################################################################################
            cat("Projecting: CC_MID")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_CC_MID,return_lfx=FALSE)
               if (i==1)	{	pred.CC_MID.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.CC_MID.10cv <- stack(pred.CC_MID.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.CC_MID.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.CC_MID.mean <- clusterR(pred.CC_MID.10cv, calc, args = list(mean, na.rm=T))
            pred.CC_MID.sd <- clusterR(pred.CC_MID.10cv, calc, args = list(sd, na.rm=T))
            pred.CC_MID.binary<-raster_threshold(input_raster = pred.CC_MID.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.CC_MID.10cv, filename=paste(wd, "/GRD/", especie ,"_CC_MID_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CC_MID.mean, filename=paste(wd, "/GRD/", especie ,"_CC_MID_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CC_MID.sd, filename=paste(wd, "/GRD/", especie ,"_CC_MID_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.CC_MID.binary, filename=paste(wd, "/GRD/", especie ,"_CC_MID_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.CC_MID.10cv)
            rm(stk_CC_MID)
            
            
            
            ####################ME_LGM###################################################################################################
            cat("Projecting: ME_LGM")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_ME_LGM,return_lfx=FALSE)
               if (i==1)	{	pred.ME_LGM.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.ME_LGM.10cv <- stack(pred.ME_LGM.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.ME_LGM.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.ME_LGM.mean <- clusterR(pred.ME_LGM.10cv, calc, args = list(mean, na.rm=T))
            pred.ME_LGM.sd <- clusterR(pred.ME_LGM.10cv, calc, args = list(sd, na.rm=T))
            pred.ME_LGM.binary<-raster_threshold(input_raster = pred.ME_LGM.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.ME_LGM.10cv, filename=paste(wd, "/GRD/", especie ,"_ME_LGM_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.ME_LGM.mean, filename=paste(wd, "/GRD/", especie ,"_ME_LGM_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.ME_LGM.sd, filename=paste(wd, "/GRD/", especie ,"_ME_LGM_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.ME_LGM.binary, filename=paste(wd, "/GRD/", especie ,"_ME_LGM_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.ME_LGM.10cv)
            rm(stk_ME_LGM)
            
            ####################ME_MID###################################################################################################
            cat("Projecting: ME_MID")
            cat("\n")
            for (i in 1:10)
            {
               p.mx_best1 <- project(maxent10_best@models[[i]],stk_ME_MID,return_lfx=FALSE)
               if (i==1)	{	pred.ME_MID.10cv <- p.mx_best1$prediction_logistic	}
               if (i>1)	{   pred.ME_MID.10cv <- stack(pred.ME_MID.10cv,p.mx_best1$prediction_logistic)	}
            }
            
            names(pred.ME_MID.10cv) <- c("cv_0", "cv_1", "cv_2", "cv_3", "cv_4", "cv_5", "cv_6", "cv_7", "cv_8", "cv_9")
            pred.ME_MID.mean <- clusterR(pred.ME_MID.10cv, calc, args = list(mean, na.rm=T))
            pred.ME_MID.sd <- clusterR(pred.ME_MID.10cv, calc, args = list(sd, na.rm=T))
            pred.ME_MID.binary<-raster_threshold(input_raster = pred.ME_MID.mean, threshold = tr$avg,  binary=TRUE)
            writeRaster(pred.ME_MID.10cv, filename=paste(wd, "/GRD/", especie ,"_ME_MID_10cv.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.ME_MID.mean, filename=paste(wd, "/GRD/", especie ,"_ME_MID_Mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.ME_MID.sd, filename=paste(wd, "/GRD/", especie ,"_ME_MID_SD.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.ME_MID.binary, filename=paste(wd, "/GRD/", especie ,"_ME_MID_Binary.grd", sep="" ), datatype='INT2S', overwrite=TRUE)
            rm(pred.ME_MID.10cv)
            rm(stk_ME_MID)
            
            
            rm(t)
            gc()
            #########################################Mean Calculation#################################
            cat("Calculating means")
            cat("\n")
            
            
            dir.create(paste(wd,"/GRD",sep=''))
            dir.create(paste(wd,"/TIFF",sep=''))
            pred.45_50.stk<- stack(pred.CESM1_CAM5_45_50.mean,pred.CSIRO_ACCESS13_45_50.mean,pred.MIROC_MIROC5_45_50.mean)
            pred.45_70.stk<- stack(pred.CESM1_CAM5_45_70.mean,pred.CSIRO_ACCESS13_45_70.mean,pred.MIROC_MIROC5_45_70.mean)
            pred.85_50.stk<- stack(pred.CESM1_CAM5_85_50.mean,pred.CSIRO_ACCESS13_85_50.mean,pred.MIROC_MIROC5_85_50.mean)
            pred.85_70.stk<- stack(pred.CESM1_CAM5_85_70.mean,pred.CSIRO_ACCESS13_85_70.mean,pred.MIROC_MIROC5_85_70.mean)
            pred.LGM.stk<- stack(pred.CC_LGM.mean,pred.ME_LGM.mean,pred.MR_LGM.mean)
            pred.MID.stk<- stack(pred.CC_MID.mean,pred.ME_MID.mean,pred.MR_MID.mean)
            pred.Current.stk<- stack(pred.Current.10cv)
            
            
            writeRaster(pred.45_50.stk, filename=paste(wd, "/GRD/", especie ,"_pred.45_50.stk.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_70.stk, filename=paste(wd, "/GRD/", especie ,"_pred.45_70.stk.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.85_50.stk, filename=paste(wd, "/GRD/", especie ,"_pred.85_50.stk.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.85_70.stk, filename=paste(wd, "/GRD/", especie ,"_pred.85_70.stk.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.LGM.stk, filename=paste(wd, "/GRD/", especie ,"_pred.LGM.stk.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MID.stk, filename=paste(wd, "/GRD/", especie ,"_pred.MID.stk.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.Current.stk, filename=paste(wd, "/GRD/", especie ,"_pred.Current.stk.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            
            writeRaster(pred.45_50.stk, filename=paste(wd, "/TIFF/", especie ,"_pred.45_50.stk.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_70.stk, filename=paste(wd, "/TIFF/", especie ,"_pred.45_70.stk.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.85_50.stk, filename=paste(wd, "/TIFF/", especie ,"_pred.85_50.stk.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.85_70.stk, filename=paste(wd, "/TIFF/", especie ,"_pred.85_70.stk.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.LGM.stk, filename=paste(wd, "/TIFF/", especie ,"_pred.LGM.stk.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MID.stk, filename=paste(wd, "/TIFF/", especie ,"_pred.MID.stk.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.Current.stk, filename=paste(wd, "/TIFF/", especie ,"_pred.Current.stk.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            
            
            
            
            pred.85_70.mean <- clusterR(pred.85_70.stk, calc, args = list(mean, na.rm=T))
            pred.85_50.mean <- clusterR(pred.85_50.stk, calc, args = list(mean, na.rm=T))
            pred.45_70.mean <- clusterR(pred.45_70.stk, calc, args = list(mean, na.rm=T))
            pred.45_50.mean <- clusterR(pred.45_50.stk, calc, args = list(mean, na.rm=T))
            pred.LGM.mean <- clusterR(pred.LGM.stk, calc, args = list(mean, na.rm=T))
            pred.MID.mean <- clusterR(pred.MID.stk, calc, args = list(mean, na.rm=T))
            pred.Current.mean <- clusterR(pred.Current.stk, calc, args = list(mean, na.rm=T))
            pred.2070.mean <- clusterR(stack(pred.45_70.stk,pred.85_70.stk), calc, args = list(mean, na.rm=T))
            pred.2050.mean <- clusterR(stack(pred.45_50.stk,pred.85_50.stk), calc, args = list(mean, na.rm=T))
            
            
            writeRaster(pred.85_70.mean, filename=paste(wd, "/GRD/", especie ,"_pred.85_70.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.85_50.mean, filename=paste(wd, "/GRD/", especie ,"_pred.85_50.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_70.mean, filename=paste(wd, "/GRD/", especie ,"_pred.45_70.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_50.mean, filename=paste(wd, "/GRD/", especie ,"_pred.45_50.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.LGM.mean, filename=paste(wd, "/GRD/", especie ,"_pred.LGM.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MID.mean, filename=paste(wd, "/GRD/", especie ,"_pred.MID.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            
            writeRaster(pred.Current.mean, filename=paste(wd, "/GRD/", especie ,"_pred.Current.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.2070.mean, filename=paste(wd, "/GRD/", especie ,"_pred.2070.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.2050.mean, filename=paste(wd, "/GRD/", especie ,"_pred.2050.mean.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            
            writeRaster(pred.85_70.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.85_70.mean.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.85_50.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.85_50.mean.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_70.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.45_700.mean.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_50.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.45_50.mean.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.LGM.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.LGM.mean.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MID.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.MID.mean.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            
            writeRaster(pred.Current.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.Current.mean.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.2070.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.2070.mean.tiff", sep="" ),format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.2050.mean, filename=paste(wd, "/TIFF/", especie ,"_pred.2050.mean.tiff", sep="" ),format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            
            
            
            pred.85_70.sd <- clusterR(pred.85_70.stk, calc, args = list(sd, na.rm=T))
            pred.85_50.sd <- clusterR(pred.85_50.stk, calc, args = list(sd, na.rm=T))
            pred.45_70.sd <- clusterR(pred.45_70.stk, calc, args = list(sd, na.rm=T))
            pred.45_50.sd <- clusterR(pred.45_50.stk, calc, args = list(sd, na.rm=T))
            pred.LGM.sd <- clusterR(pred.LGM.stk, calc, args = list(sd, na.rm=T))
            pred.MID.sd <- clusterR(pred.MID.stk, calc, args = list(sd, na.rm=T))
            pred.Current.sd <- clusterR(pred.Current.stk, calc, args = list(sd, na.rm=T))
            pred.2070.sd <- clusterR(stack(pred.45_70.stk,pred.85_70.stk), calc, args = list(sd, na.rm=T))
            pred.2050.sd <- clusterR(stack(pred.45_50.stk,pred.85_50.stk), calc, args = list(sd, na.rm=T))
            
            
            writeRaster(pred.85_70.sd, filename=paste(wd, "/GRD/", especie ,"_pred.85_70.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.85_50.sd, filename=paste(wd, "/GRD/", especie ,"_pred.85_50.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_70.sd, filename=paste(wd, "/GRD/", especie ,"_pred.45_70.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_50.sd, filename=paste(wd, "/GRD/", especie ,"_pred.45_50.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.LGM.sd, filename=paste(wd, "/GRD/", especie ,"_pred.LGM.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MID.sd, filename=paste(wd, "/GRD/", especie ,"_pred.MID.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.Current.sd, filename=paste(wd, "/GRD/", especie ,"_pred.Current.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.2070.sd, filename=paste(wd, "/GRD/", especie ,"_pred.2070.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.2050.sd, filename=paste(wd, "/GRD/", especie ,"_pred.2050.sd.grd", sep="" ), datatype='FLT4S', overwrite=TRUE)
            
            writeRaster(pred.85_70.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.85_70.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.85_50.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.85_50.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_70.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.45_70.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.45_50.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.45_50.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.LGM.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.LGM.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.MID.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.MID.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.Current.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.Current.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.2070.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.2070.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            writeRaster(pred.2050.sd, filename=paste(wd, "/TIFF/", especie ,"_pred.2050.sd.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='FLT4S', overwrite=TRUE)
            
            
            
            pred.85_70.binary<-raster_threshold(input_raster = pred.85_70.mean, threshold = tr$avg,  binary=TRUE)
            pred.85_50.binary<-raster_threshold(input_raster = pred.85_50.mean, threshold = tr$avg,  binary=TRUE)
            pred.45_70.binary<-raster_threshold(input_raster = pred.45_70.mean, threshold = tr$avg,  binary=TRUE)
            pred.45_50.binary<-raster_threshold(input_raster = pred.45_50.mean, threshold = tr$avg,  binary=TRUE)
            pred.LGM.binary<-raster_threshold(input_raster = pred.LGM.mean, threshold = tr$avg,  binary=TRUE)
            pred.MID.binary<-raster_threshold(input_raster = pred.MID.mean, threshold = tr$avg,  binary=TRUE)
            pred.2070.binary<-raster_threshold(input_raster = pred.2070.mean, threshold = tr$avg,  binary=TRUE)
            pred.2050.binary<-raster_threshold(input_raster = pred.2050.mean, threshold = tr$avg,  binary=TRUE)
            pred.Current.binary<-raster_threshold(input_raster = pred.Current.mean, threshold = tr$avg,  binary=TRUE)
            
            
            
            writeRaster(pred.85_70.binary, filename=paste(wd, "/GRD/", especie ,"_pred.85_70..binary.grd", sep="" ), overwrite=TRUE)
            writeRaster(pred.85_50.binary, filename=paste(wd, "/GRD/", especie ,"_pred.85_50.binary.grd", sep="" ), overwrite=TRUE)
            writeRaster(pred.45_70.binary, filename=paste(wd, "/GRD/", especie ,"_pred.45_70.binary.grd", sep="" ), overwrite=TRUE)
            writeRaster(pred.45_50.binary, filename=paste(wd, "/GRD/", especie ,"_pred.45_50.binary.grd", sep="" ), datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.LGM.binary, filename=paste(wd, "/GRD/", especie ,"_pred.LGM.binaryd.grd", sep="" ), datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.MID.binary, filename=paste(wd, "/GRD/", especie ,"_pred.MID.binary.grd", sep="" ), datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.Current.binary, filename=paste(wd, "/GRD/", especie ,"_pred.Current.binary.grd", sep="" ), datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.2070.binary, filename=paste(wd, "/GRD/", especie ,"_pred.2070.binary.grd", sep="" ), datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.2050.binary, filename=paste(wd, "/GRD/", especie ,"_pred.2050.binary.grd", sep="" ), datatype='INT4S', overwrite=TRUE)
            
            writeRaster(pred.85_70.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.85_70.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.85_50.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.85_50.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.45_70.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.45_70.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.45_50.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.45_50.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.LGM.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.LGM.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.MID.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.MID.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.Current.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.Current.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.2070.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.2070.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(pred.2050.binary, filename=paste(wd, "/TIFF/", especie ,"_pred.2050.binary.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            
            NullLayer = crop(NullLayer,ind)
            NullLayer = mask(NullLayer,st_zm(ind))
            
            Proj.LGM.Binary.Ind = crop(pred.LGM.binary,ind)
            Proj.LGM.Binary.Ind = mask(Proj.LGM.Binary.Ind,st_zm(ind))
            Proj.LGM.Binary.Ind = merge(Proj.LGM.Binary.Ind , NullLayer)
            
            Proj.MID.Binary.Ind = crop(pred.MID.binary,ind)
            Proj.MID.Binary.Ind = mask(Proj.MID.Binary.Ind,st_zm(ind))
            Proj.MID.Binary.Ind = merge(Proj.MID.Binary.Ind , NullLayer)
            
            Proj.Current.Binary.Ind = crop(pred.Current.binary,ind)
            Proj.Current.Binary.Ind = mask(Proj.Current.Binary.Ind,st_zm(ind))
            Proj.Current.Binary.Ind = merge(Proj.Current.Binary.Ind , NullLayer)
            
            Proj.85_70.Binary.Ind = crop(pred.85_70.binary,ind)
            Proj.85_70.Binary.Ind = mask(Proj.85_70.Binary.Ind,st_zm(ind))
            Proj.85_70.Binary.Ind = merge(Proj.85_70.Binary.Ind , NullLayer)
            
            Proj.85_50.Binary.Ind = crop(pred.85_50.binary,ind)
            Proj.85_50.Binary.Ind = mask(Proj.85_50.Binary.Ind,st_zm(ind))
            Proj.85_50.Binary.Ind = merge(Proj.85_50.Binary.Ind , NullLayer)
            
            Proj.45_70.Binary.Ind = crop(pred.45_70.binary,ind)
            Proj.45_70.Binary.Ind = mask(Proj.45_70.Binary.Ind,st_zm(ind))
            Proj.45_70.Binary.Ind = merge(Proj.45_70.Binary.Ind , NullLayer)
            
            Proj.45_50.Binary.Ind = crop(pred.45_50.binary,ind)
            Proj.45_50.Binary.Ind = mask(Proj.45_50.Binary.Ind,st_zm(ind))
            Proj.45_50.Binary.Ind = merge(Proj.45_50.Binary.Ind , NullLayer)
            
            Proj.2050.Binary.Ind = crop(pred.2050.binary,ind)
            Proj.2050.Binary.Ind = mask(Proj.2050.Binary.Ind,st_zm(ind))
            Proj.2050.Binary.Ind = merge(Proj.2050.Binary.Ind , NullLayer)
            
            Proj.2070.Binary.Ind = crop(pred.2070.binary,ind)
            Proj.2070.Binary.Ind = mask(Proj.2070.Binary.Ind,st_zm(ind))
            Proj.2070.Binary.Ind = merge(Proj.2070.Binary.Ind , NullLayer)
            
            RangeSize.Current.85_70 <- BIOMOD_RangeSize(CurrentPred=Proj.Current.Binary.Ind, FutureProj=Proj.85_70.Binary.Ind)
            RangeSize.Current.85_50 <- BIOMOD_RangeSize(CurrentPred=Proj.Current.Binary.Ind, FutureProj=Proj.85_50.Binary.Ind)
            RangeSize.Current.45_70 <- BIOMOD_RangeSize(CurrentPred=Proj.Current.Binary.Ind, FutureProj=Proj.45_70.Binary.Ind)
            RangeSize.Current.45_50 <- BIOMOD_RangeSize(CurrentPred=Proj.Current.Binary.Ind, FutureProj=Proj.45_50.Binary.Ind)
            RangeSize.Current.2050 <- BIOMOD_RangeSize(CurrentPred=Proj.Current.Binary.Ind, FutureProj=Proj.2050.Binary.Ind)
            RangeSize.Current.2070 <- BIOMOD_RangeSize(CurrentPred=Proj.Current.Binary.Ind, FutureProj=Proj.2070.Binary.Ind)
            RangeSize.MID.Current <- BIOMOD_RangeSize(CurrentPred=Proj.MID.Binary.Ind, FutureProj=Proj.Current.Binary.Ind)
            RangeSize.LGM.MID <- BIOMOD_RangeSize(CurrentPred=Proj.LGM.Binary.Ind, FutureProj=Proj.MID.Binary.Ind)
            
            # you can use do.call:
            SpRasters.stk <- stack(Proj.Current.Binary.Ind, Proj.2050.Binary.Ind,Proj.2070.Binary.Ind,Proj.MID.Binary.Ind,Proj.LGM.Binary.Ind)
            # add arguments such as filename
            # x$filename <- 'test.tif'
            SpClimRefugia <- sum(SpRasters.stk)
            
            agreements <-maxValue(SpClimRefugia)
            
            SpClimRefugia[SpClimRefugia < agreements] <- NA
            
            map.SpClimRefugia <- 
               tm_shape(SpClimRefugia,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Agreements",palette = "Dark2", n = 5,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nClimate Refugia",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            tmap_save( tm=map.SpClimRefugia,filename=paste(wd, "/jpegMAPS/", especie ,"_Climate_Refugia_ISR.jpg", sep="" ),width=6036, height=2184)
            
            map.SplimitFactor <- 
               tm_shape(SplimitFactor,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Predictors",palette = "Dark2", n = maxValue(SplimitFactor),style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nLimiting Factors",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            tmap_save( tm=map.SplimitFactor,filename=paste(wd, "/jpegMAPS/", especie ,"_LimitingFactor_ISR.jpg", sep="" ),width=6036, height=2184)
            
            
            
            tc7 <- as.data.frame(RangeSize.MID.Current$Compt.By.Models)
            tc7$Species=stringr::str_replace(especie,"_"," ")
            tc7$Family=orders
            tc7$Scn="MID.Current"
            tc7$Scenarios="Mid Holocene to Current"
            
            tc8 <- as.data.frame(RangeSize.LGM.MID$Compt.By.Models)
            tc8$Species=stringr::str_replace(especie,"_"," ")
            tc8$Family=orders
            tc8$Scn="LGM.MID"
            tc8$Scenarios="Last Glacial Maximum to Mid Holocene"
            
            tc1 <- as.data.frame(RangeSize.Current.85_70$Compt.By.Models)
            tc1$Species=stringr::str_replace(especie,"_"," ")
            tc1$Family=orders
            tc1$Scn="Current.85_70"
            tc1$Scenarios="Current to RCP 8.5 Year 2070"
            
            tc2 <- as.data.frame(RangeSize.Current.85_50$Compt.By.Models)
            tc2$Species=stringr::str_replace(especie,"_"," ")
            tc2$Family=orders
            tc2$Scn="Current.85_50"
            tc2$Scenarios="Current to RCP 8.5 Year 2050"
            
            tc3 <- as.data.frame(RangeSize.Current.45_70$Compt.By.Models)
            tc3$Species=stringr::str_replace(especie,"_"," ")
            tc3$Family=orders
            tc3$Scn="Current.45_70"
            tc3$Scenarios="Current to RCP 4.5 Year 2070"
            
            tc4 <- as.data.frame(RangeSize.Current.45_50$Compt.By.Models)
            tc4$Species=stringr::str_replace(especie,"_"," ")
            tc4$Family=orders
            tc4$Scn="Current.45_50"
            tc4$Scenarios="Current to RCP 4.5 Year 2050"
            
            tc5 <- as.data.frame(RangeSize.Current.2050$Compt.By.Models)
            tc5$Species=stringr::str_replace(especie,"_"," ")
            tc5$Family=orders
            tc5$Scn="Current.2050"
            tc5$Scenarios="Current to Year 2050"
            
            tc6 <- as.data.frame(RangeSize.Current.2070$Compt.By.Models)
            tc6$Species=stringr::str_replace(especie,"_"," ")
            tc6$Family=orders
            tc6$Scn="Current.2070"
            tc6$Scenarios="Current to Year 2070"
            
            RangechangeTotal <- rbind(tc8,tc7,tc5,tc6,tc1,tc2,tc3,tc4)
            write.csv(RangechangeTotal,paste(wd, "/", especie ,"_10CV_RangeChange_Master.csv", sep="" ))
            RangechangeTotalGraph <- RangechangeTotal
            
            
            write.csv(RangeSize.Current.85_70$Compt.By.Models,paste(wd, "/", especie ,"_RangeSize.Current.85_70.csv", sep="" ))
            write.csv(RangeSize.Current.85_50$Compt.By.Models,paste(wd, "/", especie ,"_RangeSize.Current.85_50.csv", sep="" ))
            write.csv(RangeSize.Current.45_70$Compt.By.Models,paste(wd, "/", especie ,"_RangeSize.Current.45_70.csv", sep="" ))
            write.csv(RangeSize.Current.45_50$Compt.By.Models,paste(wd, "/", especie ,"_RangeSize.Current.45_50.csv", sep="" ))
            write.csv(RangeSize.Current.2050$Compt.By.Models,paste(wd, "/", especie ,"_RangeSize.Current.2050.csv", sep="" ))
            write.csv(RangeSize.Current.2070$Compt.By.Models,paste(wd, "/", especie ,"_RangeSize.Current.2070.csv", sep="" ))
            write.csv(RangeSize.MID.Current$Compt.By.Models,paste(wd, "/", especie ,"_RangeSize.MID.Current.csv", sep="" ))
            write.csv(RangeSize.LGM.MID$Compt.By.Models,paste(wd, "/", especie ,"_RangeSize.LGM.MID.csv", sep="" ))
            
            
            writeRaster(Proj.Current.Binary.Ind, filename=paste(WorkDir,"/Finals/Current/Proj_",especie,"_Current.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(Proj.85_70.Binary.Ind, filename=paste(WorkDir,"/Finals/85_70/Proj_",especie,"_85_70.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(Proj.85_50.Binary.Ind, filename=paste(WorkDir,"/Finals/85_50/Proj_",especie,"_85_50.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(Proj.45_70.Binary.Ind, filename=paste(WorkDir,"/Finals/45_70/Proj_",especie,"_45_70.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(Proj.45_50.Binary.Ind, filename=paste(WorkDir,"/Finals/45_50/Proj_",especie,"_45_50.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(Proj.2050.Binary.Ind, filename=paste(WorkDir,"/Finals/2050/Proj_",especie,"_2050.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(Proj.2070.Binary.Ind, filename=paste(WorkDir,"/Finals/2070/Proj_",especie,"_2070.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(Proj.LGM.Binary.Ind, filename=paste(WorkDir,"/Finals/LGM/Proj_",especie,"_LGM.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(Proj.MID.Binary.Ind, filename=paste(WorkDir,"/Finals/MID/Proj_",especie,"_MID.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            writeRaster(SpClimRefugia, filename=paste(WorkDir,"/Finals/ClimRefugia/Proj_",especie,"_ClimRefugia.tiff", sep="" ), format="GTiff",options="COMPRESS=LZW", datatype='INT4S', overwrite=TRUE)
            
            endCluster()
            
            gc()
            
            #########################################Map Plotting#################################
            dir.create(paste(wd,"/jpegMAPS",sep=''))
            cat("Printing Maps...")
            cat("\n")
            
            
            pal20 <- c("#FFFFFF", "#15702e")
            map.CurrentMean <- 
               tm_shape(pred.Current.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nCurrent",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.CurrentSD <- 
               tm_shape(pred.Current.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nCurrent:",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.CurrentBinary <- 
               tm_shape(pred.Current.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nCurrent",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.PA<-tmap_arrange(map.CurrentMean, map.CurrentSD,map.CurrentBinary,outer.margins = 0.02)
            
            tmap_save( tm=map.CurrentMean,filename=paste(wd, "/jpegMAPS/", especie ,"_Current_Mean.jpg", sep="" ))
            tmap_save( tm=map.CurrentSD,filename=paste(wd, "/jpegMAPS/", especie ,"_Current_SD.jpg", sep="" ),)
            tmap_save( tm=map.CurrentBinary,filename=paste(wd, "/jpegMAPS/", especie ,"_Current_Binary.jpg", sep="" ),)
            tmap_save( tm=map.PA,filename=paste(wd, "/jpegMAPS/", especie ,"_Current_All.jpg", sep="" ),width=6036, height=2184)
            
            
            
            
            
            map.85_70.Mean <- 
               tm_shape(pred.85_70.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n RCP 8.5, 2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.85_70.SD <- 
               tm_shape(pred.85_70.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.85_70.Binary <- 
               tm_shape(pred.85_70.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.85_70.<-tmap_arrange(map.85_70.Mean, map.85_70.SD,map.85_70.Binary,outer.margins = 0.02)
            
            tmap_save( tm=map.85_70.Mean,filename=paste(wd, "/jpegMAPS/", especie ,"_85_70_Mean.jpg", sep="" ))
            tmap_save( tm=map.85_70.SD,filename=paste(wd, "/jpegMAPS/", especie ,"_85_70_SD.jpg", sep="" ),)
            tmap_save( tm=map.85_70.Binary,filename=paste(wd, "/jpegMAPS/", especie ,"_85_70_Binary.jpg", sep="" ),)
            tmap_save( tm=map.85_70.,filename=paste(wd, "/jpegMAPS/", especie ,"_85_70_All.jpg", sep="" ),width=6036, height=2184)
            
            
            
            
            
            
            map.85_50.Mean <- 
               tm_shape(pred.85_50.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.85_50.SD <- 
               tm_shape(pred.85_50.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.85_50.Binary <- 
               tm_shape(pred.85_50.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.85_50.<-tmap_arrange(map.85_50.Mean, map.85_50.SD,map.85_50.Binary,outer.margins = 0.02)
            
            tmap_save( tm=map.85_50.Mean,filename=paste(wd, "/jpegMAPS/", especie ,"_85_50_Mean.jpg", sep="" ))
            tmap_save( tm=map.85_50.SD,filename=paste(wd, "/jpegMAPS/", especie ,"_85_50_SD.jpg", sep="" ),)
            tmap_save( tm=map.85_50.Binary,filename=paste(wd, "/jpegMAPS/", especie ,"_85_50_Binary.jpg", sep="" ),)
            tmap_save( tm=map.85_50.,filename=paste(wd, "/jpegMAPS/", especie ,"_85_50_All.jpg", sep="" ),width=6036, height=2184)
            
            
            
            
            
            
            map.45_70.Mean <- 
               tm_shape(pred.45_70.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.45_70.SD <- 
               tm_shape(pred.45_70.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.45_70.Binary <- 
               tm_shape(pred.45_70.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.45_70.<-tmap_arrange(map.45_70.Mean, map.45_70.SD,map.45_70.Binary,outer.margins = 0.02)
            
            tmap_save( tm=map.45_70.Mean,filename=paste(wd, "/jpegMAPS/", especie ,"_45_70_Mean.jpg", sep="" ))
            tmap_save( tm=map.45_70.SD,filename=paste(wd, "/jpegMAPS/", especie ,"_45_70_SD.jpg", sep="" ),)
            tmap_save( tm=map.45_70.Binary,filename=paste(wd, "/jpegMAPS/", especie ,"_45_70_Binary.jpg", sep="" ),)
            tmap_save( tm=map.45_70.,filename=paste(wd, "/jpegMAPS/", especie ,"_45_70_All.jpg", sep="" ),width=6036, height=2184)
            
            
            
            
            
            
            map.45_50.Mean <- 
               tm_shape(pred.45_50.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.45_50.SD <- 
               tm_shape(pred.45_50.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.45_50.Binary <- 
               tm_shape(pred.45_50.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.45_50.<-tmap_arrange(map.45_50.Mean, map.45_50.SD,map.45_50.Binary,outer.margins = 0.02)
            
            tmap_save( tm=map.45_50.Mean,filename=paste(wd, "/jpegMAPS/", especie ,"_45_50_Mean.jpg", sep="" ))
            tmap_save( tm=map.45_50.SD,filename=paste(wd, "/jpegMAPS/", especie ,"_45_50_SD.jpg", sep="" ),)
            tmap_save( tm=map.45_50.Binary,filename=paste(wd, "/jpegMAPS/", especie ,"_45_50_Binary.jpg", sep="" ),)
            tmap_save( tm=map.45_50.,filename=paste(wd, "/jpegMAPS/", especie ,"_45_50_All.jpg", sep="" ),width=6036, height=2184)
            
            map.LGM.Mean <- 
               tm_shape(pred.LGM.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nLGM",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.LGM.SD <- 
               tm_shape(pred.LGM.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nLGM",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.LGM.Binary <- 
               tm_shape(pred.LGM.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nLGM",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.LGM<-tmap_arrange(map.LGM.Mean, map.LGM.SD,map.LGM.Binary,outer.margins = 0.02)
            
            tmap_save( tm=map.LGM.Mean,filename=paste(wd, "/jpegMAPS/", especie ,"_LGM_Mean.jpg", sep="" ))
            tmap_save( tm=map.LGM.SD,filename=paste(wd, "/jpegMAPS/", especie ,"_LGM_SD.jpg", sep="" ),)
            tmap_save( tm=map.LGM.Binary,filename=paste(wd, "/jpegMAPS/", especie ,"_LGM_Binary.jpg", sep="" ),)
            tmap_save( tm=map.LGM,filename=paste(wd, "/jpegMAPS/", especie ,"_LGM_All.jpg", sep="" ),width=6036, height=2184)
            
            
            
            map.MID.Mean <- 
               tm_shape(pred.MID.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nMID",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.MID.SD <- 
               tm_shape(pred.MID.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nMID",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.MID.Binary <- 
               tm_shape(pred.MID.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nMID",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.MID<-tmap_arrange(map.MID.Mean, map.MID.SD,map.MID.Binary,outer.margins = 0.02)
            
            tmap_save( tm=map.MID.Mean,filename=paste(wd, "/jpegMAPS/", especie ,"_MID_Mean.jpg", sep="" ))
            tmap_save( tm=map.MID.SD,filename=paste(wd, "/jpegMAPS/", especie ,"_MID_SD.jpg", sep="" ),)
            tmap_save( tm=map.MID.Binary,filename=paste(wd, "/jpegMAPS/", especie ,"_MID_Binary.jpg", sep="" ),)
            tmap_save( tm=map.MID,filename=paste(wd, "/jpegMAPS/", especie ,"_MID_All.jpg", sep="" ),width=6036, height=2184)
            
            
            map.2070.Mean <- 
               tm_shape(pred.2070.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.2070.SD <- 
               tm_shape(pred.2070.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.2070.Binary <- 
               tm_shape(pred.2070.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.2070<-tmap_arrange(map.2070.Mean, map.2070.SD,map.2070.Binary,outer.margins = 0.02)
            
            tmap_save( tm=map.2070.Mean,filename=paste(wd, "/jpegMAPS/", especie ,"_2070_Mean.jpg", sep="" ))
            tmap_save( tm=map.2070.SD,filename=paste(wd, "/jpegMAPS/", especie ,"_2070_SD.jpg", sep="" ),)
            tmap_save( tm=map.2070.Binary,filename=paste(wd, "/jpegMAPS/", especie ,"_2070_Binary.jpg", sep="" ),)
            tmap_save( tm=map.2070,filename=paste(wd, "/jpegMAPS/", especie ,"_2070_All.jpg", sep="" ),width=6036, height=2184)
            
            map.2050.Mean <- 
               tm_shape(pred.2050.mean,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Suitability", palette = "BuGn", n = 5, contrast = c(0.3, 0.94)) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.2050.SD <- 
               tm_shape(pred.2050.sd,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "SD", palette = "Reds") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.2050.Binary <- 
               tm_shape(pred.2050.binary,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = "Dark2", n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(b) +
               tm_borders(col="blue") +
               tm_add_legend('line', col = "blue", title="Calibration") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.2050<-tmap_arrange(map.2050.Mean, map.2050.SD,map.2050.Binary,outer.margins = 0.02)
            
            tmap_save( tm=map.2050.Mean,filename=paste(wd, "/jpegMAPS/", especie ,"_2050_Mean.jpg", sep="" ))
            tmap_save( tm=map.2050.SD,filename=paste(wd, "/jpegMAPS/", especie ,"_2050_SD.jpg", sep="" ),)
            tmap_save( tm=map.2050.Binary,filename=paste(wd, "/jpegMAPS/", especie ,"_2050_Binary.jpg", sep="" ),)
            tmap_save( tm=map.2050,filename=paste(wd, "/jpegMAPS/", especie ,"_2050_All.jpg", sep="" ),width=6036, height=2184)
            
            
            
            map.all.futuremain<-tmap_arrange(map.LGM.Binary,map.MID.Binary,map.CurrentBinary, map.2050.Binary,map.2070.Binary, outer.margins = 0.02)
            tmap_save( tm=map.all.futuremain,filename=paste(wd, "/jpegMAPS/", especie ,"_All_Future_main.jpg", sep="" ),width=6036, height=2184)
            
            
            
            map.all.future<-tmap_arrange(map.CurrentBinary, map.45_50.Binary,map.45_70.Binary, map.85_50.Binary,map.85_70.Binary,outer.margins = 0.02)
            tmap_save( tm=map.all.future,filename=paste(wd, "/jpegMAPS/", especie ,"_All_Future.jpg", sep="" ),width=6036, height=2184)
            
            map.Proj.Current.Binary.Ind <- 
               tm_shape(Proj.Current.Binary.Ind,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nCurrent",sep="")) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nCurrent",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            
            map.Proj.85_70.Binary.Ind <- 
               tm_shape(Proj.85_70.Binary.Ind,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2070",sep="")) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            
            map.Proj.85_50.Binary.Ind <- 
               tm_shape(Proj.85_50.Binary.Ind,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2050",sep="")) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 8.5, 2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            
            map.Proj.45_70.Binary.Ind <- 
               tm_shape(Proj.45_70.Binary.Ind,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2070",sep="")) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            
            map.Proj.45_50.Binary.Ind <- 
               tm_shape(Proj.45_50.Binary.Ind,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2050",sep="")) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nRCP 4.5, 2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            
            map.Proj.2050.Binary.Ind <- 
               tm_shape(Proj.2050.Binary.Ind,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2050",sep="")) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2050",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            
            map.Proj.2070.Binary.Ind <- 
               tm_shape(Proj.2070.Binary.Ind,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2070",sep="")) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\n2070",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            
            map.Proj.LGM.Binary.Ind <- 
               tm_shape(Proj.LGM.Binary.Ind,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "0/1, Binary",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nLast Glacial Maximum",sep="")) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nLGM",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            ## Not run: 
            
            map.Proj.MID.Binary.Ind <- 
               tm_shape(Proj.MID.Binary.Ind,is.master = TRUE) +
               tm_raster(legend.show = TRUE, title = "0/1, Binday",palette = pal20, n = 2,style="cat",colorNULL = NULL,colorNA=NULL) +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.3, alpha = 0.3,colorNA=NULL) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black", alpha = 0.3,border.alpha = 0.3, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nMid Holocene",sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            map.ALL.ISR<-tmap_arrange(map.Proj.LGM.Binary.Ind,map.Proj.MID.Binary.Ind,map.Proj.Current.Binary.Ind,map.Proj.2050.Binary.Ind,map.Proj.2070.Binary.Ind,map.SpClimRefugia,outer.margins = 0.02)
            tmap_save( tm=map.ALL.ISR,filename=paste(wd, "/jpegMAPS/", especie ,"_Binary_ALL_ISR.jpg", sep="" ),width=6036, height=2184)
            
            tmap_save( tm=map.Proj.Current.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_Current_Binary_ISR.jpg", sep=""))
            tmap_save( tm=map.Proj.MID.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_MID_Binary_ISR.jpg", sep=""))
            tmap_save( tm=map.Proj.LGM.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_LGM_Binary_ISR.jpg", sep=""))
            tmap_save( tm=map.Proj.2050.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_2050_Binary_ISR.jpg", sep=""))	
            tmap_save( tm=map.Proj.2070.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_2070_Binary_ISR.jpg", sep=""))
            tmap_save( tm=map.Proj.85_70.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_85_70_Binary_ISR.jpg", sep=""))
            tmap_save( tm=map.Proj.85_50.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_85_50_Binary_ISR.jpg", sep=""))
            tmap_save( tm=map.Proj.45_70.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_45_70_Binary_ISR.jpg", sep=""))
            tmap_save( tm=map.Proj.45_50.Binary.Ind,filename=paste(wd, "/jpegMAPS/", especie ,"_45_50_Binary_ISR.jpg", sep=""))	
            
            
            
            #############        
            save.image(file=paste(wd, "/", especie ,"_Full.Rdata", sep="" ))
            write.csv(pres.data.clean,paste(wd, "/", especie ,"_Done", sep="" ))
            write.csv(pres.data.clean,paste(wd, "/", especie ,"_Processed", sep="" ))
            
            endCluster()
            
            gc()
            files <- list.files(tmpdir_name, full.names = T, pattern = "*")
            file.remove(files)
            
         }
         
         
         
         
         
         else {
            write.csv(pres.data,paste(wd, "/", especie ,"_NotDone_LessThan30", sep="" ))
            write.csv(pres.data,paste(wd, "/", especie ,"_Processed", sep="" ))
            
            dir.create(paste(wd,"/jpegMAPS",sep=''))
            
            coordinates(pres.data.or) <- ~longitude + latitude
            
            map.CleanedLoc <- 
               tm_shape(template,is.master = TRUE)+
               tm_raster(legend.show = TRUE, title = "Elevation") +
               tm_shape(ind) +
               tm_borders(col = "black") + 
               tm_add_legend('line', col = "black", title="India") +
               tm_shape(BL_Map) +
               tm_polygons(col = "grey", border.col = "black",border.alpha = 0.7, alpha = 0.7) +
               tm_add_legend(type ='fill', col = "grey",border.col = "black",border.alpha = 0.7, title=paste("Birdlife 2020 \n", stringr::str_replace(BL_SN_Name,"_"," ") , sep="" )) +
               tm_shape(pres.data.or) +
               tm_dots(size = 0.1, shape = 21, col = "blue") +
               tm_graticules() +
               tm_layout(title = paste(stringr::str_replace(especie,"_"," "),"\nPresence: ",nrow(pres.data.or),sep="")) +
               tm_layout(legend.outside = TRUE) +
               tm_layout(legend.title.size = 0.8) +
               tm_credits("test", size = 0.7, position = c(0.01, 0.01)) +
               tm_compass(type = "8star", position = c("left", "bottom"),size = 2) +
               tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
            
            tmap_save( tm=map.CleanedLoc,filename=paste(wd, "/jpegMAPS/", especie ,"_Presence.jpg", sep="" ),)
            
            #############        
            save.image(file=paste(wd, "/", especie ,"_Full.Rdata", sep="" ))
            write.csv(pres.data.or,paste(wd, "/", especie ,"_Done", sep="" ))
            
            
            endCluster()
            
            gc()
            files <- list.files(tmpdir_name, full.names = T, pattern = "*")
            file.remove(files)
         }
         
         
      }    
      
   }
}


# endCluster()


