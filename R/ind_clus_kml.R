#' @title Plot individual cluster .kml
#'
#' @description Uses results from 'GPSeq_clus" to plot individual cluster .kmls
#'
#' @param AID Desired AID from sequential cluster output
#' @param cn Desired cluster number
#' @param locs Location dataframe output from GPSeq_clus(), default is 'dat'
#' @param cs Cluster summary output from GPSeq_clus(), default is 'clus_summary'
#' @param centroid_calc 'mean' (default) or 'median' centroid plot
#' @param overwrite TRUE (default) labels output as "ind.kml" that overwrites with each run within tempdir(). FALSE saves outputs as "AID_cn"
#' @param dir File path when saving output
#'
#' @return Opens the cluster locations and centroid .kml for assessment.
#' @export
#'
#' @examples
#' \donttest{
#' GPSeq_clus(dat = ML_ex_dat[1:50,], search_radius_m = 200, window_days = 6,
#'            clus_min_locs = 3, show_plots = c(FALSE, "mean"))
#' ind_clus_kml(AID = "ML1605M", cn = 4)
#' }
#' \donttest{
#' GPSeq_clus(dat = ML_ex_dat[1:50,], search_radius_m = 200, window_days = 6,
#'            clus_min_locs = 3, show_plots = c(FALSE, "mean"))
#' ind_clus_kml(AID = "ML1605M", cn = 4, centroid_calc = "median", overwrite = FALSE, dir= tempdir())
#' }
#'
ind_clus_kml<- function(AID, cn, locs=dat, cs=clus_summary, centroid_calc= "mean", overwrite= TRUE, dir= NULL){
  store_dir<-getwd()
  on.exit(setwd(store_dir))
  if(AID %in% cs$AID == FALSE){stop(paste("AID", AID, "not found", sep=" "))}
  if(length(cn)!=1){stop("'ind_clus_kml()' only accepts individual clusters")}
  if(cn %in% cs[which(cs$AID == AID), "clus_ID"] == FALSE){stop(paste("Cluster", cn, "does not exist for", AID, sep=" "))}
  if((!is.na(centroid_calc) && !is.null(centroid_calc) && (centroid_calc == "mean" | centroid_calc == "median"))==FALSE){stop("'centroid_calc' argument must = 'median' or 'mean'")}
  if((!is.na(overwrite) && !is.null(overwrite) && (overwrite == TRUE | overwrite == FALSE))==FALSE){stop("'overwrite' argument must = 1/T/TRUE or 0/F/FALSE")}
  ind_clus<-locs[which(locs$AID == AID & locs$clus_ID == cn),]
  ind_clus<-ind_clus[which(!is.na(ind_clus$Lat)),]
  ind_clus$AID2<-""
  ind_clus<-ind_clus[,c("AID2", "AID", "clus_ID", "TelemDate", "Long", "Lat")]
  clus_g_c<-cs[which(cs$AID == AID & cs$clus_ID==cn),]
  if(centroid_calc=="median"){
    spgeo<-sp::SpatialPointsDataFrame(matrix(c(clus_g_c$g_med_Long, clus_g_c$g_med_Lat), ncol=2), clus_g_c, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  } else {
    spgeo<-sp::SpatialPointsDataFrame(matrix(c(clus_g_c$g_c_Long, clus_g_c$g_c_Lat), ncol=2), clus_g_c, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  }
  aa<-ind_clus[1,]
  aa$AID2<-"Centroid"
  aa$TelemDate<-ind_clus$TelemDate[nrow(ind_clus)]+25200 #this adds a generic time to the end of the cluster
  aa$Lat<-spgeo$coords.x2
  aa$Long<-spgeo$coords.x1
  ind_clus<-rbind(ind_clus, aa)
  if(overwrite==TRUE){
    f_name<-"ind.kml"
    setwd(tempdir())
  } else {
    f_name<-paste(AID, "_", cn, ".kml", sep="")
    setwd(dir)
  }
  sp<- sp::SpatialPoints(ind_clus[,c("Long","Lat")]) #set up spatial points from out data
  sp::proj4string(sp) <- sp::CRS("+proj=longlat +datum=WGS84") #correct projection
  ST<- spacetime::STIDF(sp=sp,time=ind_clus$TelemDate, data=ind_clus)  #create a spacetime layer
  ind_clus[which(ind_clus$AID2 == "Centroid"), "TelemDate"]<- NA
  plotKML::plotKML(obj=ST, folder.name=paste(AID, "_", cn, sep="") , file.name=f_name,
          size= .45,
          colour=,
          points_names= paste(ind_clus$AID2,ind_clus$AID,ind_clus$clus_ID,ind_clus$TelemDate,sep=" "),
          metadata=NULL,
          open.kml=TRUE,
          LabelScale=0.6)
  rm(spgeo, sp, ST, aa, ind_clus, f_name, clus_g_c)
}
