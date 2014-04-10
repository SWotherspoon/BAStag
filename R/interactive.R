##' Interactively select data subsets
##'
##' Interactively select data subsets based on the light profile.  An
##' image represeting the recorded light levels over time displayed,
##' and the user can select contiguous subsets of data with a mouse
##' drag.
##'
##' Higher time resolution images centred on the endpoints of the
##' selected subset are shown in a second window.  The left and right
##' and down and up arrow keys can be used to adjust the selected
##' endpoints by 24 hours for fine scale adjustment.
##'
##' Once a suitable subset is selected, the selection is accepted by
##' depressing the 'a' key, and new subset can be selected.
##'
##' Each twilight may be marked with the an integer 0 to 9 with the
##' numeric keys. By default each day is given the mark 0.
##'
##' In either window
##' \tabular{ll}{
##' 'q' \tab Quits, returning the dataframe selected subsets \cr
##' 'a' \tab Accepts the candidate selection \cr
##' '+'/'-' \tab Adjust zoom scale of high resolution images\cr
##' 'Left/Right arrow' \tab Adjust selected start point \cr
##' 'Down/Up arrow' \tab Adjust selected end point \cr
##' }
##'
##' @title Subset selection
##' @param tagdata a datframe with columns \code{Date} and
##' \code{Light} that are the sequence of sample times (as POSIXct)
##' and light levels recorded by the tag.
##' @param offset the starting hour for the vertical axes.
##' @param extend the period (in days) before and after the endpoints of the selection to be shown in the zoom window.
##' @param lmax the maximum light level to plot.
##' @param width width of the interface windows.
##' @param height height of the interface windows.
##' @return the dataframe of the selected subsets
##' \item{\code{Start}}{start time of subset}
##' \item{\code{End}}{end time of subset}
##' @export
select.subset <- function(tagdata,offset=0,extend=6,lmax=64,
                          width=10,height=5) {

  ## Round down to nearest offset
  floorDate <- function(date) {
    date - ((as.hour(date)-offset)%%24)*60*60
  }

  ## Round up to nearest offset
  ceilingDate <- function(date) {
    date + ((offset-as.hour(date))%%24)*60*60
  }

  minDate <- floorDate(min(tagdata$Date))
  maxDate <- ceilingDate(max(tagdata$Date))
  startDate <- minDate
  endDate <- maxDate
  subsets <- data.frame(Start=NULL,End=NULL)

  ## Select device
  devset <- function(dev) {
    if(dev.cur()!=dev) dev.set(dev)
  }
  ## Focus if possible
  focus <- if(exists("bringToTop",mode="function")) bringToTop else devset

  ## Compute the zoom range
  zoom.range <-  function(date) {
    start <- floorDate(max(date - extend*24*60*60,minDate))
    end <- ceilingDate(min(start+2*extend*24*60*60,maxDate))
    start <- floorDate(max(end - 2*extend*24*60*60,minDate))
    c(start=start,end=end)
  }

  ## Draw the selection window
  slct.draw <- function() {
    ## Plot image
    light.image(tagdata,offset=offset,lmax=lmax)
  }

  ## Draw the selection rectangle
  rect.draw <- function() {
    ## Selection rectangles
    rx <- grconvertX(c(0,1),from="npc",to="user")
    ry <- grconvertY(c(1.01,1.03),from="npc",to="user")
    rect(rx[1],ry[1],rx[2],ry[2],border=NA,col="white",xpd=NA)
    for(k in seq_len(nrow(subsets)))
      rect(subsets$Start,ry[1],subsets$End,ry[2],border=NA,col="blue",xpd=NA)
    if(!is.null(startDate) && !is.null(endDate) && endDate!=startDate)
      rect(startDate,ry[1],endDate,ry[2],border=NA,col="red",xpd=NA)
  }

  ## Draw zoomed ends
  zoom.draw <- function() {
    if(!is.null(startDate) && !is.null(endDate)) {
      par(mfrow=c(1,2))
      light.image(tagdata,offset=offset,lmax=lmax,xlim=zoom.range(startDate))
      abline(v=startDate,col="red")
      light.image(tagdata,offset=offset,lmax=lmax,xlim=zoom.range(endDate))
      abline(v=endDate,col="red")
    }
  }

  ## onMouseDown callback for selection window.
  slctOnMouseDown <- function(buttons,x,y) {
    ## Determine selected profile.
    devset(slct.dev)
    date <- floorDate(.POSIXct(grconvertX(x,from="ndc",to="user")))
    date <- min(max(date,minDate),maxDate)
    startDate <<- date
    endDate <<- date
    ## Redraw selection rectangles
    devset(slct.dev)
    rect.draw()
    NULL
  }

  ## onMouseMove callback for selection window
  slctOnMouseMove <- function(buttons,x,y) {
    ## Button 1 drag to select range
    if(length(buttons) > 0 && buttons[1]==0) {
      date <- ceilingDate(.POSIXct(grconvertX(x,from="ndc",to="user")))
      date <- min(max(date,minDate),maxDate)
      if(date < startDate) {
        endDate <<- startDate
        startDate <<- date
      } else {
        endDate <<- date
      }
      devset(slct.dev)
      rect.draw()
    }
    NULL
  }

  ## onMouseUp callback for selection window
  slctOnMouseUp <- function(buttons,x,y) {
    ## Button 1 drag to select range
    if(length(buttons)==0) {
      date <- ceilingDate(.POSIXct(grconvertX(x,from="ndc",to="user")))
      date <- min(max(date,minDate),maxDate)
      if(date < startDate) {
        endDate <<- startDate
        startDate <<- date
      } else {
        endDate <<- date
      }
      devset(slct.dev)
      rect.draw()
      devset(zoom.dev)
      zoom.draw()
    }
    NULL
  }

  ## onKeybd callback for both windows
  onKeybd <- function(key) {
    ## q quits
    if(key=="q") return(-1)
    ## +/- : zoom time window around selection
    if(key=="+") {
      extend <<- max(extend-1,1)
    }
    if(key=="-") {
      extend <<- min(extend+1,20)
    }
    ## Left/Right - move start point
    if(key=="Left") {
      startDate <<- floorDate(max(startDate - 24*60*60,minDate))
    }
    if(key=="Right") {
      startDate <<- floorDate(min(startDate + 24*60*60,maxDate))
    }
    ## Down/Up - move end point
    if(key=="Down") {
      endDate <<- ceilingDate(max(endDate - 24*60*60,minDate))
    }
    if(key=="Up") {
      endDate <<- ceilingDate(min(endDate + 24*60*60,maxDate))
    }
    ## a : accept current selection
    if(key=="a") {
      subsets <<- rbind(subsets,data.frame(Start=startDate,End=endDate))
      startDate <<- NULL
      endDate <<- NULL
    }

    ## Redraw
    devset(slct.dev)
    rect.draw()
    devset(zoom.dev)
    zoom.draw()
    NULL
  }

  ## Set up master window
  X11(width=width,height=height)
  slct.draw()
  rect.draw()
  slct.dev <- dev.cur()
  focus(slct.dev)
  setGraphicsEventHandlers(
    which=slct.dev,
    prompt="Select Subset",
    onMouseDown=slctOnMouseDown,
    onMouseMove=slctOnMouseMove,
    onMouseUp=slctOnMouseUp,
    onKeybd=onKeybd)
  ## Set up zoom window
  X11(width=width,height=height)
  zoom.dev <- dev.cur()
  zoom.draw()
  setGraphicsEventHandlers(
    which=zoom.dev,
    prompt="Endpoint",
    onKeybd=onKeybd)
  ## Monitor for events
  tryCatch({
    getGraphicsEvent()
      dev.off(slct.dev)
      dev.off(zoom.dev)
      subsets
    }, finally=subsets)
}








##' Interactively edit twilight times
##'
##' Interactively edit times of twilight based on the light profile.
##' A plot of the estimated sunrise and sunset intervals is displayed,
##' and the user can select the twilight to be edited with a left
##' mouse click.
##'
##' The light profile for the selected twilight is shown in a separate
##' window, and the selected time of twilight is highlighted.  The
##' corresponding light profiles from the preceeding and following
##' days are also shown for reference.
##'
##' The user may select a new candidate twilight by left clicking with
##' the mouse, or the twilight can marked for deletion with the right
##' mouse button.
##'
##' Note however, no actual change to the selection is made until the
##' candidate edits are accepted depressing the 'a' key.
##'
##' Each twilight may be marked with the an integer 0 to 9 with the
##' numeric keys. By default each day is given the mark 0.
##'
##' In either window
##' \tabular{ll}{
##' 'q' \tab Quits, returning the dataframe of edited twilight segments \cr
##' 'a' \tab Accepts the candidate edit \cr
##' 'x' \tab Resets the selection \cr
##' 'p'
##' \tab Toggles the display of individual points \cr
##' '+'/'-' \tab Zoom in or out \cr
##' 'Left arrow' \tab Jump to previous twilight \cr
##' 'Right arrow' \tab Jump to ext twilight \cr
##' '0'-'9' \tab Mark this twilight \cr
##' }
##'
##' @title Edit twilights
##' @param tagdata a datframe with columns \code{Date} and
##' \code{Light} that are the sequence of sample times (as POSIXct)
##' and light levels recorded by the tag.
##' @param twilights dataframe of twilight times as generated by
##' \code{\link{find.twilights}}.
##' @param offset the starting hour for the vertical axes.
##' @param extend the period (in hours) before and after twilight for
##' which the light profile should be plotted.
##' @param threshold the light threshold that defines twilight.
##' @param lmax the maximum light level to plot.
##' @param twilight.col the colors of the estimated sunrise and sunset times.
##' @param light.col the colors of the light profiles for the day
##' before, the selected twilight and the day after.
##' @param threshold.col the colors of the threshold markers.
##' @param point.cex expansion factor for plot points.
##' @param width width of the interface windows.
##' @param height height of the interface windows.
##' @seealso \code{\link{crepuscular.editW}}
##' @return the dataframe of edited twilights, with columns
##' \item{\code{Twilight}}{edited times of twilight}
##' \item{\code{Rise}}{logical indicating sunrise}
##' \item{\code{Original}}{original times of twilight}
##' @export
twilight.editW <- function(tagdata,twilights,offset=0,extend=6,threshold=NULL,lmax=64,
                           twilight.col=c("dodgerblue","firebrick","grey80"),
                           light.col=c("#AAFFAA","black","#AAAAFF"),
                           threshold.col=c("#FFAAAA","grey90"),point.cex=0.5,width=10,height=5) {

  ## Extract date and light
  date <- tagdata$Date
  light <- tagdata$Light
  ## Order twilights and check deleted
  twilights <- twilights[order(twilights$Twilight),]
  if(is.null(twilights$Deleted)) twilights$Deleted <- logical(nrow(twilights))
  if(is.null(twilights$Marker)) twilights$Marker <- integer(nrow(twilights))
  ## Extract date and hour of twilight
  day <- twilights$Twilight
  hour <- hour.offset(as.hour(twilights$Twilight),offset)

  ## Record original times
  twilights$TwilightOriginal <- twilights$Twilight

  ## Cached data subsets
  index <- 1
  twl <- twilights$Twilight[index]
  marker <- twilights$Marker[index]
  deleted <- twilights$Deleted[index]
  twls <- NULL
  edit.pt <- NULL
  dteA <- dteB <- dteC <- NULL
  lgtA <- lgtB <- lgtC <- NULL

  changed <- FALSE
  show.obs <- FALSE

  ## Set cached values
  cache <- function(k) {
    index <<- k
    twl <<- twilights$Twilight[index]
    deleted <<- twilights$Deleted[index]
    marker <<- twilights$Marker[index]
    keep <- (twilights$Twilight >= twl-3600*extend) & (twilights$Twilight <= twl+3600*extend)
    keep[index] <- FALSE
    twls <<- twilights$Twilight[keep]
    edit.pt <<- NULL
    changed <<- FALSE
    ## Get profiles
    keep <- (date >= twl-86400-3600*extend) & (date <= twl-86400+3600*extend)
    dteA <<- date[keep]
    lgtA <<- light[keep]
    keep <- (date >= twl - 3600*extend) & (date <= twl + 3600*extend)
    dteB <<- date[keep]
    lgtB <<- light[keep]
    keep <- (date >= twl+86400-3600*extend) & (date <= twl+86400+3600*extend)
    dteC <<- date[keep]
    lgtC <<- light[keep]
  }

  ## Select device
  devset <- function(dev) {
    if(dev.cur()!=dev) dev.set(dev)
  }
  ## Focus if possible
  focus <- if(exists("bringToTop",mode="function")) bringToTop else devset

  ## Draw the twilights window
  twlght.draw <- function() {
    plot(day,hour,
         pch=16,cex=point.cex,
         xlab="Date",ylab="Hour",
         ylim=c(offset,offset+24),
         col=twilight.col[ifelse(twilights$Deleted,3,ifelse(twilights$Rise,1,2))])
    points(day[index],hour[index],pch=3)
  }

  ## Draw light profiles
  profile.draw <- function() {
    ## Draw axes for light profiles
    mlab <- if(marker>0) paste("Marker: ",marker) else ""
    plot(dteB,lgtB,ylim=c(0,lmax),xlab=mlab,ylab="Light",type="n",xaxt="n",main=as.character(twl))
    axis.POSIXct(1,x=dteB,format="%H:%M")
    ## Overlay with light threshold
    if(!is.null(threshold))
      abline(h=threshold,col=threshold.col)
    ## Overlay the light profile for the selected and surrounding days
    lines(dteA+86400,lgtA,col=light.col[1])
    lines(dteC-86400,lgtC,col=light.col[3])
    lines(dteB,lgtB,col=light.col[2])
    if(show.obs) points(dteB,lgtB,col=light.col[2],pch=16,cex=point.cex)
    abline(v=twls,col=threshold.col[2])
    abline(v=twl,col=if(!deleted) threshold.col[1] else light.col[2])
    if(changed) points(edit.pt[1],edit.pt[2],pch=16,col=threshold.col[1])
  }


  ## onMouseDown callback for twilights window.
  twlOnMouseDown <- function(buttons,x,y) {
    ## Determine selected profile.
    devset(twlght.dev)
    x <- grconvertX(x,from="ndc",to="user")
    y <- grconvertY(y,from="ndc",to="user")
    r <- ((x-as.numeric(day))/3600)^2+((y-hour+12)%%24-12)^2
    k <- which.min(r)
    ## Redraw
    cache(k)
    devset(twlght.dev)
    twlght.draw()
    devset(profile.dev)
    profile.draw()
    focus(profile.dev)
    NULL
  }

  ## onKeybd callback for both windows
  onKeybd <- function(key) {
    ## q quits
    if(key=="q") return(-1)
    ## +/- : zoom time window around threshold crossing
    if(key=="+") {
      extend <<- max(extend-1,1)
      cache(index)
    }
    if(key=="-") {
      extend <<- min(extend+1,24)
      cache(index)
    }
    ## x : reset selection
    if(key=="x") {
      cache(index)
    }
    ## p : toggle display of points in the profile window
    if(key=="p") {
      show.obs <<- (show.obs==FALSE)
      cache(index)
    }
    ## Left/Right : jump to neighbouring twilight
    if(key=="Left") {
      cache(max(index-1,1))
    }
    if(key=="Right") {
      cache(min(index+1,nrow(twilights)))
    }
    ## a : accept current edit
    if(key=="a") {
      twl <<- edit.pt[1]
      twilights$Twilight[index] <<- .POSIXct(twl,"GMT")
      edit.pt <<- NULL
      changed <<- FALSE
    }
    if(key >= "0" && key <= "9") {
      marker <<- as.numeric(key)
      twilights$Marker[index] <<- marker
    }

    ## Redraw
    devset(twlght.dev)
    twlght.draw()
    devset(profile.dev)
    profile.draw()
    NULL
  }

  ## onMouseDown callback for profile window
  prfOnMouseDown <- function(buttons,x,y) {
    ## Button 1 -> record location
    if(length(buttons) > 0 && buttons[1]==0) {
      changed <<- TRUE
      edit.pt <<- c(grconvertX(x,from="ndc",to="user"),
                    grconvertY(y,from="ndc",to="user"))
    }
    ## Button 2 -> toggle deletion
    if(length(buttons) > 0 && buttons[1]==2) {
      deleted <- (deleted==FALSE)
      twilights$Deleted[index] <<- deleted
      cache(index)
    }
    profile.draw()
    NULL
  }


  ## Set up twilights window
  index <- 1
  cache(index)
  X11(width=width,height=height)
  twlght.draw()
  twlght.dev <- dev.cur()
  focus(twlght.dev)
  setGraphicsEventHandlers(
    which=twlght.dev,
    prompt="Select Twilight",
    onMouseDown=twlOnMouseDown,
    onKeybd=onKeybd)
  ## Set up profile window
  X11(width=width,height=height)
  profile.dev <- dev.cur()
  profile.draw()
  setGraphicsEventHandlers(
    which=profile.dev,
    prompt="Light Profile",
    onMouseDown=prfOnMouseDown,
    onKeybd=onKeybd)
  ## Monitor for events
  tryCatch({
      getGraphicsEvent()
      dev.off(profile.dev)
      dev.off(twlght.dev)
      twilights
  }, finally=twilights)
}



##' Interactively edit crepuscular intervals
##'
##' Interactively edit the crepuscular intervals based on the light
##' profile.  A plot of the estimated sunrise and sunset intervals is
##' displayed, and the user can select the twilight to be edited with
##' a left mouse click.
##'
##' The light profile for the selected twilight is shown in a separate
##' window, and the selected segments of the light profile are
##' highlighted.  The corresponding light profiles from the preceeding
##' and following days are also shown for reference.
##'
##' The user may select a new candidate interval by clicking and
##' dragging with the left mouse button.  Individual points may be
##' selected or deselected with the right mouse button.
##'
##' Note however, no actual change to the selection is made until the
##' candidate edits are accepted depressing the 'a' key.
##'
##' Each twilight may be marked with the an integer 0 to 9 with the
##' numeric keys. By default each day is given the mark 0.
##'
##' In either window
##' \tabular{ll}{
##' 'q' \tab Quits, returning the dataframe of edited twilight segments \cr
##' 'a' \tab Accepts the candidate edit \cr
##' 'x' \tab Resets the selection \cr
##' 'p' \tab Toggles the display of individual points \cr
##' '+'/'-' \tab Zoom in or out \cr
##' 'Left arrow' \tab Jump to previous twilight \cr
##' 'Right arrow' \tab Jump to ext twilight \cr
##' '0'-'9' \tab Mark this twilight \cr
##' }
##'
##' @title Edit crepuscular segments
##' @param tagdata a datframe with columns \code{Date} and
##' \code{Light} that are the sequence of sample times (as POSIXct)
##' and light levels recorded by the tag.
##' @param twilights dataframe of twilight times as generated by
##' \code{\link{find.crepuscular}}.
##' @param offset the starting hour for the vertical axes.
##' @param extend the period (in hours) before and after twilight for
##' which the light profile should be plotted.
##' @param threshold the light threshold that defines twilight.
##' @param lmax the maximum light level to plot.
##' @param twilight.col the colors of the estimated sunrise and sunset intervals.
##' @param light.col the colors of the light profiles for the day
##' before, the selected twilight and the day after.
##' @param selected.col the colors of the selected light data when it
##' has accepted and when it is partially edited.
##' @param threshold.col the color of the threshold markers.
##' @param point.cex expansion factor for plot points.
##' @param width width of the interface windows.
##' @param height height of the interface windows.
##' @seealso \code{\link{twilight.editW}}
##' @return the dataframe of edited twilights, with columns
##' \item{\code{Twilight}}{edited times of twilight}
##' \item{\code{Rise}}{logical indicating sunrise}
##' \item{\code{Start}}{date of first observation in the crepuscular segment}
##' \item{\code{End}}{date of last observation in the crepuscular segment}
##' @export
crepuscular.editW <- function(tagdata,twilights,offset=0,extend=6,threshold=NULL,lmax=64,
                              twilight.col=c("dodgerblue","firebrick","grey80"),
                              light.col=c("#AAFFAA","black","#AAAAFF"),
                              threshold.col="#FFAAAA",selected.col=c("blue","red"),
                              point.cex=0.5,width=10,height=5) {

  ## Extract date and light
  date <- tagdata$Date
  light <- tagdata$Light
  ## Order twilights
  if(is.null(twilights$Marker)) twilights$Marker <- integer(nrow(twilights))
  twilights <- twilights[order(twilights$Twilight,twilights$Start),
                         c("Twilight","Rise","Start","End","Marker")]
  ## Extract date and hour of twilight
  day <- twilights$Twilight
  hour <- hour.offset(as.hour(twilights$Twilight),offset)

  ## Cached data subsets
  index <- 1
  indices <- as.numeric(factor(as.numeric(twilights$Twilight)))
  twl <- NULL
  marker <- 0
  dteA <- dteB <- dteC <- NULL
  lgtA <- lgtB <- lgtC <- NULL
  selected <- NULL
  start <- end <- 0
  changed <- FALSE
  show.obs <- FALSE

  ## Set cached values
  cache <- function(k) {
    index <<- k
    twl <<- twilights$Twilight[which(index==indices)[1]]
    marker <<- twilights$Marker[which(index==indices)[1]]
    changed <<- FALSE
    ## Get profiles
    keep <- (date >= twl-86400-3600*extend) & (date <= twl-86400+3600*extend)
    dteA <<- date[keep]
    lgtA <<- light[keep]
    keep <- (date >= twl - 3600*extend) & (date <= twl + 3600*extend)
    dteB <<- date[keep]
    lgtB <<- light[keep]
    keep <- (date >= twl+86400-3600*extend) & (date <= twl+86400+3600*extend)
    dteC <<- date[keep]
    lgtC <<- light[keep]

    selected <<- logical(length(dteB))
    for(i in which(index==indices))
      selected[dteB >= twilights$Start[i] & dteB <= twilights$End[i]] <<- TRUE
  }

  ## Select device
  devset <- function(dev) {
    if(dev.cur()!=dev) dev.set(dev)
  }
  ## Focus if possible
  focus <- if(exists("bringToTop",mode="function")) bringToTop else devset

  ## Draw the twilights window
  twlght.draw <- function() {
    plot(day,hour,type="n",
         xlab="Date",ylab="Hour",
         ylim=c(offset,offset+24))
    rise <- twilights[twilights$Rise,]
    tsimage.ribbon(.POSIXct(tapply(rise$Start,rise$Twilight,min),"GMT"),
                   .POSIXct(tapply(rise$End,rise$Twilight,max),"GMT"),
                   offset=offset,border=NA,col=twilight.col[1])
    set <- twilights[!twilights$Rise,]
    tsimage.ribbon(.POSIXct(tapply(set$Start,set$Twilight,min),"GMT"),
                   .POSIXct(tapply(set$End,set$Twilight,max),"GMT"),
                   offset=offset,border=NA,col=twilight.col[2])
    points(day[index],hour[index],pch=3)
  }


  ## Draw axes for light profiles
  profile.init <- function() {
    mlab <- if(marker>0) paste("Marker: ",marker) else ""
    plot(dteB,lgtB,ylim=c(0,lmax),xlab=mlab,ylab="Light",type="n",xaxt="n",main=as.character(twl))
    axis.POSIXct(1,x=dteB,format="%H:%M")
  }

  ## Draw light profiles
  profile.draw <- function() {
    ## Overlay light threshold
    if(!is.null(threshold))
      abline(h=threshold,col=threshold.col)
    ## Overlay the light profile for the selected and surrounding days
    lines(dteA+86400,lgtA,col=light.col[1])
    lines(dteC-86400,lgtC,col=light.col[3])
    lines(dteB,lgtB,col=light.col[2])
    if(show.obs) points(dteB,lgtB,col=light.col[2],pch=16,cex=point.cex)

    ## Show selection
    col <- if(!changed) selected.col[1] else selected.col[2]
    ## Hightlight selected segments
    x <- ifelse(selected,dteB,NA)
    y <- ifelse(selected,lgtB,NA)
    lines(x,y,col=col)
    if(show.obs) points(x,y,pch=16,cex=point.cex,col=col)
    ## Selection rectangles
    rx <- grconvertX(c(0,1),from="npc",to="user")
    ry <- grconvertY(c(1.01,1.03),from="npc",to="user")
    rect(rx[1],ry[1],rx[2],ry[2],border=NA,col="white",xpd=NA)
    if(any(selected)) {
      rect(dteB[diff(c(FALSE,selected))==1],ry[1],
           dteB[diff(c(selected,FALSE))==-1],ry[2],
           border=NA,col=col,xpd=NA)
    }
  }


  ## onMouseDown callback for twilights window.
  twlOnMouseDown <- function(buttons,x,y) {
    ## Determine selected profile.
    devset(twlght.dev)
    xs <- grconvertX(c(day,day,day),from="user",to="ndc")
    ys <- grconvertY(c(hour-24,hour,hour+24),from="user",to="ndc")
    k <- (which.min((x-xs)^2+(y-ys)^2)-1)%%length(day)+1
    ## Redraw
    cache(k)
    devset(twlght.dev)
    twlght.draw()
    devset(profile.dev)
    profile.init()
    profile.draw()
    focus(profile.dev)
    NULL
  }

  ## onKeybd callback for both windows
  onKeybd <- function(key) {
    ## q quits
    if(key=="q") return(-1)
    ## +/- : zoom time window around threshold crossing
    if(key=="+") {
      extend <<- max(extend-1,1)
      cache(index)
    }
    if(key=="-") {
      extend <<- min(extend+1,24)
      cache(index)
    }
    ## x : reset selection
    if(key=="x") {
      cache(index)
    }
    ## p : toggle display of points in the profile window
    if(key=="p") {
      show.obs <<- (show.obs==FALSE)
      #cache(index)
    }
    ## Left/Right : jump to neighbouring twilight
    if(key=="Left") {
      cache(max(index-1,1))
    }
    if(key=="Right") {
      cache(min(index+1,max(indices)))
    }
    ## a : accept current edit
    if(key=="a") {
      d <- split(twilights,twilights$Twilight)
      d[[index]] <- cbind(Twilight=d[[index]]$Twilight[1],
                          Rise=d[[index]]$Rise[1],
                          data.frame(Start=dteB[diff(c(FALSE,selected))==1],
                                     End=dteB[diff(c(selected,FALSE))==-1]),
                          Marker=d[[index]]$Marker[1])
      d <- do.call("rbind",d)
      d$Twilight <- .POSIXct(d$Twilight,"GMT")
      d$Start <- .POSIXct(d$Start,"GMT")
      d$End <- .POSIXct(d$End,"GMT")
      twilights <<- d
      indices <<- as.numeric(factor(as.numeric(twilights$Twilight)))
      changed <<- FALSE
    }
    if(key >= "0" && key <= "9") {
      marker <<- as.numeric(key)
      twilights$Marker[which(index==indices)] <<- marker
    }

    ## Redraw
    devset(twlght.dev)
    twlght.draw()
    devset(profile.dev)
    profile.init()
    profile.draw()
    NULL
  }

  ## onMouseDown callback for profile window
  prfOnMouseDown <- function(buttons,x,y) {
    ## Button 1 -> record location and do complete draw
    if(length(buttons) > 0 && buttons[1]==0) {
      changed <<- TRUE
      start <<- grconvertX(x,from="ndc",to="user")
      selected <<- logical(length(dteB))
      end <<- start
      profile.init()
    }
    ## Button 2 -> toggle selected points
    if(length(buttons) > 0 && buttons[1]==2) {
      changed <<- TRUE
      xs <- grconvertX(dteB,from="user",to="ndc")
      ys <- grconvertY(lgtB,from="user",to="ndc")
      k <- which.min((x-xs)^2+(y-ys)^2)
      selected[k] <<- (selected[k]==FALSE)
    }
    profile.draw()
    NULL
  }

  ## onMouseMove callback for profile window
  prfOnMouseMove <- function(buttons,x,y) {
    ## Button 1 drag to select crepuscular period
    if(length(buttons) > 0 && buttons[1]==0) {
      end <<- grconvertX(x,from="ndc",to="user")
      sel <- (dteB >= min(start,end) & dteB <= max(start,end))
      if(any(sel!=selected)) {
        selected <<- sel
        profile.draw()
      }
    }
    NULL
  }

  ## Set up twilights window
  index <- 1
  cache(index)
  X11(width=width,height=height)
  twlght.draw()
  twlght.dev <- dev.cur()
  setGraphicsEventHandlers(
    which=twlght.dev,
    prompt="Select Twilight",
    onMouseDown=twlOnMouseDown,
    onKeybd=onKeybd)
  ## Set up profile window
  X11(width=width,height=height)
  profile.dev <- dev.cur()
  profile.init()
  profile.draw()
  setGraphicsEventHandlers(
    which=profile.dev,
    prompt="Light Profile",
    onMouseDown=prfOnMouseDown,
    onMouseMove=prfOnMouseMove,
    onKeybd=onKeybd)
  focus(twlght.dev)
  ## Monitor for events
  tryCatch({
      getGraphicsEvent()
      dev.off(profile.dev)
      dev.off(twlght.dev)
      twilights
  }, finally=twilights)
}



