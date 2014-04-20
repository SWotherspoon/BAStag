default.palette <- c(red="#E41A1C", blue="#377EB8", green="#4DAF4A", violet="#984EA3",
                     orange="#FF7F00", yellow="#FFFF33", brown="#A65628", pink="#F781BF",
                     black="#000000", grey3="#333333", grey6="#666666", grey9="#999999", greyC="#CCCCCC")


image.draw <- function(tagdata=NULL,twilights=NULL,offset=0,xlim=NULL,
                       point=NULL,ribbon=NULL,mark=NULL,
                       lmax=64,point.cex=0.6,point.col=NULL,
                       palette=default.palette[c(5,2,1)]) {

  ## Plot background image
  if(!is.null(tagdata)) light.image(tagdata,offset=offset,lmax=lmax)

  ## If there is twilight data
  if(!is.null(twilights)) {

    day <- twilights$Twilight
    hour <- hour.offset(as.hour(twilights$Twilight),offset)

    ## Initialize plot axes
    if(is.null(tagdata)) plot(day,hour,type="n",xlab="Date",ylab="Hour",ylim=c(offset,offset+24))

    ## Plot twilight times
    if(is.null(point.col)) point.col <- palette[ifelse(twilights$Rise,1,2)]
    if(point) points(day,hour,pch=16,cex=point.cex,col=point.col)

    ## Plot twilights segments
    if(!is.null(ribbon)) {
      rise <- twilights[twilights$Rise,]
      tsimage.ribbon(.POSIXct(tapply(rise$Start,rise$Twilight,min),"GMT"),
                     .POSIXct(tapply(rise$End,rise$Twilight,max),"GMT"),
                     offset=offset,border=NA,col=palette[1])
      set <- twilights[!twilights$Rise,]
      tsimage.ribbon(.POSIXct(tapply(set$Start,set$Twilight,min),"GMT"),
                     .POSIXct(tapply(set$End,set$Twilight,max),"GMT"),
                     offset=offset,border=NA,col=palette[2])
    }
    ## Mark points
    if(!is.null(mark)) points(day[mark],hour[mark],pch=3)
  }
}


profile.init <- function(date,light,lmax=64,xlab="",main="") {
  ## Draw axes for light profiles
  plot(date,light,ylim=c(0,lmax),xlab=xlab,ylab="Light",type="n",xaxt="n",main=main)
  axis.POSIXct(1,x=date,format="%H:%M")
}


profile.overlay <- function(date,light,threshold=NULL,point=FALSE,
                            date.prev,light.prev,date.next,light.next,
                            point.cex=0.6,palette=default.palette[c(9,5,2,1)]) {

  ## Overlay with light threshold
  if(!is.null(threshold)) abline(h=threshold,col=palette[4])
  ## Overlay the light profile for the current and surrounding days
  lines(date.prev+86400,light.prev,col=palette[2])
  lines(date.next-86400,light.next,col=palette[3])
  lines(date,light,col=palette[1])
  ## Overlay observations.
  if(point) points(dteB,lgtB,col=palette[1],pch=16,cex=point.cex)
}

selection.rectangle <- function(x1,x2,col,add=FALSE) {
  ## Determine upper and lower limits
  rx <- grconvertX(c(0,1),from="npc",to="user")
  ry <- grconvertY(c(1.01,1.03),from="npc",to="user")
  ## Over plot with white
  if(!add) rect(rx[1],ry[1],rx[2],ry[2],border=NA,col="white",xpd=NA)
  if(length(x1) >0 && length(x2)>0)
    rect(x1,ry[1],x2,ry[2],border=NA,col=col,xpd=NA)
}


ndc.closest.twilight <- function(x,y,day,hour) {
  xs <- grconvertX(c(day,day,day),from="user",to="ndc")
  ys <- grconvertY(c(hour-24,hour,hour+24),from="user",to="ndc")
  (which.min((x-xs)^2+(y-ys)^2)-1)%%length(day)+1
}

ndc.image.date <- function(x,y) {
  day <- .POSIXct(grconvertX(x,from="ndc",to="user"))
  hour <- grconvertY(y,from="ndc",to="user")
  date <- .POSIXct(day+((hour-as.hour(day))%%24)*60*60,tz="GMT")
}



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
##' @param extend the period (in days) before and after the endpoints
##' of the selection to be shown in the zoom window.
##' @param lmax the maximum light level to plot.
##' @param width width of the interface windows.
##' @param height height of the interface windows.
##' @param palette a colour palette of 2 colours.
##' @return the dataframe of the selected subsets
##' \item{\code{Start}}{start time of subset}
##' \item{\code{End}}{end time of subset}
##' @export
select.subset <- function(tagdata,offset=0,extend=6,lmax=64,
                          width=10,height=5,
                          palette=default.palette[1:2]) {

  ## Round down/up to nearest offset
  floorDate <- function(date) date - ((as.hour(date)-offset)%%24)*60*60
  ceilingDate <- function(date) date + ((offset-as.hour(date))%%24)*60*60

  ## Set initial selection to entire date range
  minDate <- floorDate(min(tagdata$Date))
  maxDate <- ceilingDate(max(tagdata$Date))
  startDate <- minDate
  endDate <- maxDate

  ## Data frame of  selected subsets
  subsets <- data.frame(Start=NULL,End=NULL)

  ## Select device
  set.device <- function(dev) if(dev.cur()!=dev) dev.set(dev)
  ## Focus if possible
  focus <- if(exists("bringToTop",mode="function")) bringToTop else set.device

  ## Compute the zoom range
  zoom.range <-  function(date) {
    start <- floorDate(max(date - extend*24*60*60,minDate))
    end <- ceilingDate(min(start+2*extend*24*60*60,maxDate))
    start <- floorDate(max(end - 2*extend*24*60*60,minDate))
    c(start=start,end=end)
  }

  ## Draw the selection window
  winA.init <- function() {
    set.device(winA)
    light.image(tagdata,offset=offset,lmax=lmax)
  }

  ## Draw the selection rectangles
  winA.draw <- function() {
    set.device(winA)
    selection.rectangle(subsets$Start,subsets$End,endDate,col=palette[2])
    selection.rectangle(startDate,endDate,col=palette[1])
  }

  ## Draw zoom window
  winB.draw <- function() {
    set.device(winB)
    if(!is.null(startDate) && !is.null(endDate)) {
      par(mfrow=c(1,2))
      light.image(tagdata,offset=offset,lmax=lmax,xlim=zoom.range(startDate))
      abline(v=startDate,col=palette[1])
      light.image(tagdata,offset=offset,lmax=lmax,xlim=zoom.range(endDate))
      abline(v=endDate,col=palette[1])
    }
  }

  ## onMouseDown callback for selection window.
  winA.OnMouseDown <- function(buttons,x,y) {
    ## Determine selected profile.
    set.device(winA)
    date <- floorDate(.POSIXct(grconvertX(x,from="ndc",to="user")))
    date <- min(max(date,minDate),maxDate)
    startDate <<- date
    endDate <<- date
    ## Redraw selection rectangles
    winA.draw()
    NULL
  }

  ## onMouseMove callback for selection window
  winA.OnMouseMove <- function(buttons,x,y) {
    set.device(winA)
    ## Button 1 drag selects range
    if(length(buttons) > 0 && buttons[1]==0) {
      date <- ceilingDate(.POSIXct(grconvertX(x,from="ndc",to="user"),"GMT"))
      date <- min(max(date,minDate),maxDate)
      if(date < startDate) {
        endDate <<- startDate
        startDate <<- date
      } else {
        endDate <<- date
      }
      winA.draw()
    }
    NULL
  }

  ## onMouseUp callback for selection window
  winA.OnMouseUp <- function(buttons,x,y) {
    set.device(winA)
    ## All buttons up -> redraw zoom window
    if(length(buttons)==0) {
      date <- ceilingDate(.POSIXct(grconvertX(x,from="ndc",to="user")))
      date <- min(max(date,minDate),maxDate)
      if(date < startDate) {
        endDate <<- startDate
        startDate <<- date
      } else {
        endDate <<- date
      }
      winA.draw()
      winB.draw()
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
    winA.draw()
    winB.draw()
    NULL
  }

  ## Set up master window
  X11(width=width,height=height)
  winA <- dev.cur()
  winA.init()
  winA.draw()
  setGraphicsEventHandlers(
    which=winA,
    prompt="Select Subset",
    onMouseDown=winA.OnMouseDown,
    onMouseMove=winA.OnMouseMove,
    onMouseUp=winA.OnMouseUp,
    onKeybd=onKeybd)
  ## Set up zoom window
  X11(width=width,height=height)
  winB <- dev.cur()
  winB.draw()
  setGraphicsEventHandlers(
    which=winB,
    prompt="Select Endpoints",
    onKeybd=onKeybd)
  ## Monitor for events
  focus(winA)
  tryCatch({
    getGraphicsEvent()
      dev.off(winA)
      dev.off(winB)
      subsets
    }, finally=subsets)
}




##' Interactively select periods of night for estimating twilights
##'
##' Interactively select the periods of night that will be used by
##' \code{find.twilights} to seek sunset, sunrise pairs that
##' correspond to a given light threshold.  Left mouse button clicks
##' select segments to be included in the search, and right button
##' clicks select segments to be excluded from the search.
##'
##' @title Select nights
##' @param tagdata a datframe with columns \code{Date} and
##' \code{Light} that are the sequence of sample times (as POSIXct)
##' and light levels recorded by the tag.
##' @param threshold the light threshold that defines twilight.
##' @param offset the starting hour for the vertical axes.
##' @param extend a time in minutes. The function seeks periods of
##' darkness that differ from one another by 24 hours plus or minus
##' this interval.
##' @param dark.min a time in minutes. Periods of darkness shorter
##' than this interval will be excluded.
##' @param point.cex expansion factor for plot points.
##' @param lmax the maximum light level to plot.
##' @param width width of the interface windows.
##' @param height height of the interface windows.
##' @param palette a colour palette of 2 colours.
##' @return A dataframe with columns
##' \item{\code{Twilight}}{times of twilight}
##' \item{\code{Rise}}{logical indicating sunrise}
##' where each row corresponds to a single twilight.
##' @export
select.night <- function(tagdata,threshold,offset=0,
                         extend=0,dark.min=0,
                         point.cex=0.6,lmax=64,
                         width=10,height=5,
                         palette=default.palette[c(5,2,3,4)]) {

  twilights <- NULL
  include <- NULL
  exclude <- NULL

  ## Select device
  set.device <- function(dev) if(dev.cur()!=dev) dev.set(dev)
  ## Focus if possible
  focus <- if(exists("bringToTop",mode="function")) bringToTop else set.device

  ## Draw the selection window
  winA.draw <- function() {
    set.device(winA)
    ## Plot image
    image.draw(tagdata,twilights,offset=offset,point=TRUE,
               lmax=lmax,point.cex=point.cex,palette=palette)
    if(length(include)>0) tsimage.points(include,offset=offset,pch=16,col=palette[3])
    if(length(exclude)>0) tsimage.points(exclude,offset=offset,pch=16,col=palette[4])
  }

  ## onMouseDown callback for selection window.
  winA.OnMouseDown <- function(buttons,x,y) {
    set.device(winA)
    ## Add segments to include or exclude
    if(length(buttons) > 0 && buttons[1]==0)
      include <<- c(ndc.image.date(x,y,day,hour),include)
    if(length(buttons) > 0 && buttons[1]==2)
      exclude <<- c(ndc.image.date(x,y,day,hour),exclude)

    ## Recompute twilights
    if(length(buttons) > 0 && (buttons[1]==0 || buttons[1]==2))
      twilights <<- find.twilights(tagdata,threshold=threshold,
                                   include=include,exclude=exclude,
                                   extend=extend,dark.min=dark.min)
    winA.draw()
    NULL
  }

  ## onKeybd callback for both windows
  onKeybd <- function(key) {
    ## q quits
    if(key=="q") return(-1)

    ## Redraw
    winA.draw()
    NULL
  }

  ## Set up master window
  X11(width=width,height=height)
  winA <- dev.cur()
  winA.draw()
  focus(winA)
  setGraphicsEventHandlers(
    which=winA,
    prompt="Select Night",
    onMouseDown=winA.OnMouseDown,
    onKeybd=onKeybd)
  tryCatch({
    getGraphicsEvent()
    dev.off(winA)
    twilights
  }, finally=twilights)
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
##' 'r' \tab Resets the selection \cr
##' 'p' \tab Toggles the display of individual points \cr
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
##' @param point.cex expansion factor for plot points.
##' @param width width of the interface windows.
##' @param height height of the interface windows.
##' @param palette a colour palette of 8 colours.
##' @seealso \code{\link{select.crepuscular}}
##' @return the dataframe of edited twilights, with columns
##' \item{\code{Twilight}}{edited times of twilight}
##' \item{\code{Rise}}{logical indicating sunrise}
##' \item{\code{Original}}{original times of twilight}
##' @export
select.twilight <- function(tagdata,twilights,offset=0,extend=6,threshold=NULL,lmax=64,
                           point.cex=0.6,width=10,height=5,
                           palette=default.palette[c(5,2,9,3,4,1,13)]) {

  ## Order twilights and check Deleted and Marker.
  twilights <- twilights[order(twilights$Twilight),]
  if(is.null(twilights$Deleted)) twilights$Deleted <- logical(nrow(twilights))
  if(is.null(twilights$Marker)) twilights$Marker <- integer(nrow(twilights))
  ## Record original times
  twilights$TwilightOriginal <- twilights$Twilight
  ## Extract date and hour of twilight
  day <- twilights$Twilight
  hour <- hour.offset(as.hour(twilights$Twilight),offset)


  ## Cached data subsets
  index <- 1
  edit.pt <- NULL
  changed <- FALSE
  twls <- NULL
  date <- vector(3,mode="list")
  light <- vector(3,mode="list")
  show.obs <- FALSE


  ## Set cached values
  cache <- function(k) {
    index <<- k
    edit.pt <<- NULL
    changed <<- FALSE
    ## Get twilight times
    twl <- twilights$Twilight[index]
    keep <- (twilights$Twilight >= twl-3600*extend) & (twilights$Twilight <= twl+3600*extend)
    keep[index] <- FALSE
    twls <<- twilights$Twilight[keep]
    ## Get profiles
    for(k in 1:3) {
      mid <- twilights$Twilight[index]+(k-2)*86400
      keep <- (tagdata$Date >= mid-3600*extend) & (tagdata$Date <= mid+3600*extend)
      date[[k]] <<- tagdata$Date[keep]
      lght[[k]] <<- tagdata$Light[keep]
    }
  }

  ## Select device
  set.device <- function(dev) if(dev.cur()!=dev) dev.set(dev)
  ## Focus if possible
  focus <- if(exists("bringToTop",mode="function")) bringToTop else set.device

  ## Draw the twilights window
  winA.draw <- function() {
    set.device(winA)
    image.draw(NULL,twilights,offset=offset,point=TRUE,mark=index,
               col=palette[ifelse(twilights$Deleted,7,ifelse(twilights$Rise,1,2))],
               palette=palette)
  }

  ## Draw light profiles
  winB.draw <- function() {
    set.device(winB)
    ## Draw axes for light profiles
    marker <- twilights$Marker[index]
    profile.init(date[[2]],lght[[2]],lmax=lmax,
                 xlab=if(marker>0) paste("Marker: ",marker) else "",
                 main=as.character(twilights$Twilight[index]))
    ## Overlay with light profiles
    profile.overlay(date[[2]],lght[[2]],threshold,show.obs,
                    date[[1]],lght[[1]],date[[3]],lght[[3]],
                    point.cex=point.cex,palette=palette[3:6])
    abline(v=twls,col=palette[7])
    abline(v=twilights$Twilight[index],col=if(!twilights$Deleted[index]) palette[6] else palette[7])
    if(changed) points(edit.pt[1],edit.pt[2],pch=16,col=palette[6])
  }


  ## onMouseDown callback for twilights window.
  winA.OnMouseDown <- function(buttons,x,y) {
    ## Determine selected profile.
    if(length(buttons) > 0 && buttons[1]==0) {
      set.device(winA)
      k <- ndc.closest.twilight(x,y,day,hour)
      ## Redraw
      cache(k)
      winA.draw()
      winB.draw()
      focus(winB)
    }
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
    if(key=="r") {
      cache(index)
    }
    ## d : toggle deletion
    if(key=="r") {
      twilights$Deleted[index] <<- !twilights$Deleted[index]
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
      day <<- twilights$Twilight
      hour <<- hour.offset(as.hour(twilights$Twilight),offset)
      edit.pt <<- NULL
      changed <<- FALSE
    }
    if(key >= "0" && key <= "9") {
      twilights$Marker[index] <<- as.numeric(key)
    }

    ## Redraw
    winA.draw()
    winB.draw()
    NULL
  }

  ## onMouseDown callback for profile window
  winB.OnMouseDown <- function(buttons,x,y) {
    set.device(winB)
    ## Button 1 -> record location
    if(length(buttons) > 0 && buttons[1]==0) {
      changed <<- TRUE
      edit.pt <<- c(grconvertX(x,from="ndc",to="user"),
                    grconvertY(y,from="ndc",to="user"))
    }
    ## Button 2 -> toggle deletion
    if(length(buttons) > 0 && buttons[1]==2) {
      twilights$Deleted[index] <<- !twilights$Deleted[index]
      cache(index)
    }
    winB.draw()
    NULL
  }


  ## Set up twilights window
  index <- 1
  cache(index)
  X11(width=width,height=height)
  winA <- dev.cur()
  winA.draw()
  setGraphicsEventHandlers(
    which=winA,
    prompt="Select Twilight",
    onMouseDown=winA.OnMouseDown,
    onKeybd=onKeybd)
  ## Set up profile window
  X11(width=width,height=height)
  winB <- dev.cur()
  winB.draw()
  setGraphicsEventHandlers(
    which=profile,
    prompt="Light Profile",
    onMouseDown=winB.OnMouseDown,
    onKeybd=onKeybd)
  focus(winA)
  ## Monitor for events
  tryCatch({
      getGraphicsEvent()
      dev.off(winB)
      dev.off(winA)
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
##' 'r' \tab Resets the selection \cr
##' 'p' \tab Toggles the display of individual points \cr
##' '+'/'-' \tab Zoom in or out \cr
##' 'Left arrow' \tab Jump to previous twilight \cr
##' 'Right arrow' \tab Jump to next twilight \cr
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
##' @param point.cex expansion factor for plot points.
##' @param width width of the interface windows.
##' @param height height of the interface windows.
##' @param palette a colour palette of 8 colours.
##' @seealso \code{\link{select.twilight}}
##' @return the dataframe of edited twilights, with columns
##' \item{\code{Twilight}}{edited times of twilight}
##' \item{\code{Rise}}{logical indicating sunrise}
##' \item{\code{Start}}{date of first observation in the crepuscular segment}
##' \item{\code{End}}{date of last observation in the crepuscular segment}
##' @export
select.crepuscular <- function(tagdata,twilights,offset=0,extend=6,threshold=NULL,lmax=64,
                              point.cex=0.5,width=10,height=5,
                              palette=default.palette[c(5,2,9,3,4,1,1)]) {



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
  rise <- FALSE
  date <- vector(3,mode="list")
  light <- vector(3,mode="list")
  selected <- NULL
  start <- end <- 0
  changed <- FALSE
  show.obs <- FALSE

  ## Set cached values
  cache <- function(k) {
    index <<- k
    changed <<- FALSE
    twl <<- twilights$Twilight[which(index==indices)[1]]
    marker <<- twilights$Marker[which(index==indices)[1]]
    rise <<- twilights$Rise[which(index==indices)[1]]
    ## Get profiles
    for(k in 1:3) {
      mid <- twilights$Twilight[index]+(k-2)*86400
      keep <- (tagdata$Date >= mid-3600*extend) & (tagdata$Date <= mid+3600*extend)
      date[[k]] <<- tagdata$Date[keep]
      lght[[k]] <<- tagdata$Light[keep]
    }
    ## Determined selected range
    selected <<- logical(length(date[[2]]))
    for(i in which(index==indices))
      selected[date[[2]] >= twilights$Start[i] & date[[2]] <= twilights$End[i]] <<- TRUE
  }

  ## Select device
  set.device <- function(dev) if(dev.cur()!=dev) dev.set(dev)
  ## Focus if possible
  focus <- if(exists("bringToTop",mode="function")) bringToTop else set.device

  ## Draw the twilights window
  winA.draw <- function() {
    set.device(winA)
    image.draw(NULL,twilights,offset=offset,ribbon=TRUE,mark=index,
               col=palette[ifelse(twilights$Deleted,7,ifelse(twilights$Rise,1,2))],
               palette=palette)
  }


  ## Draw axes for light profiles
  winB.init <- function() {
    set.device(winB)
    profile.init(date[[2]],lght[[2]],lmax=lmax,
                 xlab=if(marker>0) paste("Marker: ",marker) else "",
                 main=as.character(twl))
  }

  ## Draw light profiles
  winB.draw <- function() {
    set.device(winB)
    ## Overlay with light profiles
    profile.overlay(date[[2]],lght[[2]],threshold,show.obs,
                    date[[1]],lght[[1]],date[[3]],lght[[3]],
                    point.cex=point.cex,palette=palette[3:6])

    ## Show selection
    col <- palette[if(changed) 7 else (if(rise) 1 else 2)]
    ## Hightlight selected segments
    x <- ifelse(selected,date[[2]],NA)
    y <- ifelse(selected,lght[[2]],NA)
    lines(x,y,col=col)
    if(show.obs) points(x,y,pch=16,cex=point.cex,col=col)

    ## Selection rectangle
    x1 <- x2 <- NULL
    if(any(selected)) {
      x1 <- date[[2]][diff(c(FALSE,selected))==1]
      x2 <- date[[2]][diff(c(selected,FALSE))==-1]
    }
    selection.rectangle(x1,x2,col)
  }


  ## onMouseDown callback for twilights window.
  winA.OnMouseDown <- function(buttons,x,y) {
    set.device(winA)
    if(length(buttons) > 0 && buttons[1]==0) {
      ## Determine selected profile.
      set.device(winA)
      xs <- grconvertX(c(day,day,day),from="user",to="ndc")
      ys <- grconvertY(c(hour-24,hour,hour+24),from="user",to="ndc")
      k <- (which.min((x-xs)^2+(y-ys)^2)-1)%%length(day)+1
      ## Redraw
      cache(k)
      set.device(winA)
      winA.draw()
      set.device(profile)
      profile.init()
      profile.draw()
      focus(profile)
    }
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
    if(key=="r") {
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
    winA.draw()
    winB.init()
    winB.draw()
    NULL
  }

  ## onMouseDown callback for profile window
  winB.OnMouseDown <- function(buttons,x,y) {
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
  winB.OnMouseMove <- function(buttons,x,y) {
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
  winA <- dev.cur()
  winA.draw()
  setGraphicsEventHandlers(
    which=winA,
    prompt="Select Twilight",
    onMouseDown=winA.OnMouseDown,
    onKeybd=onKeybd)
  ## Set up profile window
  X11(width=width,height=height)
  winB <- dev.cur()
  winB.init()
  winB.draw()
  setGraphicsEventHandlers(
    which=profile,
    prompt="Light Profile",
    onMouseDown=winB.OnMouseDown,
    onMouseMove=winB.OnMouseMove,
    onKeybd=onKeybd)
  focus(winA)
  ## Monitor for events
  tryCatch({
      getGraphicsEvent()
      dev.off(winB)
      dev.off(winA)
      twilights
  }, finally=twilights)
}


##' Interactively edit a path of twilight locations.
##'
##' Interactively edit a path of twilight locations.  A plot of the
##' estimated sunrise and sunset times is displayed, and the user can
##' select the location corresponding to a particular twilight with a
##' left mouse click.
##'
##' The path is dislayed in another window, with the editable location
##' highlighted.  The user can move the editable location with a left
##' mouse click, or recentre the map with a right mouse click.  In
##' auto advance mode, when a location is edited, the editable
##' location advances to the next location in the sequence.
##'
##' In either window
##' \tabular{ll}{
##' 'q' \tab Quits, returning the dataframe of edited twilight segments \cr
##' 'a' \tab Toggle auto advance mode \cr
##' 'r' \tab Resets the zoom to the encompass the entire track \cr
##' 'z' \tab Zooms to the locations surrounding the current location \cr
##' '+'/'-' \tab Zoom in or out \cr
##' 'Left arrow' \tab Jump to previous location \cr
##' 'Right arrow' \tab Jump to next location \cr
##' }
##'
##' The user may supply a function \code{plot.map} that plots the
##' background map.  This must be a function of two arguments
##' \code{xlim} and \code{ylim} the determine the extent of the plot.
##'
##' The user may also supply a function \code{is.invalid} that accepts
##' the current path as an argument and returns a logical vector
##' indicating which locations along the path are in some way invalid.
##' The twilights for these locations will be highlighted.
##'
##' @title  Edit a path
##' @param path a two column matrix of the (lon,lat) locations at the
##' twilight times.
##' @param twilights dataframe of twilight times as generated by
##' \code{\link{find.twilights}}.
##' @param offset the starting hour for the vertical axes.
##' @param fixed logical vector indicating which locations to hold
##' fixed.
##' @param zenith the solar zenith angle that defines twilight.
##' @param aspect aspect ratio of the map.
##' @param contours the levels (in minutes) of twilight residuals to
##' contour.
##' @param extend the number of locations before and after the current
##' location to highlight.
##' @param auto.advance advance to next point afet edit.
##' @param plot.map A function to plot the background map.
##' @param is.invalid A function check if a location is not valid.
##' @param point.cex expansion factor for plot points.
##' @param width width of the interface windows.
##' @param height height of the interface windows.
##' @param palette a colour palette of 8 colours.
##' @return a two column matrix of (lon,lat) locations.
##' @export
select.path <- function(path,twilights,offset=0,fixed=F,zenith=96,aspect=1,
                        contours=c(10,20,50),extend=1,auto.advance=F,
                        plot.map=NULL, is.invalid=function(path) logical(nrow(path)),
                        point.cex=0.5,width=10,height=5,
                        palette=default.palette[c(5,2,1,12,3,4)]) {

  add.alpha <- function(col,alpha) {
    col <- col2rgb(col)/255
    rgb(red=col[1],green=col[2],blue=col[3],alpha=alpha)
  }

  set.zoom <- function(w) {
    window <<- w
    xlim <<- centre[1]+window*c(-0.5,0.5)
    ylim <<- pmax(pmin(centre[2]+aspect*window*c(-0.5,0.5),90),-90)
    map <<- NULL
  }

  set.window <- function() {
    xl <- range(path[,1])
    yl <- range(path[,2])
    centre <<- c(mean(xl),mean(yl))
    set.zoom(max(diff(xl),diff(yl)/aspect))
  }



  ## Order twilights and check deleted
  twilights <- twilights[order(twilights$Twilight),]
  ## Extract date and hour of twilight
  day <- twilights$Twilight
  hour <- hour.offset(as.hour(twilights$Twilight),offset)
  ## Set fixed points
  fixed <- rep(fixed,length.out=nrow(path))
  invalid <- !fixed | is.invalid(path)

  ## Map window parameters
  xlim <- ylim <- centre <- window
  set.window(path)
  ## Cached map
  map <- NULL

  if(is.null(plot.map))
    plot.map <- function(xlim,ylim) { plot.new(); plot.window(xlim,ylim) }

  ## Select device
  set.device <- function(dev) if(dev.cur()!=dev) dev.set(dev)
  ## Focus if possible
  focus <- if(exists("bringToTop",mode="function")) bringToTop else set.device

  ## Draw the twilights window
  winA.draw <- function() {
    set.device(winA)
    image.draw(tagdata,twilights,offset=offset,point=TRUE,mark=index,
               lmax=lmax,point.cex=point.cex,
               point.col=palette[ifelse(invalid,3,ifelse(twilights$Rise,1,2))],
               palette=palette)
  }

  ## Draw light profiles
  winB.draw <- function() {
    set.device(winB)
    ## Create underlying map
    if(is.null(map)) {
      ## User defined map function
      plot.map(xlim,ylim)
      map <<- recordPlot()
      p <- par()$usr
      xlim <<- p[1:2]
      ylim <<- p[3:4]
    } else {
      ## Replot stored plot
      replayPlot(map)
    }

    ## Overlay twilight residuals
    if(!is.null(zenith)) {
      grid <- raster(nrows=30,ncols=30,xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2])
      grid <- solar.residuals(twilights$Twilight[index],twilights$Rise[index],grid,zenith=zenith)
      contour(grid,add=T,levels=c(0,contours),col=add.alpha(palette[5],0.5))
      contour(grid,add=T,levels=-contours,col=add.alpha(palette[6],0.5))
    }
    ## Show full path
    lines(path[,1],path[,2],col=palette[4])
    points(path[,1],path[,2],col=palette[4],pch=16,cex=0.4)
    ## Highlight current point
    ks <- max(1,index-extend):min(nrow(path),index+extend)
    lines(path[ks,1],path[ks,2],col=palette[3])
    points(path[index,1],path[index,2],col=palette[if(fixed[index]) 4 else 3],pch=16,cex=1)
  }

  ## onMouseDown callback for twilights window.
  winA.OnMouseDown <- function(buttons,x,y) {
    set.device(winA)
    ## Determine selected profile.
    index <<- ndc.closest.twilight(x,y,day,hour)
    ## Redraw
    winA.draw()
    winB.draw()
    focus(winB)
    NULL
  }

  ## onKeybd callback for both windows
  onKeybd <- function(key) {
    ## q quits
    if(key=="q") return(-1)
    if(key=="a") {
      auto.advance <<- !auto.advance
    }
    ## Reset zoom to the full path
    if(key=="r") {
      set.window(path)
    }
    ## Zoom to locations surrounding current location
    if(key=="z") {
      set.window(path[max(1,index-extend):min(nrow(path),index+extend),])
    }
    ## +/- : zoom time window around threshold crossing
    if(key=="+") {
      set.zoom(5/8*window)
    }
    if(key=="-") {
      set.zoom(8/5*window)
    }
    ## Left/Right : jump to neighbouring twilight
    if(key=="Left") {
      index <<- max(index-1,1)
    }
    if(key=="Right") {
      index <<- min(index+1,nrow(path))
    }

    ## Redraw
    winA.draw()
    winB.draw()
    NULL
  }

  ## onMouseDown callback for path window
  winB.OnMouseDown <- function(buttons,x,y) {
    set.device(winB)
    ## Button 1 -> move location
    if(length(buttons) > 0 && buttons[1]==0 && !fixed[index]) {
      path[index,] <<- c(grconvertX(x,from="ndc",to="user"),
                         grconvertY(y,from="ndc",to="user"))
      invalid <<- !fixed | is.invalid(path)
      if(auto.advance) index <<- min(index+1,nrow(path))
    }
    ## Button 2 -> centre map
    if(length(buttons) > 0 && buttons[1]==2) {
      ## Map window parameters
      centre <<- c(grconvertX(x,from="ndc",to="user"),
                   grconvertY(y,from="ndc",to="user"))
      set.zoom(window)
    }
    winB.draw()
    NULL
  }

  ## Set up twilights window
  index <- 1
  X11(width=width,height=height)
  winA <- dev.cur()
  winA.draw()
  setGraphicsEventHandlers(
    which=winA,
    prompt="Select Twilight",
    onMouseDown=winA.OnMouseDown,
    onKeybd=onKeybd)
  ## Set up path window
  X11(width=width,height=height)
  winB <- dev.cur()
  winB.draw()
  setGraphicsEventHandlers(
    which=winB,
    prompt="Path",
    onMouseDown=winB.OnMouseDown,
    onKeybd=onKeybd)
  focus(winA)
  ## Monitor for events
  tryCatch({
    getGraphicsEvent()
    dev.off(winB)
    dev.off(winA)
    path
  }, finally=path)
}
