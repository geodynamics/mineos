c The scr subroutines are designed to make it easy to set up an
c interactive plotting environment. 
c
c Some changes from the Apollo version have been made. scr_xhair
c has a changed calling sequence and returns an x value as well
c as the nearest integer -- this means it can now be used with
c scr_pl as well as scr_plt. Finally scr_pltchr does not now
c need the string length
c
c   Useful routines:
c   scr_init(text,numopt)
c   scr_end
c   scr_plt(icl,iopt,b,npt,ibeg,nser,ymn,ymx,flag,title)
c   scr_pl(icl,iopt,b,ery,x,erx,npt,ymn,ymx,xmn,xmx,title)
c   scr_xhair(ilo,xlcdat,ylcdat,edc,iopt)
c
c   scr_anyline(x1,y1,x2,y2,icol,idash)
c   scr_line(x1,y1,x2,y2)
c   scr_dline(x1,y1,x2,y2)
c
c   scr_rdtext(string,text,nchar)
c   scr_rdnums(text,fnum,num)
c   scr_pline(text)
c   scr_inmess(x,y)
c
c   scr_pltchr(ix,iy,string)
c
c*** the basic structure of an interactive graphics program is:
c
c     call scr_init(text,numopt)
c     call scr_plt or call scr_pl
c     call scr_xhair(ilo,x,y,ed,iopt)
c   now respond to user input -- first check for iopt gt 0
c   and respond to an option box, a computed goto is a useful
c   way of branching to the desired option  -- if iopt is zero,
c   the user pushed a button in the plotting box -- another
c   call to xhair can be done to get the upper limit of a
c   range to be plotted (ie zoom) and then loop back to
c   call scr_plt. One option should be to quit which
c   is done by calling scr_end.
c
c   you can reinitialize at any time with a different set
c   of option boxes
c
c   the remaining routines are for making input of text (scr_rdtext),
c   and numbers (scr_rdnums) easy and for printing prompts and information
c   (scr_pline). The line routines (scr_line etc) are for drawing
c   lines on top of the plot (done with XOR raster operation) and are
c   useful for drawing grids and fiducial lines -- because of the XOR
c   raster operation, the line color will be unpredictable!
c                      TGM
      subroutine scr_init(text,numopt) 
c    initialize the window. The option boxes plus
c    the text are drawn. There are up to 12 option boxes. 
c...text = character array(<=12) -- input: text option for box    
c...numopt = integer -- input: number of options, text
      parameter (iby0=680,ibx0=15,ibwid=240,ibht=30,
     +                      jbwid=ibwid+10,jbht=ibht+10)
      character*32 text(*)  
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      common/linXX/nlines
      data init/0/
      nlines=0
      nropt = numopt
      yfac=1.
      xfac=1.
      if(init.eq.0) then
        init=1
        call linitializewindow(1024,800)
c      call linitfancywindow(1024,800,'scr-window','scr',5,5)
c*** note that in the parameter statements, nch should be
c*** set to the character width and ncht to the character height
        call lloadfont(ifont,'7x13bold')
        call lsetfont(ifont)
        call lscreentype(kwid,kht,ifcol,nplanes)
        print *,kwid,kht,ifcol,nplanes
        ifcol=1
        if(ifcol.eq. 1) then
          ifill=3
          idraw=4
        else
          ifill=0
          idraw=1
        endif
      end if
c draw the boxes and fill in the text
      nrbox=0        
      call lsetcolor(ifill)
      call ldrawfilledbox(0,0,1024,800)
      do 9010 i=1,3
           jy=iby0+(i-1)*jbht
           do 9020 j=1,4
                nrbox=nrbox+1
                if (nrbox .gt. nropt) goto 9030
                jx=(j-1)*jbwid+ibx0
                call lsetcolor(idraw)
                call ldrawhollowbox(jx,jy,ibwid,ibht)
                call lsetcolor(ifill)
                call scr_pltchr(jx+8,jy+20,text(nrbox))
9020         continue
9010  continue
9030  continue  
      return
      end

      subroutine scr_clrseg(ix1,ix2)
      parameter (iymin=6,iymax=603)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
c...The screen between x pixels ix1 and ix2 is cleared by 
c     over painting the graphic area -- internal
      call ldrawfilledbox(ix1,iymin,ix2-ix1+1,iymax-iymin+1)
      return
      end
  
      subroutine scr_pltchr(ix,iy,string)
c*** plots character string at pixel position ix,iy
c     mostly called internally
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      character*(*)  string 
      call lsetcolor(idraw)
      call lmoveto(ix,iy)
      call lprint(string)
      call lsetcolor(ifill)
      return
      end

      subroutine scr_end
c*** close up X-windows interface
      call ldie
      return
      end

      subroutine scr_rdnums(text,fnum,num)
c...reads numbers from screen
c...text: character -- input -- prompt string
c   fnum: real -- output -- data array containing num numbers
      parameter (nch=7,nchmax=1012/nch)
      real fnum(1)
      integer ist(10),ied(10)
      character*(*) text   
      character*(nchmax) string
      itry=0
9000  call scr_rdtext(text,string,nchar)
      string(nchar+1:nchar+1)=' '
      num=0
      if(string(1:1).ne.' ') then
        ist(1)=1
        num=1
      endif
      do i=1,nchar
        if((string(i:i).ne.' ').and.(string(i+1:i+1).eq.' ')) then
          ied(num) = i
        elseif((string(i:i).eq.' ').and.(string(i+1:i+1).ne.' ')) then
          num=num+1
          ist(num)=i+1
        endif
      enddo
      do i= 1,num
        read(string(ist(i):ied(i)),*,err=1000) fnum(i)
      enddo
      return
1000  if(itry.gt.5) return
      itry=itry+1
      call scr_pline('error in input line -- repeat entry$')      
      goto 9000
      end

      subroutine scr_plt(icl,iopt,b,npt,ibeg,nser,ymn,ymx,flag,title)
c  Routine for plotting uniformly spaced time series allowing for
c  decimation.  General x,y data should be plotted with scr_pl
c  Plots b(i) for b(1)...b(npt). The first call for a plot must be made
c  with icl=0, afterwards segments of the initial plot may be replotted
c  or overplotted by calling with icl.ne.0. The segment to be replotted
c  is expected to be in b(1) to b(npt) where npt is now the number of
c  points in the segment
c
c  if icl=0 clear screen before plotting
c  if icl>0 overplot segment
c  if icl<0 replot segment
c  if iopt=0 connect points with straight line segments
c  if iopt>0 draw with dashed line of pixel length iopt
c  if iopt<0 plot symbol
c*** iopt=1 is stupid dashed line (for speed) ***
c
c...npt = input -- number of points
c   b   = array of y-data points
c   ibeg = index of first actual point in undecimated data
c   nser = length of undecimated data
c   ymn = input -- minimum value of b-data array
c   ymx = input -- maximum value...
c   title = name of the plot
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017,ix0=5,iy0=5)
      parameter (nch=7,ncht=13,nchmax=1012/nch)
      character*(nchmax) text
      character*(*) title
      character ylab*12
      real b(*)
      common/scr1XX/iscrx(2000),iscry(2000)
      integer cent(2)
      common/scrXXX/idraw,ifill,flags,xfc,bxpc,yfac,byp,nropt,jbeg
      save xfac,bxp
      if(npt.le.1) return
c... set up plot scale factors whenever screen is cleared 
      if(icl.eq.0) then
        if(flag.gt.0.) flags=flag
        ymaxs=ymx
        ymins=ymn
        ypmin=iymin
        ypmax=iymax
        xpmin=ixmin
        xpmax=ixmax
        if(ymaxs.le.ymins) call scr_lim(b,npt,ymins,ymaxs,flags)
        if(ymaxs.le.ymins) then
            ymaxs=ymaxs+0.5
            ymins=ymins-0.5
        end if
        xfac=(xpmax-xpmin)/(npt-1)
        xfc=(xpmax-xpmin)/(nser-1)
        yfac=(ypmax-ypmin)/(ymaxs-ymins)
        jbeg=ibeg
        jpt=npt
        bxpc=ixmin+0.5-xfc
        call ldrawfilledbox(0,0,1024,619)
        call lsetcolor(idraw)
        call ldrawhollowbox(ix0,iy0,ixmax-ixmin+2,iymax-iymin+2)
        call lsetcolor(ifill)
      end if
c... determine x and y pixel range to be plotted
      byp=iymin+0.5+yfac*ymaxs
      bxp=ixmin+0.5-xfac
      ist=ibeg-jbeg+1
      iend=ist+npt-1
      ip1=bxp+xfac*ist
      ip2=bxp+xfac*iend
      if(icl .lt. 0) call scr_clrseg(ip1,ip2)
      if(ifill.eq.0) then
         call lsetcolor(1)
      else
         call lsetcolor(0)
      end if
      if(iopt .lt. 0) goto 9030
      if(iopt .gt. 1) goto 9015
c***************************************************************** 
c...Plot solid line or standard dashed line
      kpt=npt
      k=ist-1
      if(iopt.eq.1) call llinestyle(0,5,0,0)
9007  lpt=min(2000,kpt)
      do 9006 i=1,lpt
             k=k+1
             yp=-yfac*b(k)+byp
             iscry(i)=max(min(yp,ypmax),ypmin)
             xp=xfac*k+bxp
9006         iscrx(i)=max(min(xp,xpmax),xpmin)
      call llines(iscrx,iscry,lpt)
      kpt=kpt-lpt
      if(kpt.gt.0) goto 9007
      call llinestyle(0,0,0,0)
      goto 9050
c*****************************************************************      
c...Plot dashed line
9015  continue    
      ion=1
      nrem=iopt
      x0=xpmin-xfac
      yp=-yfac*b(ist)+byp
      iys=max(min(yp,ypmax),ypmin)
      call lmoveto(ip1,iys)
      do 25 i=ip1,ip2
        x=(i-x0)/xfac
        lx=x
        bp=b(lx)+(b(lx+1)-b(lx))*(x-lx)
        yl=-yfac*bp+byp
        iy=max(min(yl,ypmax),ypmin)
        if(nrem.le.0) then
          nrem=iopt
          ion=-ion
          if(ion.lt.0) nrem=iopt+2
        end if
        if(ion.gt.0) then
           call llineto(i,iys)
        else
           call lmoveto(i,iys)
        end if
        nrem=nrem-1
        npix=iy-iys
        if(npix.eq.0) goto 25
        isn=1
        if(npix.lt.0) isn=-1
        npix=isn*npix
  251   if(nrem.le.0) then
          nrem=iopt
          ion=-ion
          if(ion.lt.0) nrem=iopt+2
        end if
        nmove=min(npix,nrem)
        iys=iys+isn*nmove
        if(ion.gt.0) then
           call llineto(i,iys)
        else
           call lmoveto(i,iys)
        end if
        nrem=nrem-nmove
        npix=npix-nmove
        if(npix.gt.0) goto 251
   25 continue
      goto 9050
c*****************************************************************      
c...Plot symbols
9030  isymb=min(-iopt,8)
      do 9300 i=ist,iend
             xp=xfac*i+bxp
             if (xp.lt.xpmin.or.xp.gt.xpmax) goto 9300
             yp=-yfac*b(i)+byp
             if (yp.lt.ypmin.or.yp.gt.ypmax) goto 9300
             cent(1)=xp
             cent(2)=yp
             call scr_symb(cent,isymb)
9300  continue
c*****************************************************************
9050  call lsetcolor(ifill)
      if(icl .ne. 0) goto 9999
c...Plot labels if required
c write the labels for ymin and ymax
      write (ylab,9900) ymaxs
      call scr_pltchr(ix0+2,ncht+iy0+1,ylab)
      write (ylab,9900) ymins  
      call scr_pltchr(ix0+2,iymax-1,ylab)      
9900  format(g12.4)
      nlen=min(len(title),nchmax-14)
      text(1:nchmax)=' '
      nstr=0
      do 50 i=1,nlen
      if(title(i:i).eq.'$') goto 55
   50 nstr=nstr+1
   55 iept=ibeg+nser-1
      write(text(1:6),910) ibeg
      write(text(nchmax-5:nchmax),910) iept
  910 format(i6)
      mbeg=nchmax/2-nstr/2
      mend=mbeg+nstr-1
      text(mbeg:mend)=title(1:nstr)
      call scr_pltchr(ix0+2,iymax+ncht+3,text)
9999  call ldoit
      return
      end

      subroutine scr_pl(icl,iopt,b,ery,x,erx,npt,ymn,ymx,xmn,xmx,title)
c
c  generate a screen plot - this version allows no decimation.
c  Plots b(x) for b(1)...b(npt). The first call for a plot must be made
c  with icl=0, afterwards segments of the initial plot may be replotted
c  or overplotted by calling with icl.ne.0. The segment to be replotted
c  is expected to be in b(1) to b(npt) where npt is now the number of
c  points in the segment
c
c  if icl=0 clear screen before plotting
c  if icl>0 overplot segment
c  if icl<0 replot segment
c  if iopt=0 connect points with straight line segments
c  if iopt>0 draw with dashed line of pixel length iopt
c  if iopt<0 plot symbol
c  if iopt is -8 -> -1 then plot symbol alone
c  if iopt is -18 -> -11 then plot symbol + y errors
c  if iopt is -28 -> -21 then plot symbol + x errors
c  if iopt is -38 -> -31 then plot symbol + x + y errors
c*** iopt=1 is stupid dashed line for speed ***
c
c...npt = input -- number of points
c   x   = array of x-data points
c   b   = array of y-data points
c   erx = error in x-data
c   ery = error in y-data
c   xmn = input -- first  (chosen) start value of x-data array
c   xmx = input -- last value...
c   ymn = input -- minimum value of b-data array
c   ymx = input -- maximum value...
c   title = name of the plot
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017,ix0=5,iy0=5)
      parameter (nch=7,ncht=13,nchmax=1012/nch)
      character*(nchmax) text
      character ylab*12
      character*(*) title
      real b(*),x(*),erx(*),ery(*)
      common/scr1XX/iscrx(2000),iscry(2000)
      integer cent(2)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      if(npt.le.1) return
c... set up plot scale factors whenever screen is cleared 
      if(icl.eq.0) then
        ypmin=iymin
        ypmax=iymax
        xpmin=ixmin
        xpmax=ixmax
        flags=1.e+33
        ymaxs=ymx
        ymins=ymn
        xmaxs=xmx
        xmins=xmn
        if(ymaxs.le.ymins) call scr_lim(b,npt,ymins,ymaxs,flags)
        if(xmaxs.le.xmins) call scr_lim(x,npt,xmins,xmaxs,flags)
        if(xmaxs.le.xmins) return
        if(ymaxs.le.ymins) then
               ymaxs=ymaxs+0.5
               ymins=ymins-0.5
        end if
        xfac=(xpmax-xpmin)/(xmaxs-xmins)
        yfac=(ypmax-ypmin)/(ymaxs-ymins)
        jbeg=1
        call ldrawfilledbox(0,0,1024,619)
        call lsetcolor(idraw)
        call ldrawhollowbox(ix0,iy0,ixmax-ixmin+2,iymax-iymin+2)
        call lsetcolor(ifill)
      end if
c... determine x and y pixel range to be plotted
      byp=iymin+0.5+yfac*ymaxs
      bxp=ixmin+0.5-xfac*xmins
      ip1=bxp+xfac*x(1)
      ip2=bxp+xfac*x(npt)
      ip1=max(ixmin,ip1)
      ip2=min(ixmax,ip2)
      if(icl .lt. 0) call scr_clrseg(ip1,ip2)
      if(ifill.eq.0) then
         call lsetcolor(1)
      else
         call lsetcolor(0)
      end if
      if(iopt .gt. 1) goto 9015
      if(iopt .lt. 0) goto 9030
c***************************************************************** 
c...Plot solid line or gks dashed line
      kpt=npt
      k=0
      if(iopt.eq.1) call llinestyle(0,5,0,0)
9007  lpt=min(2000,kpt)
      do 9006 i=1,lpt
             k=k+1
             yp=-yfac*b(k)+byp
             iscry(i)=max(min(yp,ypmax),ypmin)
             xp=xfac*x(k)+bxp
9006         iscrx(i)=max(min(xp,xpmax),xpmin)
      call llines(iscrx,iscry,lpt)
      kpt=kpt-lpt
      if(kpt.gt.0) goto 9007
      call llinestyle(0,0,0,0)
      goto 9050
c*****************************************************************      
c...Plot dashed line
9015  continue    
      x0=xpmin-xfac*xmins
      ys=-yfac*b(1)+byp
      iys=max(min(ys,ypmax),ypmin)
      ion=1
      nrem=iopt
      jx=2
      grad=(b(2)-b(1))/(x(2)-x(1))
      call lmoveto(ip1,iys)
      do 25 i=ip1,ip2
        xp=(i-x0)/xfac
   26   if(xp.lt.x(jx)) goto 27
          jx=min(jx+1,npt)
          grad=(b(jx)-b(jx-1))/(x(jx)-x(jx-1))
          if(jx.lt.npt) goto 26
   27   bp=b(jx-1)+grad*(xp-x(jx-1))
        yl=-yfac*bp+byp
        iy=max(min(yl,ypmax),ypmin)
        if(nrem.le.0) then
          nrem=iopt
          ion=-ion
          if(ion.lt.0) nrem=iopt+2
        end if
        if(ion.gt.0) then
           call llineto(i,iys)
        else
           call lmoveto(i,iys)
        end if
        nrem=nrem-1
        npix=iy-iys
        if(npix.eq.0) goto 25
        isn=1
        if(npix.lt.0) isn=-1
        npix=isn*npix
  251   if(nrem.le.0) then
          nrem=iopt
          ion=-ion
          if(ion.lt.0) nrem=iopt+2
        end if
        nmove=min(npix,nrem)
        iys=iys+isn*nmove
        if(ion.gt.0) then
           call llineto(i,iys)
        else
           call lmoveto(i,iys)
        end if
        nrem=nrem-nmove
        npix=npix-nmove
        if(npix.gt.0) goto 251
   25 continue
      goto 9050
c*****************************************************************      
c...Plot symbols
c    Isymb will be expressed in modulo 10, and the  numbers
c    less then 10 represent a symbol for every datapoint b(i).
c    For more information see subroutine scr_symb
c       
9030  isymb=-iopt
      mode=0
9031  If (isymb.le.10) goto 9032
      mode=mode+1
      isymb=isymb-10
      goto 9031
9032  if (mode.gt.3) mode=0
      do 9300 i=1,npt
             xp=xfac*x(i)+bxp
             if (xp.lt.xpmin.or.xp.gt.xpmax) goto 9300
             yp=-yfac*b(i)+byp
             if (yp.lt.ypmin.or.yp.gt.ypmax) goto 9300
             cent(1)=xp
             cent(2)=yp
             call scr_symb(cent,isymb)
9300  continue
c*****************************************************************
c...Plot error bars
c   mode = 1 -- y-error bars, 2 -- x-error bars, 3 both
c
      if (mode .eq. 0) goto 9050
      if (mode .eq. 2) goto 9041
      do 9400 i=1,npt
             xp=xfac*x(i)+bxp
             if (xp.lt.xpmin.or.xp.gt.xpmax) goto 9400
             yp=-yfac*b(i)+byp
             if (yp.lt.ypmin.or.yp.gt.ypmax) goto 9400
             cent(1)=xp
             cent(2)=yp
             call scr_ebary(cent,ery(i))
9400  continue
      if(mode.eq.1) goto 9050
9041  continue
      do 9410 i=1,npt
             xp=xfac*x(i)+bxp
             if (xp.lt.xpmin.or.xp.gt.xpmax) goto 9410
             yp=-yfac*b(i)+byp
             if (yp.lt.ypmin.or.yp.gt.ypmax) goto 9410
             cent(1)=xp
             cent(2)=yp
             call scr_ebarx(cent,erx(i))
9410  continue
 
c*****************************************************************
9050  call lsetcolor(ifill)
      if(icl .ne. 0) goto 9999
c...Plot labels if required
c set character height 
c write the labels for ymin and ymax
      write (ylab,9900) ymaxs
      call scr_pltchr(ix0+2,ncht+iy0+1,ylab)
      write (ylab,9900) ymins  
      call scr_pltchr(ix0+2,iymax-1,ylab)      
9900  format(g12.4)
      nlen=min(len(title),nchmax-24)
      text(1:nchmax)=' '
      nstr=0   
      do 51 i=1,nlen
      if(title(i:i).eq.'$') goto 55
   51 nstr=nstr+1
   55 write(text(1:12),9900) xmins
      write(text(nchmax-11:nchmax),9900) xmaxs
      kc=0
      do 550 i=1,12
      if(text(i:i).ne.' ') goto 551
  550 kc=kc+1  
  551 do 552 i=1,12
      ii=i+kc  
  552 text(i:i)=text(ii:ii)
      kc=0
      do 553 i=nchmax,nchmax-11,-1
      if(text(i:i).ne.' ') goto 554
  553 kc=kc+1
  554 do 555 i=nchmax,nchmax-11,-1
      ii=i-kc
  555 text(i:i)=text(ii:ii)
      mbeg=nchmax/2-nstr/2
      mend=mbeg+nstr-1
      text(mbeg:mend)=title(1:nstr)
      call scr_pltchr(ix0+2,iymax+ncht+3,text)
9999  call ldoit
      return
      end

      subroutine scr_lim(buf,n,rmin,rmax,flags)
c*** internal routine to compute limits on an array
      real buf(*)
      badun = 0.7 * flags
      rmin=1.e+30
      rmax=-1.e+30
      do 9010 i=1,n
         if(buf(i).gt.badun) goto 9010
         rmax=amax1(rmax,buf(i))
         rmin=amin1(rmin,buf(i))
9010  continue
c      if ( (abs(rmin).lt.1.e-4).and.(abs(rmax).lt.1.e-4) ) then
c        rmin = 0.0
c        rmax = 0.0
c      endif
      return
      end

      subroutine scr_line(x1,y1,x2,y2)
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017)
c...Plot solid  straight line
c*** coordinates are user coordinates (not pixels)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      iy1=-y1*yfac+byp
      iy1=max(min(iy1,iymax),iymin)
      iy2=-y2*yfac+byp
      iy2=max(min(iy2,iymax),iymin)
      ix1=x1*xfac+bxp
      ix1=max(min(ix1,ixmax),ixmin)
      ix2=x2*xfac+bxp
      ix2=max(min(ix2,ixmax),ixmin)
      call lsetrasterops(6)
      if (ifill.ne.0) then
         call lsetcolor(10)
      else
         call lsetcolor(idraw)
      end if
      call lmoveto(ix1,iy1)
      call llineto(ix2,iy2)
      call lsetrasterops(3)
      call lsetcolor(ifill)
      call ldoit
      return
      end

      subroutine scr_anyline(x1,y1,x2,y2,icol,idash)
c...Plot straight line with color icol and dash length idash pixels
c*** coordinates are user coordinates (not pixels)
c*** because of raster ops -- surprising colors will result
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      iy1=-y1*yfac+byp
      iy1=max(min(iy1,iymax),iymin)
      iy2=-y2*yfac+byp
      iy2=max(min(iy2,iymax),iymin)
      ix1=x1*xfac+bxp
      ix1=max(min(ix1,ixmax),ixmin)
      ix2=x2*xfac+bxp
      ix2=max(min(ix2,ixmax),ixmin)
      jcol=max(min(icol,11),0)
      call lsetcolor(jcol)
      call llinestyle(0,idash,0,0)
      call lsetrasterops(6) 
      call lmoveto(ix1,iy1) 
      call llineto(ix2,iy2) 
      call lsetrasterops(3) 
      call llinestyle(0,0,0,0)
      call lsetcolor(ifill)
      call ldoit
      return
      end

      subroutine scr_dline(x1,y1,x2,y2)
c...Plot dashed straight line
c*** coordinates are user coordinates (not pixels)
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      iy1=-y1*yfac+byp
      iy1=max(min(iy1,iymax),iymin)
      iy2=-y2*yfac+byp
      iy2=max(min(iy2,iymax),iymin)
      ix1=x1*xfac+bxp
      ix1=max(min(ix1,ixmax),ixmin)
      ix2=x2*xfac+bxp
      ix2=max(min(ix2,ixmax),ixmin)
      call llinestyle(0,5,0,0)
      call lsetrasterops(6) 
      if (ifill.ne.0) then
         call lsetcolor(10)
      else
         call lsetcolor(idraw)
      end if
      call lmoveto(ix1,iy1) 
      call llineto(ix2,iy2) 
      call lsetrasterops(3) 
      call llinestyle(0,0,0,0)
      call lsetcolor(ifill)
      call ldoit
      return
      end

      subroutine scr_dump(io,filnam)
c...makes a postscript file.
      character*(*) filnam
      call scr_pline('click on window to be dumped')
      call psscreendump(filnam)
      return
      end

      subroutine scr_xhair(ilo,xlcdat,ylcdat,edc,iopt)
c...subroutine generates a crosshair on the screen and gives access
c   to option boxes. 
c
c    5 output variables:
c    xlcdat = output, real -- locator x-position in data coordinates
c    ylcdat = output, real -- locator y-position in data coordinates
c    iopt = output, integer, 1-->12 -- nr of option box: equals 0 
c                            outside boxes
c    edc = output, character -- button nr.(A,B or C)
c    ilo is nearest integer to xlcdat
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017)
      parameter (iby0=680,ibx0=15,ibwid=240,ibht=30,
     +                      jbwid=ibwid+10,jbht=ibht+10)
      character*1 edc
      integer lpos(2)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      data lpos/10,630/
      lopt=0
      if (ifill.ne.0) then
         call lsetcolor(10)
      else
         call lsetcolor(idraw)
      end if
      call lfastmode
      call lsetrasterops(6)
      call leventtypeon(5)
      call leventtypeon(3)
      if(lpos(2).le.iymax) then
        call lcursor(1)
        call lmoveto(ixmin,lpos(2))
        call llineto(ixmax,lpos(2))
        call lmoveto(lpos(1),iymin)
        call llineto(lpos(1),iymax)
        call lmoveto(lpos(1),lpos(2))
      else
        call lcursor(0)
      end if
c*** begin event loop
 1000   main=0
        call leventwait(main,lcx,lcy,mode,itype)
c*** if in plot box, erase old xhair then plot new one
        if(lpos(2).le.iymax) then
          call lmoveto(ixmin,lpos(2))
          call llineto(ixmax,lpos(2))
          call lmoveto(lpos(1),iymin)
          call llineto(lpos(1),iymax)
          call lmoveto(lpos(1),lpos(2))
        end if
        if(lcy.le.iymax) then
          if(lcx.lt.ixmin.or.lcx.gt.ixmax.or.lcy.lt.iymin) then
             lcx=max(min(lcx,ixmax),ixmin)
             lcy=max(min(lcy,iymax),iymin)
             call lwarp(lcx,lcy)
          end if
          call lcursor(1)
          call lmoveto(ixmin,lcy)
          call llineto(ixmax,lcy)
          call lmoveto(lcx,iymin)
          call llineto(lcx,iymax)
          call lmoveto(lcx,lcy)
        else
          call lcursor(0)
        end if
c*** test for an option box
        iopt=0
        if(lcy.ge.iby0) then
          ldum=lcy-iby0
          if(mod(ldum,jbht).le.ibht) then
              mdum=lcx-ibx0
              if(mod(mdum,jbwid).le.ibwid) then
                jj=ldum/jbht
                ii=mdum/jbwid
                iopt=jj*4+ii+1
                if(iopt.gt.nropt) iopt=0
              end if
           end if
        end if
c*** do highlighting if necessary
        if(iopt.ne.lopt) then
           call scr_bfill(lopt)
           call scr_bfill(iopt)
           lopt=iopt
        end if
        lpos(1)=lcx
        lpos(2)=lcy
c        call ldoit
      if(main.eq.0) goto 1000
      if(lcx.lt.0.or.lcx.gt.1023) goto 1000
      if(lcy.lt.0.or.lcy.gt.799) goto 1000
c*** clean up and set output variables
      call scr_bfill(lopt)
      if(lpos(2).le.iymax) then
        call lmoveto(ixmin,lpos(2))
        call llineto(ixmax,lpos(2))
        call lmoveto(lpos(1),iymin)
        call llineto(lpos(1),iymax)
        call lmoveto(lpos(1),lpos(2))
      end if
c      call ldoit
      call lsetrasterops(3)
      if (main .eq. 1) edc='A'
      if (main .eq. 2) edc='B'
      if (main .eq. 3) edc='C'
c...conversion from world coordinates into data coordinates
      xlcdat=(lcx-bxp+0.5)/xfac
      ylcdat=-(lcy-byp-0.5)/yfac  
      ilo = xlcdat+jbeg-.4999
      call leventtypeoff(3)
      call lslowmode
      call lsetcolor(ifill)
      return
      end     

      subroutine scr_bfill(iopt)
c*** internal routine to highlight a chosen box
      parameter (iby0=680,ibx0=15,ibwid=240,ibht=30,
     +                      jbwid=ibwid+10,jbht=ibht+10)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      if (iopt .eq. 0) return
      j=(iopt-1)/4
      i=iopt-j*4
      jx=(i-1)*jbwid+ibx0
      jy=j*jbht+iby0
      call ldrawfilledbox(jx,jy,ibwid,ibht)
      return
      end

      subroutine scr_mvtxt(iy)
c*** internal routine to do text scrolling
      parameter (iwy0=625,iwx0=7,ncht=13,iwid=2*ncht+4)
      common/linXX/nlines
      nlines=nlines+1
      nlines=min(nlines,4)
      iy=iwy0+min(nlines,3)*(ncht+2)
      if(nlines.lt.4) return
      call lbitblt(0,iwy0+ncht+2,1024,iwid,0,iwy0)
      call ldrawfilledbox(0,iwy0+iwid,1024,ncht+2)
      return
      end

      subroutine scr_pline(text)
c*** prints a line of text -- scrolling is done automatically
      parameter (iwx0=7,nch=7,nchmax=1012/nch)
      character*(*) text
      character*(nchmax) string
      nlen=min(len(text),nchmax)
      do 1 i=1,nchmax
    1 string(i:i)=' '
      do 5 i=1,nlen
      if(text(i:i).eq.'$') goto 10
    5 string(i:i)=text(i:i)
   10 call scr_mvtxt(iy)
      call scr_pltchr(iwx0,iy,string)
      call ldoit
      return
      end

      subroutine scr_rdtext(string,text,nchar)
c*** prints the prompt in array string then accepts text input
c*** into array text. nchar is the length of the input text
      parameter (iwx0=7,nch=7,nchmax=1012/nch)
      character*(*) string,text
      nlen=min(len(string),nchmax)
      nstr=0
      do 1 i=1,nlen
      if(string(i:i).eq.'$') goto 2
    1 nstr=nstr+1
    2 if(nstr.eq.nlen) then
      do 3 i=nlen,1,-1
      if(string(i:i).ne.' ') goto 4
    3 nstr=nstr-1
      end if
    4 call scr_mvtxt(iy)
      if(nstr.gt.0) call scr_pltchr(iwx0,iy,string(1:nstr))
      call ldoit
      ix=(nstr+1)*nch+iwx0
      call scr_rdmess(ix,iy,text,nchar)
      return
      end

      subroutine scr_symb(center,isymb)
c*** internal routine for plotting symbols, the symbol
c is plotted centered at 'center' (a two element integer array)
c  and isymb determines the type of symbol (see below)
      integer center(2),rad1,rad2
      data rad1,rad2/1,2/
      goto (10,20,30,40,50,60,70,80),isymb
c*** open circle
   10 call ldrawhollowcircle(center(1),center(2),2)
      return    
c*** x   
   20 ix=center(1)+2
      iy=center(2)+2
      ix1=ix-4
      iy1=iy-4
      call lmoveto(ix,iy)
      call llineto(ix1,iy1)
      call lmoveto(ix1,iy)
      call llineto(ix,iy1)
      call lmoveto(center(1),center(2))
      return
c*** +
   30 iy=center(2)+2
      ix=center(1)-2
      call lmoveto(center(1),iy)
      call llineto(center(1),iy-4)
      call lmoveto(ix,center(2))
      call llineto(ix+4,center(2))
      call lmoveto(center(1),center(2))
      return
c*** *
   40 ix=center(1)+2
      iy=center(2)+2
      ix1=ix-4
      iy1=iy-4
      call lmoveto(ix,iy)
      call llineto(ix1,iy1)
      call lmoveto(ix1,iy)
      call llineto(ix,iy1)
      call lmoveto(center(1),iy)
      call llineto(center(1),iy1)
      call lmoveto(ix1,center(2))
      call llineto(ix,center(2))
      call lmoveto(center(1),center(2))
      return             
c*** open square
   50 call ldrawhollowbox(center(1)-2,center(2)-2,4,4)
      return
c*** open small circle (looks like a diamond)
   60 call ldrawhollowcircle(center(1),center(2),rad1)
      return   
c*** filled circle
   70 call ldrawfilledcircle(center(1),center(2),3)
      return
c*** filled square 
   80 call ldrawfilledbox(center(1)-2,center(2)-2,5,5)
      return
      end

      subroutine scr_ebarx(center,err)
c*** internal routine to draw x error bars centered at
c  'center' pixel position with length given by err (in
c  real coordinates)
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017)
      integer center(2)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      ixl=center(1)-xfac*err
      ixrt=max(min(ixl,ixmax+1),ixmin-1)
      ixl=center(1)+xfac*err
      ixlt=max(min(ixl,ixmax+1),ixmin-1)
      iy=max(iymin,center(2)-2)
      iy1=min(iymax,center(2)+2)
      call lmoveto(ixlt,iy)
      call llineto(ixlt,iy1)
      call lmoveto(ixlt,center(2))
      call llineto(ixrt,center(2))
      call lmoveto(ixrt,iy)
      call llineto(ixrt,iy1)
      call lmoveto(center(1),center(2))
      return
      end

      subroutine scr_ebary(center,err)
c*** internal routine to draw y error bars centered at
c  'center' pixel position with length given by err (in
c  real coordinates)
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017)
      integer center(2)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      iyl=center(2)+err*yfac
      iyup=max(min(iyl,iymax+1),iymin-1)
      iyl=center(2)-err*yfac
      iydn=max(min(iyl,iymax+1),iymin-1)
      ix=max(ixmin,center(1)-2)
      ix1=min(ixmax,center(1)+2)
      call lmoveto(ix,iydn)
      call llineto(ix1,iydn)
      call lmoveto(center(1),iydn)
      call llineto(center(1),iyup)
      call lmoveto(ix,iyup)
      call llineto(ix1,iyup)
      call lmoveto(center(1),center(2))
      return
      end

      subroutine scr_rdmess(ixp,iyp,text,nchar)
c*** internal routine to actually read character strings
      parameter (nch=7,ncht=13,nchmax=1012/nch)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      character*(*) text
      character*1 ed
      call leventtypeon(1)
      call lcursor(152)
      ix=ixp
      iy=iyp
      call lmoveto(ix,iy)
      call lwarp(ix,iy-ncht/2)
      call ldoit
      nlen=min(len(text),nchmax)
      nstr=1
      i=0
   10 i=i+1
   15 main=0
      call leventwait(main,jx,jy,mode,itype)
      if(itype.ne.1) goto 15
      ed=char(main)
c*** handle return
      if (main.eq.13) then
         call leventtypeoff(1)
         nchar=i-1
         np=i
         if(np.gt.nlen) return
         do 30 i=np,nlen
   30      text(i:i)=' '
        call lcursor(0)
        return
      end if
c*** handle deletes
      if (main.eq.8.or.main.eq.127) then
        if (i.eq.1) go to 15
        i=i-1
        ix=ix-nch
        call lmoveto(ix,iy)
        call ldrawfilledbox(ix,iy-ncht,nch,ncht)
        call lwarp(ix,iy-ncht/2)
        call ldoit
        go to 15
      end if
c*** only allow standard characters
      if (main.lt.32.or.main.gt.126) goto 15
      if(i.gt.nlen) goto 15
      if(ix.gt.1012-nch) goto 15
      call lwarp(ix+nch,iy-ncht/2)
      call scr_pltchr(ix,iy,ed)
      call ldoit
      text(i:i)=ed
      ix=ix+nch
      goto 10
      end

      subroutine scr_inmess(x,y)
c*** allows message to be typed on screen at point x,y
c*** where x,y is in real units (ie as returned by xhair)
c  useful for annotating screens before dumps
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017)
      parameter (nch=7,ncht=13,nchmax=1012/nch)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      character*(nchmax) text
      iy=-y*yfac+byp
c*** refuse if not in plot screen
      if(iy.gt.iymax-1.or.iy.lt.iymin+ncht) return
      ix=x*xfac+bxp
      ix=max(min(ix,ixmax),ixmin)
      call scr_rdmess(ix,iy,text,nchar)
      return
      end

      subroutine scr_strch(x1,y1,x2,y2,lx2)
c*** draws line by stretching -- x1,y1 is input point
c*** x2,y2 is output point, lx2 is nearest integer to x2
      parameter (iymin=6,iymax=603,ixmin=6,ixmax=1017)
      common/scrXXX/idraw,ifill,flags,xfac,bxp,yfac,byp,nropt,jbeg
      character*1 edc
      iy1=-y1*yfac+byp
      iy1=max(min(iy1,iymax),iymin)
      ix1=x1*xfac+bxp
      ix1=max(min(ix1,ixmax),ixmin)
      if (ifill.ne.0) then
         call lsetcolor(10)
      else
         call lsetcolor(idraw)
      end if
      call lsetrasterops(6)
      call leventtypeon(5)
      call leventtypeon(3)
      call lfastmode
      main=0
      call leventwait(main,lcx,lcy,mode,itype)
      jx=max(min(lcx,ixmax),ixmin)
      jy=max(min(lcy,iymax),iymin)
      if(main.ne.0) goto 100
      call lmoveto(ix1,iy1)
      call llineto(jx,jy)
   10 main=0
      call leventwait(main,lcx,lcy,mode,itype)
c  erase old line
      call lmoveto(ix1,iy1)
      call llineto(jx,jy)
      if(main.ne.0) goto 100
c   draw new line
      jx=max(min(lcx,ixmax),ixmin)
      jy=max(min(lcy,iymax),iymin)
      call lmoveto(ix1,iy1)
      call llineto(jx,jy)
      goto 10
 100  call lsetrasterops(3)
      if (main .eq. 1) edc='A'
      if (main .eq. 2) edc='B'
      if (main .eq. 3) edc='C'
      call leventtypeoff(3)
      call lslowmode
      call lmoveto(ix1,iy1)
      call llineto(jx,jy)
      call lwarp(jx,jy)
      call lsetcolor(ifill)
      call ldoit
      x2=(jx-bxp+0.5)/xfac
      y2=-(jy-byp-0.5)/yfac
      lx2=x2+0.5
      return
      end             
