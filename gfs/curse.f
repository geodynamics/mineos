c  curse is designed to edit time series from disk files, the present
c  version uses the GFS system and ida format files.
      logical search
      character*256 name,arg
      data io/3/     
      search=.false.
      name=' '
      numarg=iargc()
      if(numarg.lt.1) then
        print *,' '
        print *,' usage: curse [filename]  [-s]'
        print *,' '
        stop
      end if
      do 5 i=1,numarg
        call getarg(i,arg)
        if(arg(1:2).eq.'-s') then
          search=.true.
        else
          name=arg
        end if
    5 continue
      itype=-1
      call gfs_open(io,name,itype,iret)
      if(iret.gt.0.and.(itype.eq.1.or.itype.eq.3)) then
        if(.not.search) then
          call nosearch
        else
          call gfs_phdef(io) 
          call gfs_sdef(io) 
        end if
        call select(io)
      else
        print *,'file does not exist or is of wrong type'
      end if
      call gfs_close(io)
      stop
      end

      subroutine nosearch
      common/srXXXX/nlist,list(40),flist(10,40)
      nlist=0
      return
      end

      subroutine select(io)
c  The main purpose of select is to allow data sequences to be chosen for later
c  editing. It also allows gross editing e.g. flagging of large data ranges,etc.
c  There is a zoom feature to aid selection-this version saves only the original
c  screen and the last zoom level. This allows viewing of changes in a local 
c  context. Direct editing can be performed-changes are reflected on the screen.
      integer*4 xlow(100),xhigh(100),spoint,epoint,pread,pwrite
      logical edit,autosc,edflag,reflag,nxtf,flagit
      common/slectX/autosc
      common/headX/nsta,nchn,iy,id,ih,im,ss,si,nscan,title
      common/flagXX/flag,badun
      common/envXXX/spoint,epoint,nser,npt,ymax,ymin,screen(1022),il,ix
      common/scrtch/scrbuf(5000)
      common/hcopy/ncops
      dimension fnm(20)
      character*32 text(12) 
      character*62 title
      character*120 line
      character*1 edc
      character*9 namhcopy
      data dflag,badf/5.e15,0.7/     
c*** initialize
      text(1)=' choose range (C is now all rec)'
      text(2)='     later/immediate edit       '
      text(3)='   A) flag or B) unflag range   '
      text(4)=' A) set scale or B) self scale  '
      text(5)=' A) next+plot  B) next  C) skip '
      text(6)='             quit               '
      text(7)='   A) advance or B) re-plot     '
      text(8)='        set flag value          '
      text(9)='            hardcopy            '
      text(10)='         delete record          '
      text(11)='          time a point          '
      ied=0
      ncops=0
      flag=dflag
      badun=badf*flag
      autosc=.true. 
      ymin=0.
      ymax=0.
      nsel=0
      lenv=0              
      kfl=0       
      edc='A'
      reflag=.false.
      call scr_init(text,11)                            
c*** now go read in first file
      goto 35
c***************************************************
 1000 if(autosc) ymax=ymin
      call scr_plt(0,0,screen,npt,spoint,nser,ymin,ymax,flag,title)
c*** select for edit
  200 if(edflag) then
        call scr_pline('immediate edit: A=select; B=zoom; C=unzoom')
      else 
        call scr_pline('deferred edit: A=select; B=zoom; C=unzoom')
      end if
    5 call getran(ilo,ihi,edc,iopt,ied,yval)
      if(iopt.ne.0) goto 10
      if(edc.eq.'C') then
c*** un-zoom ***
        if(lenv.ne.0) goto 206
        if(nsel.le.0) goto 5
        lenv=0
        goto 202
      else if(edc.eq.'B') then
c*** zoom in region of plot ***
         len=ihi-ilo+1
         call pshenv(lenv)
         npts=indata(io,kfl,ilo,len)
         goto 1000
      end if
      if(ihi-ilo.le.0.or.ihi-ilo.gt.1022) then
        call scr_pline('edit range too big or invalid-try again')
        goto 200
      end if
      if(nsel.ge.100) then
        call scr_pline('unable to store any more windows')
        goto 5
      end if
      nsel=nsel+1
      xlow(nsel)=ilo
      xhigh(nsel)=ihi
      if(.not.edflag) goto 5
c*** edit selected ranges - un-zoom if necessary ***
  202 call pshenv(lenv)
      do 204 i=1,nsel
        npts=indata(io,kfl,xlow(i),xhigh(i)-xlow(i)+1)
        if(edit(io,kfl)) reflag=.true.
  204   continue
      nsel=0
      call scr_init(text,11)
  206 call popenv(lenv)
      if(reflag) npts=indata(io,kfl,spoint,nser)
      goto 1000
c*** move to desired option
   10 jopt=iopt
      goto (15,20,25,30,35,40,45,50,55,75,80),iopt
c*** plot command *** 
   15 if(edc.eq.'C') then
        spoint=1
        nser=nscan
      else if(edc.eq.'A') then
  151   call scr_rdnums('input range to view :',fnm,num)
        if(num.eq.0) goto 152
        if(num.ne.2) goto 151
        if(fnm(1)+1.ge.fnm(2)) goto 151
        i2=nint(fnm(2))
        i1=nint(fnm(1))
        nser=i2-i1+1
        spoint=i1
      end if
  152 npts=indata(io,kfl,spoint,nser)
      reflag=.false.
      if(npts.gt.1) goto 1000             
      if(spoint.eq.1.and.nser.gt.1) goto 40
      call scr_pline('invalid range-try again')
      goto 15    
c*** toggle for immediate edit           
   20 if(edc.eq.'C') then
        call scr_pline(' A or B = toggle for later/immediate edit')
        goto 5
      else if(edflag) then
        edflag=.false.
      else
        edflag=.true.
      end if
      goto 200
c*** flag/unflag points
   25 flagit=.true.
      if(edc.eq.'B') flagit=.false.
      if(edc.eq.'C') then
        call scr_pline(' A/B = add/remove flags from data range')
        goto 5
      end if
      call getran(ilo,ihi,edc,iopt,ied,yval)
      if(iopt.ne.0) goto 10
      if(edc.eq.'C') goto 5
      reflag=.true.
  251   itop=min0(ilo+5000-1,ihi)
        nread=pread(io,kfl,scrbuf,ilo,itop)
        if(.not.flagit) then
          do 253 i=1,nread
  253       if(scrbuf(i).gt.badun) call maksml(scrbuf(i),scrbuf(i))
        else
          do 255 i=1,nread
  255       if(scrbuf(i).lt.badun) call makbig(scrbuf(i),scrbuf(i))
        end if
        nwrit=pwrite(io,kfl,scrbuf,ilo,itop)
        if(itop.ge.ihi) goto 200
        ilo=ilo+nread
      goto 251
c*** scale command ***
   30 if(edc.eq.'C') then
        call scr_pline(' A = input scale from screen, B = self-scale')
        goto 5
      else if(edc.eq.'A') then
  301   call scr_rdnums('min and max (return = self-scale) :',fnm,num)
        if(num.eq.0) goto 302
        if(num.ne.2) goto 301
        if(fnm(1).ge.fnm(2)) goto 301
        ymin=fnm(1)
        ymax=fnm(2)  
        autosc=.false.
        goto 1000
      end if
  302 ymin=0.
      ymax=0.
      autosc=.true.
      goto 1000
c*** next command ***
   35 if(edc.ne.'C') goto 351
  350 call scr_rdnums('enter time series number :',fnm,num)
      if(num.ne.1) goto 350
      kfl=fnm(1)-1
      kfl=max0(0,kfl)
  351 if(.not.nxtf(io,kfl)) then
        call scr_pline('unable to move to requested file')
        goto 5
      end if
      if(edc.ne.'A') call scr_pline(title)
      nsel=0
      lenv=0              
      if(edc.eq.'B') goto 5
      spoint=1
      nser=min0(5000,nscan)
      goto 152
c*** quit command ***
   40 call scr_end
      return
c*** advance command ***
   45 if(edc.eq.'C') then
        call scr_pline('A = plot next data segment, B = replot screen')
        goto 5
      else if(edc.eq.'B') then
        npts=indata(io,kfl,spoint,nser)
      else
        if(spoint+nser.ge.nscan) goto 454
        npts=indata(io,kfl,spoint+nser,nser)
      end if
      reflag=.false.
      if(npts.gt.1) goto 1000
  454 call scr_pline('end of file reached')
      goto 5
c*** flag command ***
   50 call scr_rdnums('enter flag value (return for default) :',fnm,num)
      flag=dflag
      badun=badf*flag
      if(num.eq.0) goto 501
      if(num.ne.1) goto 50
      flag=fnm(1)        
      if(flag.le.0.) goto 50
      badun=badf*flag
  501 write(line,502) flag
  502 format('flag value is ',g12.4,'$') 
      call scr_pline(line)
      goto 5
c*** hardcopy ***
   55 ncops=ncops+1
      do 56 jjj=1,9
   56 namhcopy(jjj:jjj)=' '
      if(ncops.le.9) then
        write(namhcopy,57) ncops
      elseif(ncops.ge.10.and.ncops.le.99) then
        write(namhcopy,58) ncops
      else
        write(namhcopy,59) ncops
      end if
   57 format('hrdcpy',i1)
   58 format('hrdcpy',i2)
   59 format('hrdcpy',i3)
      call scr_dump(7,namhcopy)
      goto 5                             
c*** delete record
   75 continue
      call gfs_dentry(io,kfl)
      kfl=kfl-1
      goto 351
c*** time a point
   80 continue
      call scr_xhair(ilo,xval,yval,edc,iopt)
      call updat(ilo,iy1,id1,ih1,im1,ss1)
      write(line,900) ilo,iy1,id1,ih1,im1,ss1,yval
  900 format('time of point ',i10,' is : ',i5,i4,2i3,f6.2,
     +   '        y value is :',g14.6,'$')
      call scr_pline(line)
      goto 5
      end

      subroutine getran(ilo,ihi,edc,iopt,ied,yval)
      character*(*) edc 
   50 call scr_xhair(ilo,xval,yval,edc,iopt)
      if(ied.eq.5.and.edc.eq.'A') return
      if(edc.eq.'C') return
      if(iopt.ne.0) return
      call scr_xhair(ihi,xval,yval,edc,iopt)
      if(edc.eq.'C') return
      if(iopt.ne.0) return
      if(ihi-ilo.ge.0) return
      call scr_pline('invalid range - try again')
      goto 50                                      
      end

      logical function edit(io,ifl)
c  performs most of the editing functions.the array screen in envXXX contains the 
c  points being edited. npt in envXXX is the # of points being edited. The points 
c  are not decimated for editing so npts must be .le. 1022. Spoint and epoint 
c  contain the absolute positions of the first and last points stored in screen.
c  ednum counts the # times the current screen has been modified by an edit. It is 
c  intially zero and is incremented each time saved() is called and is reset to zero 
c  if the user chooses to restart the edit session. saved() stores a copy of the 
c  current envXXX block in a common block. rstred reloads the ednum-1 block back 
c  into envXXX. edlvl is used to record the depth of zooming. It is initially one
c  and is incremented each time a region of the plot is blown up and is decremented 
c  each time the user returns from a zoom. the array nedits records the number of 
c  edits made at each level and is used as a stack of the edit history of each zoom 
c  in the current nesting. When the user returns from the most recent zoom level all 
c  changes made to the screen while in the zoom are merged into the screen saved at 
c  the previous level,these changes are now treated as a single change.
c   edit functions :
c     ied=1  linear interpolation
c     ied=2  flag
c     ied=3  zero fill
c     ied=4  despike
c     ied=5  time points
      integer*4 spoint,epoint,xl,xh,pwrite,ednum,edlvl,spos,epos
      logical autosc
      common/slectX/autosc
      common/headX/nsta,nchn,iy,id,ih,im,ss,si,nscan,title
      common/flagXX/flag,badun
      common/envXXX/spoint,epoint,nser,npt,ymax,ymin,screen(1022),xl,xh
      common/scrtch/oldscr(5000)
      character*32 text(12)
      character*62 title
      character*1 edc
      character*120 line
      dimension fnm(20),nedits(20)
      data maxed/10/
      text(1)='     change edit function       '
      text(2)='     save changes + return      '
      text(3)='      abort edit session        '
      text(4)='  A) re-scale    B) re-plot     '
      text(5)='     re-start edit session      '
      text(6)='     replot/overplot edits      '
      call scr_init(text,6)                            
    5 edit=.false.
      ednum=0
      edlvl=1              
      ipl=-1                           
      ied=1
      nedits(1)=0
c*** plot the region to be edited ***
  500 if(autosc) ymax=ymin
      call scr_plt(0,0,screen,npt,spoint,nser,ymin,ymax,flag,title)
   16 call edfun(ied)
   18 if(ednum.ge.maxed) then 
        call scr_pline('unable to save more edits--suggest you save')
      end if
      call getran(xl,xh,edc,iopt,ied,yval)
      goto (20,50,55,40,30,60),iopt
      if(edc.eq.'C') then
c*** undo last edit ***
        if(ednum.eq.0) goto 18
        if(nedits(edlvl).eq.0) then
          edlvl=edlvl-1
          call rstred(ednum,edlvl,nedits)
          goto 500
        else
          call rstred(ednum,edlvl,nedits)
          nplot=xh-xl+1
          call scr_plt(-1,0,screen,nplot,xl,nser,ymin,ymax,flag,title)
          goto 18
        end if
      end if
      if(ied.eq.5.and.edc.eq.'A') then
c*** print time of a point and y value ***
        call updat(xl,iy1,id1,ih1,im1,ss1)
        write(line,900) xl,iy1,id1,ih1,im1,ss1,yval
  900   format('time of point ',i10,' is : ',i5,i4,2i3,f6.2,
     +   '        y value is :',g14.6,'$')
        call scr_pline(line)
        goto 18
      end if
      if(ednum.ge.maxed) goto 18
      spos=xl-spoint+1
      epos=xh-spoint+1
      call saved(ednum,edlvl,nedits)
      if(edc.eq.'B') then       
c*** zoom in on range ***
        edlvl=edlvl+1
        nedits(edlvl)=0
        do 161 i=spos,epos
  161     screen(i-spos+1)=screen(i)
        npt=epos-spos+1
        spoint=xl
        epoint=xh
        nser=xh-xl+1
        goto 500
      end if
c*** linear interpolate range ***
c*** flag range ***
c*** zero fill range ***  
c*** despike ***
      goto (150,151,153,155),ied
  150 call lintrp(screen,spos+1,epos-1)
      goto 159
  151 do 152 i=spos,epos
  152 if(screen(i).lt.badun) call makbig(screen(i),screen(i))
      goto 159
  153 do 154 i=spos,epos
  154 screen(i)=0.0
      goto 159
  155 len=epos-spos+1
      call spike(screen(spos),len)
  159 nplot=xh-xl+1
      call scr_plt(ipl,0,screen,nplot,xl,nser,ymin,ymax,flag,title)
      goto 18
c******* options start here *******
c*** edit function toggle                                   
   20 ied=ied+1
      if(ied.gt.5) ied=1
      goto 16
c*** start editing session over again ***
   30 if(ednum.eq.0) goto 18
      ednum=1
      call rstred(ednum,edlvl,nedits)
      do 301 i=1,edlvl
  301 nedits(i)=0
      goto 5
c*** rescale + replot ***
   40 if(edc.eq.'B') goto 500
  401 call scr_rdnums('min and max (return for self-scale) ',fnm,num)
      ymin=0.
      ymax=0.
      autosc=.true.
      if(num.ne.2) goto 500
      if(fnm(1).ge.fnm(2)) goto 401
      ymin=fnm(1)
      ymax=fnm(2)  
      autosc=.false.
      goto 500
c*** exit edit mode - unzoom if necessary ***
   50 if(edlvl.eq.1) then        
        if(nedits(1).eq.0) return
        nwrit=pwrite(io,ifl,screen,spoint,epoint)
        edit=.true.
        return                
      end if
      spos=spoint
      epos=epoint
      if(nedits(edlvl).eq.0) then
        edlvl=edlvl-1
        call rstred(ednum,edlvl,nedits)
      else
        do 503 i=1,npt
  503     oldscr(i)=screen(i)
        ednum=ednum-nedits(edlvl)
        call rstred(ednum,edlvl,nedits)
        edlvl=edlvl-1
        ednum=ednum+1
        spos=spos-spoint+1
        epos=epos-spoint+1
        do 504 i=spos,epos
  504     screen(i)=oldscr(i-spos+1)
      end if
      goto 500
c*** replot/overplot toggle                                
   60 ipl=-ipl
      goto 18
   55 return                
      end            

      subroutine edfun(ied)
      goto (1,2,3,4,5),ied
    1 call scr_pline('A=linear interpolation; B=zoom; C=undo edit')
      return
    2 call scr_pline('A=flag range; B=zoom; C=undo edit')
      return
    3 call scr_pline('A=zero fill range; B=zoom; C=undo edit')
      return
    4 call scr_pline('A=despike range; B=zoom; C=undo edit')
      return
    5 call scr_pline('A=time a point; B=zoom; C=undo edit')
      return
      end

      subroutine spike(data,len)
      dimension data(1),n1(50),n2(50),fnm(20)
      call panl(data,len,n1,n2,np)
    5 call scr_rdnums('enter maxdur,spmin : ',fnm,num)
      if(num.ne.2) goto 5
      maxdur=fnm(1)
      spmin=fnm(2)
      do 10 j=1,np
      jlen=n2(j)-n1(j)+1
      ind=n1(j)
   10 call despik(data(ind),jlen,maxdur,spmin)
      return
      end

      subroutine despik(data,npts,maxdur,spmin)
c   despiking program - removes spikes in array data(1...npts) which are
c   .le. maxdur in length and .ge. spmin in size. work(1...npts) is work
c   space which holds median filtered data on output. the filter length
c   is maxdur*2+1 points long. linear interpolation of spikes is performed.
      integer*4 spoint,epoint
      logical autosc
      common/flagXX/flag,badun
      common/slectX/autosc
      common/envXXX/spoint,epoint,nser,npt,ymax,ymin,screen(1022),il,ix
      common/headX/nsta,nchn,iy,id,ih,im,ss,si,nscan,title
      common/scrtch/work(5000)
      dimension data(1)
      character*1 edc
      character*62 title
      m=2*maxdur+1
      m=max0(m,3)
      if(m.gt.npts) return
c   filter data
      do 10 i=1,npts
   10 work(i)=data(i)
      call medflt(work,npts,m)
      do 15 i=1,npts
   15 work(i)=abs(data(i)-work(i))
      if(spmin.gt.0.) goto 25
      call scr_plt(0,0,work,npts,1,npts,0.,0.,flag,title)
   20 call scr_xhair(llx,xval,spmin,edc,iopt)
      if(spmin.le.0.0) goto 20
      if(autosc) ymax=ymin
      call scr_plt(0,0,screen,npt,spoint,nser,ymin,ymax,flag,title)
c   perform interpolation
   25 i=1
   30 i=i+1
      if(i.ge.npts) return
      if(work(i).lt.spmin) goto 30
      ii=i
   35 i=i+1
      if(work(i).ge.spmin) goto 35
      i=i-1
      call lintrp(data,ii,i)
      goto 30
      end
      subroutine lintrp(data,i1,i2)
c  performs linear interpolation on data(i1...i2)
      dimension data(1)
      im=i1-1
      ip=i2+1
      y=(data(ip)-data(im))/(ip-im)
      do 10 j=i1,i2
   10 data(j)=data(im)+y*(j-im)
      return
      end
      subroutine medflt(a,ln,m)
c   running median filter - filters array a(1...ln) in place with a filter
c   length of m. the first and last (m-1)/2 points are unfiltered. m must
c   be odd and be between 3 and 101.
      dimension a(1),sort(101),isubs(101)
      m=max0(min0(m,101),3)
      n=(m-1)/2
      m=2*n+1
      if(m.gt.ln) return
      n1=n+1
      ns=ln-n
      iold=0
      inew=m
      io=n1
c  do inital sort on first filter length of data (uses shell sort)
      do 5 i=1,m
      isubs(i)=i
    5 sort(i)=a(i)
      ig=m
   10 if(ig.le.1) goto 50
      ig=ig/2
      im=m-ig
   15 iex=0
      do 20 i=1,im
      ipg=i+ig
      if(sort(i).le.sort(ipg)) go to 20
      s=sort(i)
      sort(i)=sort(ipg)
      sort(ipg)=s
      is=isubs(i)
      isubs(i)=isubs(ipg)
      isubs(ipg)=is
      iex=iex+1
   20 continue
      if(iex.gt.0) goto 15
      goto 10
c  filter rest of data
   50 a(io)=sort(n1)
      io=io+1
      if(io.gt.ns) return
      inew=inew+1
      iold=iold+1
      i=0
   60 i=i+1
      if(isubs(i).ne.iold) goto 60
c   get new data point
      s=a(inew)
      if(i.eq.1) goto 71
      if(sort(i-1).gt.s) goto 76
c  search up stack
   70 if(i.eq.m) goto 80
   71 if(sort(i+1).ge.s) goto 80
      sort(i)=sort(i+1)
      isubs(i)=isubs(i+1)
      i=i+1
      goto 70
c  search down stack
   75 if(sort(i-1).le.s) goto 80
   76 sort(i)=sort(i-1)
      isubs(i)=isubs(i-1)
      i=i-1
      if(i.gt.1) goto 75
c  put new data point in stack
   80 sort(i)=s
      isubs(i)=inew
      goto 50
      end
      subroutine panl(a,len,n1,n2,np)
c   find gaps of bad data, previously flagged and index them.
c   the resulting time series consists of np panels of good data indexed
c   from n1(i) to n2(i), i=1 to np.
      dimension a(1),n1(1),n2(1)
      common/flagXX/flag,badun
      id=2
      np=0
      do 30 i=1,len
      go to (15,20),id
   15 if(a(i).lt.badun) go to 30
      n2(np)=i-1
      id=2
      go to 30
   20 if(a(i).gt.badun) go to 30
   25 np=np+1
      n1(np)=i
      id=1
   30 continue
      if(id.eq.2) return
      n2(np)=len
      return
      end
      integer*4 function indata(io,ifl,spt,blk)
c  moves a block of data into core. This routine tries to read blk points starting at
c  spt but does so in such a way as to allow for decimation. If blk.le.1022 then no 
c  decimation is necessary and npt (in envXXX) is equal to nptsrd. If blk.gt.1022 
c  then a decimation factor is calculated by
c        idfact=nptsrd/maxtek+1
c  The program loops through reading idfact*2 point at a time,computing the max and
c  min of these points and storing these in the appropriate slots in the array screen.
c  If indata reaches the end of a file without reading any data it returns zero.
      integer*4 spoint,epoint,spt,blk,pread
      common/headX/nsta,nchn,iy,id,ih,im,ss,dt,nscan,title
      common/envXXX/spoint,epoint,nser,npt,ymax,ymin,screen(1022),il,ix
      common/scrtch/scrbuf(5000)
      character*62 title
      if(blk.gt.1022) goto 5
      npt=pread(io,ifl,screen,spt,spt+blk-1)
      indata=npt
      goto 20
    5 nptsrd=min0(nscan,spt+blk-1)-spt+1
      idfact=nptsrd/1022+1
      idec=2*idfact
      mfac=5000/idec
      mfac=mfac*idec
      npt=0
      indata=0
      i1=spt
   10 i2=min0(mfac,nptsrd)+i1-1
      nread=pread(io,ifl,scrbuf,i1,i2)
      indata=indata+nread
      if(nread.le.0) goto 20
      do 15 n=1,nread,idec
      ldec=min0(idec,nread-n+1)
      call decim(scrbuf(n),ldec,screen(npt+1),screen(npt+2))
   15 npt=npt+2
      nptsrd=nptsrd-nread
      if(nptsrd.le.0) goto 20
      i1=i1+nread
      goto 10
   20 if(indata.le.1) return
      spoint=spt
      nser=indata
      epoint=spt+nser-1
      return
      end
      subroutine decim(buf,n,rfirst,rlast)
c  find max and min of a buffer and put in order of occurence
      dimension buf(1)
      imin=1
      imax=1
      do 5 i=1,n
      if(buf(i).gt.buf(imax)) imax=i
    5 if(buf(i).lt.buf(imin)) imin=i
      if(imin.gt.imax) goto 10
      rfirst=buf(imin)
      rlast=buf(imax)
      return
   10 rfirst=buf(imax)
      rlast=buf(imin)
      return
      end
      subroutine pshenv(lenv)
c  save environment
      common/envXXX/buf(1030)
      common/envsvX/envsav(1030,2)
      lenv=min0(2,lenv+1)
      do 5 i=1,1030
    5 envsav(i,lenv)=buf(i)
      return
      end
      subroutine popenv(lenv)
c  restore environment
      common/envXXX/buf(1030)
      common/envsvX/envsav(1030,2)
      if(lenv.eq.0) return
      do 5 i=1,1030
    5 buf(i)=envsav(i,lenv)
      lenv=lenv-1
      return
      end
      subroutine saved(ednum,edlvl,nedits)
c  save edit
      integer*4 ednum,edlvl
      common/envXXX/envbuf(1030)
      common/edsvX/edsav(1030,10)
      dimension nedits(1)
      ednum=ednum+1
      nedits(edlvl)=nedits(edlvl)+1
      do 5 i=1,1030
    5 edsav(i,ednum)=envbuf(i)
      return
      end
      subroutine rstred(ednum,edlvl,nedits)
c  restore edit
      integer*4 ednum,edlvl
      common/envXXX/envbuf(1030)
      common/edsvX/edsav(1030,10)
      dimension nedits(1)
      if(ednum.eq.0) return
      do 5 i=1,1030
    5 envbuf(i)=edsav(i,ednum)
      ednum=ednum-1
      nedits(edlvl)=max0(nedits(edlvl)-1,0)
      return
      end
      logical function nxtf(io,ifl)
c implement the next command. advance to next required file and read header.
      integer*4 spoint,epoint
      common/envXXX/spoint,epoint,nser,npt,ymax,ymin,screen(1022),il,ix
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,spare(20)
      common/headX/jsta,jchn,jy,jd,jh,jm,ssj,dtj,jscan,title          
      character*62 title
      dimension hbuf(30)
      equivalence (nscan,hbuf)                             
      nxtf=.false.
      call gfs_search(io,hbuf,ifl,0)
      if(ifl.lt.0) return
c*** set up internal header 
      jsta=nsta
      jchn=nchn
      jy=iy
      jd=id
      jm=im
      jh=ih
      ssj=ss
      dtj=dt
      jscan=nscan
      write(title,900) ifl,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,nscan
  900 format(i4,') ',a4,2x,a4,2x,a4,i6,i4,':',i2,':',i2,':',f6.3,f8.3,
     +  i8,'$')
      spoint=1
      epoint=nscan
      nser=nscan
      npt=0
      nxtf=.true.
      return
      end
      integer*4 function pread(io,ifl,buf,lo,hi)
c  primitive read from input file,this version for gfs
      integer*4 lo,hi
      dimension buf(1)             
      nwd=hi-lo+1
      call gfs_rwdata(io,buf,ifl,lo,nwd,'r',ierr)
      if(ierr.ne.0) nwd=0
      pread=nwd
      return
      end
      integer*4 function pwrite(io,ifl,buf,lo,hi)
c  primitive write to output file,this version for gfs
      integer*4 lo,hi
      dimension buf(1)                           
      nwd=hi-lo+1
      call gfs_rwdata(io,buf,ifl,lo,nwd,'w',ierr)
      if(ierr.ne.0) nwd=0
      pwrite=nwd
      return
      end
      subroutine updat(n,jy,jd,jh,jm,ssj)
      real*8 t,sec,c1,c2,c3,c4,dt
      common/headX/nsta,nchn,iy,id,ih,im,ss,si,nscan,title
      character*62 title
      data c1,c2,c3,c4/60.d0,24.d0,3600.d0,86400.d0/
      sec=ss
      dt=si                                  
      t=((id*c2+ih)*c1+im)*c1+sec+n*dt-dt
      jd=t/c4
      t=t-jd*c4
      jh=t/c3
      t=t-jh*c3
      jm=t/c1
      ssj=t-jm*c1
      jy=iy
    1 idpy=365
      if(jy-4*(jy/4).eq.0) idpy=366
      if(jd.le.idpy) return
      jy=jy+1
      jd=jd-idpy
      goto 1
      end

      subroutine makbig(rriin,rriout)
      equivalence (riin,iin),(riout,iout)
c  subroutine to use top 4 bits to flag a real*4 datum
c  call this with real numbers as arguments!
      riin=rriin
      iout=or(and(iin,268435440),and(rshift(iin,28),15)+1610612736)
      rriout=riout
      return
      end

      subroutine maksml(rriin,rriout)
      equivalence (riin,iin),(riout,iout)
c  subroutine to use top 4 bits to unflag a real*4 datum
c  call this with real numbers as arguments!
      riin=rriin
      iout=or(and(iin,268435440),lshift(iin,28))
      rriout=riout
      return
      end
