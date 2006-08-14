      subroutine cleartree(treename)
      character*(*) treename
      do 1 i=1,256
    1 treename(i:i) = ' '
      return
      end
c
c
      subroutine defhdr(io,itype)
c  user supplied routine to define header of type itype to be
c  associated with file on unit io.
c  nwb=data type (2=i*2,4=i*4 or r*4,8=r*8 or complex)
c  lhed=number of 4byte words in the header (can be larger than #used)
c  natt=number of attributes defined for searching (.le.20)
c  attrem(i,io)=16 character string describing the i'th attribute
c  iatt(j,i,io)=3 integer array describing the i'th attibute
c   iatt(1  )=index of first 4 byte word of the attribute in the header
c   iatt(2  )=index of last  "   "   "   "   "   "   "   "   "   "   "
c   note that each attribute to be searched on can only extend for up
c   to 10 4byte words
c   iatt(3  )=integer defining the type of search attribute
c      0=character string               4=integer (range)
c      1=integer (equality)             5=real*4  (range)
c      2=real*4  (equality)             6=real*8  (range)
c      3=real*8  (equality)             7=time    (equality)
c                                       8=time    (range) 
c   (equality) means that the search expects to find exact equality
c              between the header attribute and the search variable
c   (range)    means that the header attribute is expected to fall
c              in a range (min,max) of search variables
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10)
      common/atXXXX/natt(10),iatt(3,20,10),attrem(20,10)
      character*16 attrem
      if(itype.ne.1) goto 30
c
c.....itype=1  TIME SERIES HEADER
c
      nwb(io)=4
      lhed(io)=30
      natt(io)=7
      attrem(1,io)='station name    '
      iatt(1,1,io)=2
      iatt(2,1,io)=2
      iatt(3,1,io)=0
      attrem(2,io)='station channel '
      iatt(1,2,io)=3
      iatt(2,2,io)=3
      iatt(3,2,io)=0
      attrem(3,io)='instrument type '
      iatt(1,3,io)=4
      iatt(2,3,io)=4
      iatt(3,3,io)=0
      attrem(4,io)='year            '
      iatt(1,4,io)=5
      iatt(2,4,io)=5
      iatt(3,4,io)=1
      attrem(5,io)='day             '
      iatt(1,5,io)=6
      iatt(2,5,io)=6
      iatt(3,5,io)=4
      attrem(6,io)='yr,dy,hr,mn,sec '
      iatt(1,6,io)=5
      iatt(2,6,io)=5
      iatt(3,6,io)=7
      attrem(7,io)='ditto but range '
      iatt(1,7,io)=5
      iatt(2,7,io)=5
      iatt(3,7,io)=8
      return
c
c.....itype=3  TIME SERIES HEADER WITH SOURCE LOCATION INCLUDED
c
   30 if(itype.ne.3) goto 40
      nwb(io)=4
      lhed(io)=30
      natt(io)=5
      attrem(1,io)='station name    '
      iatt(1,1,io)=2
      iatt(2,1,io)=2
      iatt(3,1,io)=0
      attrem(2,io)='station channel '
      iatt(1,2,io)=3
      iatt(2,2,io)=3
      iatt(3,2,io)=0
      attrem(3,io)='instrument type '
      iatt(1,3,io)=4
      iatt(2,3,io)=4
      iatt(3,3,io)=0
      attrem(4,io)='year            '
      iatt(1,4,io)=5
      iatt(2,4,io)=5
      iatt(3,4,io)=1
      attrem(5,io)='day             '
      iatt(1,5,io)=6
      iatt(2,5,io)=6
      iatt(3,5,io)=4
      return
c
c.....itype=4  SPECTRA OF EVENTFILES
c
   40 if(itype.ne.4) goto 50
      nwb(io)=4
      lhed(io)=30
      natt(io)=5
      attrem(1,io)='station name    '
      iatt(1,1,io)=2
      iatt(2,1,io)=2
      iatt(3,1,io)=0
      attrem(2,io)='station channel '
      iatt(1,2,io)=3
      iatt(2,2,io)=3
      iatt(3,2,io)=0
      attrem(3,io)='instrument type '
      iatt(1,3,io)=4
      iatt(2,3,io)=4
      iatt(3,3,io)=0
      attrem(4,io)='year            '
      iatt(1,4,io)=5
      iatt(2,4,io)=5
      iatt(3,4,io)=1
      attrem(5,io)='day             '
      iatt(1,5,io)=6
      iatt(2,5,io)=6
      iatt(3,5,io)=4
      return                                
c
c.....itype=5  FULL EIGENFUNCTION FILES
c
   50 if(itype.ne.5) goto 60
      nwb(io)=4
      lhed(io)=30
      natt(io)=5
      attrem(1,io)='mode type       '
      iatt(1,1,io)=4
      iatt(2,1,io)=4
      iatt(3,1,io)=0
      attrem(2,io)='order n         '
      iatt(1,2,io)=5
      iatt(2,2,io)=5
      iatt(3,2,io)=4
      attrem(3,io)='degree l        '
      iatt(1,3,io)=6
      iatt(2,3,io)=6
      iatt(3,3,io)=4
      attrem(4,io)='eigen frequency '
      iatt(1,4,io)=7
      iatt(2,4,io)=7
      iatt(3,4,io)=5
      attrem(5,io)='group velocity  '
      iatt(1,5,io)=9
      iatt(2,5,io)=9
      iatt(3,5,io)=5
      return                                
c
c.....itype=6  REDUCED EIGENFUNCTION FILES FOR STRIPPING
c                AND COMPUTATION OF GREENS FUNCTIONS
c
   60 if(itype.ne.6) goto 70
      nwb(io)=4
cxx (MB)   lhed(io)=214
      lhed(io)=1540
      natt(io)=4
      attrem(1,io)='mode type       '
      iatt(1,1,io)=6
      iatt(2,1,io)=6
      iatt(3,1,io)=0
      attrem(2,io)='order n         '
      iatt(1,2,io)=5
      iatt(2,2,io)=5
      iatt(3,2,io)=4
      attrem(3,io)='degree l        '
      iatt(1,3,io)=7
      iatt(2,3,io)=7
      iatt(3,3,io)=4
      attrem(4,io)='eigen frequency '
      iatt(1,4,io)=8
      iatt(2,4,io)=8
      iatt(3,4,io)=5
      return                                
c
c  Differential travel time headers
c
   70 if (itype.ne.7) goto 80
      nwb(io)=0
      lhed(io)=30
      natt(io)=9
      attrem(1,io)='station name    '
      iatt(1,1,io)=2
      iatt(2,1,io)=2
      iatt(3,1,io)=0
      attrem(2,io)='station channel '
      iatt(1,2,io)=3
      iatt(2,2,io)=3
      iatt(3,2,io)=0
      attrem(3,io)='source year     '
      iatt(1,3,io)=4
      iatt(2,3,io)=4
      iatt(3,3,io)=1
      attrem(4,io)='source day (rng)'
      iatt(1,4,io)=5
      iatt(2,4,io)=5
      iatt(3,4,io)=4
      attrem(5,io)='yr,dy,hr,mn,sec '
      iatt(1,5,io)=4
      iatt(2,5,io)=4
      iatt(3,5,io)=7
      attrem(6,io)='ditto but range '
      iatt(1,6,io)=4
      iatt(2,6,io)=4
      iatt(3,6,io)=8
      attrem(7,io)='grade (a/b/c)   '
      iatt(1,7,io)=20
      iatt(2,7,io)=20      
      iatt(3,7,io)=0
      attrem(8,io)='distance range  '
      iatt(1,8,io)=17
      iatt(2,8,io)=17
      iatt(3,8,io)=5
      attrem(9,io)='error range     '
      iatt(1,9,io)=21
      iatt(2,9,io)=21
      iatt(3,9,io)=5
      return

   80 if(itype.ne.8) goto 99
      nwb(io)=0
      lhed(io)=65
      natt(io)=12
      attrem(1,io)='station name    '
      iatt(1,1,io)=16
      iatt(2,1,io)=16
      iatt(3,1,io)=0
      attrem(2,io)='station channel '
      iatt(1,2,io)=18
      iatt(2,2,io)=18
      iatt(3,2,io)=0
      attrem(3,io)='source year     '
      iatt(1,3,io)=2
      iatt(2,3,io)=2
      iatt(3,3,io)=1
      attrem(4,io)='source day (rng)'
      iatt(1,4,io)=3
      iatt(2,4,io)=3
      iatt(3,4,io)=4
      attrem(5,io)='yr,dy,hr,mn,sec '
      iatt(1,5,io)=2
      iatt(2,5,io)=2
      iatt(3,5,io)=7
      attrem(6,io)='ditto but range '
      iatt(1,6,io)=2
      iatt(2,6,io)=2
      iatt(3,6,io)=8
      attrem(7,io)='grade (A/B/C)   '
      iatt(1,7,io)=47
      iatt(2,7,io)=47      
      iatt(3,7,io)=0
      attrem(8,io)='distance range  '
      iatt(1,8,io)=25
      iatt(2,8,io)=25
      iatt(3,8,io)=5
      attrem(9,io)='depth range     '
      iatt(1,9,io)=11
      iatt(2,9,io)=11
      iatt(3,9,io)=5
      attrem(10,io)='turning lat     '
      iatt(1,10,io)=51
      iatt(2,10,io)=51
      iatt(3,10,io)=5
      attrem(11,io)='turning lon     '
      iatt(1,11,io)=52
      iatt(2,11,io)=52
      iatt(3,11,io)=5
      attrem(12,io)='turning depth   '
      iatt(1,12,io)=53
      iatt(2,12,io)=53
      iatt(3,12,io)=5
      return

   99 PRINT*,'CURRENTLY ONLY HEADER TYPE itype=1,3,4,5,6,7,8',
     *       ' ARE DEFINED'
      return
      end
c
c---------------------------------------------------------------------------
c
      subroutine prhdr(nfl,head,itype)
      dimension head(1),hed1(30),hed4(30),hed5(30),hed6(214),hed7(30)
      dimension head8(65)
      character*8 model,amodel
      common/myhdXX/nscan,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,slat,slon
      common/sphdX/mfreq,msta,mchn,mtyp,jy,jd,jh,jm,secs,
     *         f0,fny,df,nshift,ntap,nt,tst,rest(14)
      common/modhed/nots,model,ktyp,n,l,w,q,g,spare(21)
      common/hedX/nhed,amodel,jcom,nz,atyp,lz,wz,qz,gz,nrad,
     *           rn,wn,accn,rout(1400)
      common/myhed7/nscan7,nsta7,nchn7,dum(5),t1,p1,
     & iyd,idd,ihd,imd,ssd,npts,del,prediff,obdiff,mgrade,err
      equivalence (nscan,hed1),(mfreq,hed4),(nots,hed5)
      equivalence (nhed,hed6),(nscan7,hed7)
c*** common block for pick files                 
c  event info:
c    origin time = iye,ide,ihe,ime,sse
c    origin time in secs = evtim
c    location = elat,elon,edep
c    magnitude and moment = ems,emb,emom
c  station info:
c    nsta = station name
c    ntyp = instrument type
c    nchn = channel identifier
c    dt = sample interval (seconds between sample points)
c    location = slat,slon,selev
c    station correction = scor  
c  event-station info:
c    dist = range in degrees
c    azr,azs = azimuth at receiver and source resp.
c    hlat,hlon = midpoint position               
c  time series info:
c    start time = iys,ids,ims,ihs,sss
c    start time in secs = stim
c    toffset = time offset from event time
c    npts = # pts in time series              
c  phase info:
c    pnam= phase name
c    ptim= time from event (or epochal if no event)
c    amp,tstar= amplitude and tstar
c    pol=rel phase
c    qual = quality ('A   ','B   ' etc)
c    ctim=calcualted time for model identified by code modid
c    ecor=ellipt correction
c    tlat,tlon,tdep = turning point location
      integer ecode,scode,code,tcode
      character*4 nsta,ntyp,nchn,qual,modid
      character*8 pnam
      real*8 evtim,stim,ptim
      common/pkhd/nscan8,iye,ide,ihe,ime,sse,evtim,elat,elon,edep,         
     +    ems,emb,emom,ecode,                                             
     +    nsta8,ntyp8,nchn8,dtd,slat8,slon8,selev,scor,scode,                   
     +    dist,azs,azr,hlat,hlon,code,                                    
     +    iys,ids,ihs,ims,sss,zzz,stim,toffset,npts8,                          
     +    pnam,ptim,amp,tstar,pol,qual,ctim,ecor,modid,tlat,tlon,   
     +    tdep,tcode,extra(11)
      equivalence (head8,nscan8)
      if(itype.ne.1) goto 30
c
c.....itype=1  TIME SERIES HEADER
c
      do 1 i=1,10
    1 hed1(i)=head(i)
      PRINT 901,nfl,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,nscan
  901 format(i5,2x,a4,2x,a4,2x,a4,i6,i4,':',i2,':',i2,':',f6.3,
     +       2X,f8.3,i8)
      return
   30 if(itype.ne.3) goto 40
c
c.....itype=3  TIME SERIES HEADER WITH SOURCE LOCATION INCLUDED
c
      do 3 i=1,12
    3 hed1(i)=head(i)
      print 903,nfl,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,nscan,slat,slon
  903 format(i5,2x,a4,2x,a4,2x,a4,i6,i4,':',i2,':',i2,':',f6.3,
     +       2x,f8.3,i8,f7.2,2x,f7.2)
      return
   40 if(itype.ne.4) goto 50                       
c
c.....itype=4  SPECTRA OF EVENTFILES
c
      do 4 i=1,16
    4 hed4(i)=head(i)
      print 904,nfl,msta,mchn,mtyp,jy,jd,jh,jm,secs,
     *    mfreq,f0,fny,df,nshift,ntap,nt,tst
  904 format(i5,2x,a4,2x,a4,2x,a4,i6,i4,':',i2,':',i2,':',f6.3,
     + i6,3f8.3,3i7,f10.1)
      return    
   50 if(itype.ne.5) goto 60
c
c.....itype=5  FULL EIGENFUNCTION FILES
c
      do 5 i=1,14
    5 hed5(i)=head(i)
      print 905,nfl,model,n,ktyp,l,w,q,g
  905 format(i5,2x,a8,i2,2x,a4,i2,e15.6,f10.0,f12.2)
      return    
   60 if(itype.ne.6) goto 70
c
c.....itype=6  REDUCED EIGENFUNCTION FILES FOR STRIPPING
c                AND COMPUTATION OF GREENS FUNCTIONS
c
      do 6 i=1,14
    6 hed6(i)=head(i)
      print 906,nfl,amodel,nz,atyp,lz,wz,qz
  906 format(i5,2x,a8,i2,2x,a4,i2,2e14.5)
      return
   70 if(itype.ne.7) goto 80
c
c.....itype=7 differential travel time headers
c
      do 7 i=1,30
    7 hed7(i)=head(i)
      write(*,907) nfl,nsta7,nchn7,iyd,idd,ihd,imd,ssd,npts,
     &            del,prediff,obdiff,mgrade,err  
  907 format(i5,2x,a4,2x,a4,2x,i4,1x,i3,1x,i2,':',i2,':',f5.2,2x,i4,2x,
     &       f5.1,2(2x,f8.2),2x,a1,2x,f10.3)
      return                                                

   80 if(itype.ne.8) goto 99
      do 8 i=1,65
    8 head8(i)=head(i)            
      if(tstar.ne.-999.) then
      print 908,nfl,nsta8,nchn8,iye,ide,ihe,ime,sse,dtd,edep,dist
     +  ,ptim,ctim,tstar,qual(1:1),pnam
  908 format(i5,1x,a4,1x,a4,i5,i4,':',i2,':',i2,':',f6.3,
     +       3f8.3,2f10.3,f7.2,1x,a1,1x,a8)
      else
      print 9088,nfl,nsta8,nchn8,iye,ide,ihe,ime,sse,dtd,edep,dist
     +  ,ptim,ctim,qual(1:1),pnam
 9088 format(i5,1x,a4,1x,a4,i5,i4,':',i2,':',i2,':',f6.3,
     +       3f8.3,2f10.3,1x,a1,1x,a8)
      endif
      return

   99 print*,'CURRENTLY ONLY HEADER TYPE itype=1,3,4,5,6,7,8',
     *' ARE DEFINED'
      return
      end
c
c-----------------------------------------------------------------------------
c
      subroutine wrhdr(iout,nfl,head,itype)
      dimension head(1),hed1(30),hed4(30),hed5(30),hed6(214),hed7(30)
      dimension head8(65)
      character*8 model,amodel
      common/myhdXX/nscan,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,slat,slon
      common/sphdX/mfreq,msta,mchn,mtyp,jy,jd,jh,jm,secs,
     *         f0,fny,df,nshift,ntap,nt,tst,rest(14)
      common/modhed/nots,model,ktyp,n,l,w,q,g,spare(21)
      common/hedX/nhed,amodel,jcom,nz,atyp,lz,wz,qz,gz,nrad,
     *           rn,wn,accn,rout(1400)
      common/myhed7/nscan7,nsta7,nchn7,dum(5),t1,p1,
     & iyd,idd,ihd,imd,ssd,npts,del,prediff,obdiff,mgrade,err,
     & elcorr,tpcorr,bpcolat,bpcolon,ratio,crcorr,isign,pamp,ppamp
      equivalence (nscan,hed1),(mfreq,hed4),(nots,hed5)
      equivalence (nhed,hed6),(nscan7,hed7)
c
      integer ecode,scode,code,tcode
      character*4 nsta,ntyp,nchn,qual,modid
      character*8 pnam
      real*8 evtim,stim,ptim
      common/pkhd/nscan8,iye,ide,ihe,ime,sse,evtim,elat,elon,edep,         
     +    ems,emb,emom,ecode,                                             
     +    nsta8,ntyp8,nchn8,dtd,slat8,slon8,selev,scor,scode,                   
     +    dist,azs,azr,hlat,hlon,code,                                    
     +    iys,ids,ihs,ims,sss,zzz,stim,toffset,npts8,                          
     +    pnam,ptim,amp,tstar,pol,qual,ctim,ecor,modid,tlat,tlon,   
     +    tdep,tcode,extra1(11)
      equivalence (head8,nscan8)
      if(itype.ne.1) goto 30
c
c.....itype=1  TIME SERIES HEADER
c
      do 1 i=1,10
    1 hed1(i)=head(i)
      write(iout,901) nfl,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,nscan
  901 format(i5,2x,a4,2x,a4,2x,a4,i6,i4,':',i2,':',i2,':',f6.3,
     +       2X,f8.3,i8)
      return
   30 if(itype.ne.3) goto 40
c
c.....itype=3  TIME SERIES HEADER WITH SOURCE (OR RECEIVER) LOCATION INCLUDED
c
      do 3 i=1,12
    3 hed1(i)=head(i)
      write(iout,903) nfl,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,nscan,
     * slat,slon
  903 format(i5,2x,a4,2x,a4,2x,a4,i6,i4,':',i2,':',i2,':',f6.3,
     +       2x,f8.3,i8,f7.2,2x,f7.2)
      return
   40 if(itype.ne.4) goto 50                       
c
c.....itype=4  SPECTRA OF EVENTFILES
c
      do 4 i=1,16
    4 hed4(i)=head(i)
      write(iout,904) nfl,msta,mchn,mtyp,jy,jd,jh,jm,secs,
     *    mfreq,f0,fny,df,nshift,ntap,nt,tst
  904 format(i5,2x,a4,2x,a4,2x,a4,i6,i4,':',i2,':',i2,':',f6.3,
     + i6,3f8.3,3i7,f10.1)
      return    
   50 if(itype.ne.5) goto 60
c
c.....itype=5  FULL EIGENFUNCTION FILES
c
      do 5 i=1,14
    5 hed5(i)=head(i)
      write(iout,905) nfl,model,n,ktyp,l,w,q,g
  905 format(i5,2x,a8,i2,2x,a4,i2,e15.6,f10.0,f12.2)
      return    
   60 if(itype.ne.6) goto 70
c
c.....itype=6  REDUCED EIGENFUNCTION FILES FOR STRIPPING
c                AND COMPUTATION OF GREENS FUNCTIONS
c
      do 6 i=1,14
    6 hed6(i)=head(i)
      write(iout,906) nfl,amodel,nz,atyp,lz,wz,qz
  906 format(i5,2x,a8,i2,2x,a4,i2,2e14.5)
      return
   70 if(itype.ne.7) goto 80
c
c.....itype=7 differential travel time headers
c
      do 7 i=1,30
    7 hed7(i)=head(i)
      write(iout,907) nfl,nsta7,nchn7,iyd,idd,ihd,imd,ssd,npts,
     &            del,prediff,obdiff,mgrade,err,isign
  907 format(i5,2x,a4,2x,a4,2x,i4,1x,i3,1x,i2,':',i2,':',f5.2,2x,i4,2x,
     &       f5.1,2(2x,f8.2),2x,a1,2x,f10.3,2x,i2)
      return          
      
   80 if(itype.ne.8) goto 99
      do 8 i=1,65
    8 head8(i)=head(i)
      if(tstar.ne.-999.) then
      write(iout,908) nfl,nsta8,nchn8,iye,ide,ihe,ime,sse,dtd,edep,dist
     +  ,ptim,ctim,tstar,qual(1:1),pnam
  908 format(i5,1x,a4,1x,a4,i5,i4,':',i2,':',i2,':',f6.3,
     +       3f8.3,2f10.3,f7.2,1x,a1,1x,a8)
      else
      write(iout,9088) nfl,nsta8,nchn8,iye,ide,ihe,ime,sse,dtd,edep,dist
     +  ,ptim,ctim,qual(1:1),pnam
 9088 format(i5,1x,a4,1x,a4,i5,i4,':',i2,':',i2,':',f6.3,
     +       3f8.3,2f10.3,1x,a1,1x,a8)
      endif
      return

   99 print*,'CURRENTLY ONLY HEADER TYPE itype=1,3,4,5,6,7,8',
     *' ARE DEFINED'
      return
      end

