      subroutine allnsta(nsta,th,ph)
      parameter(lsta=10000)
      character*2 dtype(lsta)
      character*40 dname(lsta)
      integer*4 dsta(lsta)
      dimension dlat(lsta),dlon(lsta),delev(lsta)
      dimension ddep(lsta)
      save
      data rad,ifirs/.017453293,0/
      th=0.
      ph=0.
      istat=0
      if(ifirs.eq.1)goto 1
  
      open(51,file=
     + '/home/guy/resp.dir/stations',status='old')
      j=0
 2    continue
      j=j+1
      read(51,101,end=99)dsta(j),dtype(j),dlat(j),
     +     dlon(j),delev(j),ddep(j),dname(j)
      if(dlon(j).lt.0.)dlon(j)=dlon(j)+360.
      goto 2
 99   continue
      close(51)
      ifirs=1
      ndat=j-1
 1    continue
      do 10 i=1,ndat
      if(nsta.eq.dsta(i))then
          istat=1  
          th=rad*(90.-dlat(i))
          ph=rad*dlon(i)
          return
      endif
 10   continue
      print 100,' station not found: ',nsta
 101  format(a4,4x,a2,3x,2f11.4,f8.0,f5.0,17x,a40)
 100  format(a,a4,1x,a4)
      return
      end

      subroutine allgsta(nsta,th,ph,ntyp)
      parameter(lsta=10000)
      character*2 dtype(lsta)
      character*4 ntyp
      character*40 dname(lsta)
      integer*4 dsta(lsta)
      dimension dlat(lsta),dlon(lsta),delev(lsta)
      dimension ddep(lsta)

      save
      data rad,ifirs/.017453293,0/
      th=0.
      ph=0.
      istat=0
      if(ifirs.eq.1)goto 1
  
      open(51,file=
     + '/home/guy/resp.dir/stations',status='old')
      j=0
 2    continue
      j=j+1
      read(51,101,end=99)dsta(j),dtype(j),dlat(j),
     +     dlon(j),delev(j),ddep(j),dname(j)
      if(dlon(j).lt.0.)dlon(j)=dlon(j)+360.
      goto 2
 99   continue
      close(51)
      ifirs=1
      ndat=j-1
 1    continue
      do 10 i=1,ndat
      if(nsta.eq.dsta(i))then
          if(ntyp.eq.dtype(i))then
             istat=1  
             th=rad*(90.-dlat(i))
             ph=rad*dlon(i)
             return
          elseif(ntyp.eq.'')then
             ista=1
             th=rad*(90.-dlat(i))
             ph=rad*dlon(i)
             write(ntyp,'(a4)')dtype(i)
             return
          endif
      endif
 10   continue
      print 100,' station not found: ',nsta
 101  format(a4,4x,a2,3x,2f11.4,f8.0,f5.0,17x,a40)
 100  format(a,a4,1x,a4)
      return
      end


	subroutine pznall(nsta,nchn,ntyp,iy,id,istat)
c*** response is evaluated with evresh
	character*4 nsta,nchn,ntyp,ftmp3,ftmp2,sta
        character*2 typ,ftmp4,ftmp4d
        character*90 filename
	character*23 ftmp5
	character*3 chn
	real*8 freql,freqh,tfreq,treal,trealw,timag,timagw
	parameter(mx=70001,nx=2001)
        common/irisXXX/niris,nmx,sta(mx),chn(mx),typ(mx),
     &    iy1(mx),id1(mx),ih1(mx),iy2(mx),id2(mx),ih2(mx),
     &    freql,freqh,tfreq(nx),
     &    treal(nx),trealw(nx),timag(nx),timagw(nx)
	data iflag/1/
	istat = -1
c  first read in list
	if(iflag.eq.1) then
          filename='/home/guy/resp.dir/LIST'
          open(99,file=filename(1:lenb2(filename)))
          n=1
   98     read(99,96,end=97) typ(n),sta(n),chn(n),iy1(n),id1(n),
     &     ih1(n),iy2(n),id2(n),ih2(n)
   96     format(a2,1x,a4,1x,a3,1x,i4,1x,i3,1x,i2,1x,i4,1x,i3,1x,i2)
          n=n+1
	  go to 98
   97     close(99)
          nmx=n-1
          if(nmx.gt.mx) stop 'resize dimension of pznall'
          iflag=0
	end if
c  now find response
c   the following works for almost everything now
        ftmp4(1:2)=ntyp(1:2)
        ftmp4d(1:2)=ntyp(1:2)

c*** the following is to handle some old network names
        if(ntyp(1:3).eq.'IDA') then
c for OIDA, channel convention VGZ
            if(nchn(1:2).eq.'VG'.or.nchn(1:2).eq.'UG'. 
     +         and.nsta(1:3).ne.'BFO')then
                 ftmp4='ID'
                 ftmp4d='ID'
            else
                 ftmp4='II'
                 ftmp4d='II'
            end if
        end if
c for GEOSCOPE
        if(ntyp(1:3).eq.'GEO'.or.ntyp(1:2).eq.'G-'.or.
     +     ntyp(1:2).eq.'G ') then
          ftmp4='G-'
          ftmp4d='G-'
        endif
c for OIDA
        if(ntyp(1:3).eq.'OID') then
          ftmp4='ID'
          ftmp4d='ID'
        endif
c for MEDNET
       if(ntyp(1:3).eq.'MED') then
         ftmp4='MN'
         ftmp4d='MN'
       endif
c for temporary arrays
        if(ntyp(1:1).eq.'X'.or.ntyp(1:1).eq.'Y') then
          ftmp4d='XY'
          ftmp4=ntyp(1:2)
        endif
c now make sure that components 1,2,3 are covered
	ftmp3=nsta
	ftmp2=nchn
	if(ftmp3(4:4).eq.' ') ftmp3(4:4)='-'
c  I don't think we need this anymore
c        if(ftmp4.eq.'G-'.and.ftmp2(3:3).eq.'3')ftmp2(3:3)='E'
c        if(ftmp4.eq.'G-'.and.ftmp2(3:3).eq.'2')ftmp2(3:3)='N'
c        if(ftmp4.eq.'G-'.and.ftmp2(3:3).eq.'1')ftmp2(3:3)='Z'
	do 94 i=1,nmx
	if(ftmp4.ne.typ(i)) goto 94
	if(ftmp3.ne.sta(i)) goto 94
	if(ftmp2(1:3).ne.chn(i)) goto 94
	if(iy.lt.iy1(i)) goto 94
	if(iy.gt.iy2(i)) goto 94
	if(iy.eq.iy1(i).and.id.lt.id1(i)) goto 94
	if(iy.eq.iy2(i).and.id.gt.id2(i)) goto 94
	write(ftmp5,95) iy1(i),id1(i),ih1(i),
     &  iy2(i),id2(i),ih2(i)
   95   format(i4.4,'.',i3.3,'.',i2.2,'.',i4.4,'.',i3.3,'.',i2.2)
 	filename='/home/guy/resp.dir/'
     &  //ftmp4d//'.dir/'
     &  //ftmp4//'.'//ftmp3//'.'//ftmp2(1:3)//'.'//ftmp5
c        print *,filename
 	open(93,file=filename(1:lenb2(filename)),status='old',
     &  form='unformatted',err=94)
	read(93,end=94) niris,sfreql,sfreqh
	freql=dble(sfreql)
	freqh=dble(sfreqh)
c       write(98,*)niris,sfreql,sfreqh
	do k=1,niris
        read(93) stfreq,streal,stimag
 	tfreq(k)=dble(stfreq)
 	treal(k)=dble(streal)
 	timag(k)=dble(stimag)
        treal(k)=-treal(k)
        timag(k)=-timag(k)
c       write(98,*)stfreq,sqrt(treal(k)*treal(k)+timag(k)*timag(k)),
c    +       atan2(timag(k),treal(k))
	end do
	close(93)
C for linear interpolation
C	call splneq2(niris,treal,trealw)
C	call splneq2(niris,timag,timagw)
	istat=0
	return
   94   continue
	write(6,*) 'response file not found',ftmp4,ftmp3,nchn
	return
	end

        complex function evresh(w)
	real*8 freql,freqh,tfreq,tmp,eval2
        real*8 treal,trealw,timag,timagw
	character*4 sta
	character*3 chn
        character*2 typ
	parameter(mx=70001,nx=2001)
        common/irisXXX/niris,nmx,sta(mx),chn(mx),typ(mx),
     &    iy1(mx),id1(mx),ih1(mx),iy2(mx),id2(mx),ih2(mx),
     &    freql,freqh,tfreq(nx),
     &    treal(nx),trealw(nx),timag(nx),timagw(nx)
C       tmp=1.D0+(   ( dble(w)/(2.D0*3.1415926535D0) ) - freql )
C    &            /( (freqh - freql ) / dble(niris-1) )
C       realtmp=eval2(tmp,niris,treal,trealw)
C       rmagtmp=eval2(tmp,niris,timag,timagw)
C       evresh= cmplx(realtmp,rmagtmp)

C for linear interpolation
        real*8 tmp0
        real tmp1,tmp2
        tmp=dble(w)/(2.D0*3.1415926535D0)
        if(tmp.le.tfreq(1)) then
          evresh= cmplx(treal(1),timag(1))
          return
        endif
        do k = 2, niris
          if(tmp.lt.tfreq(k)) then
            tmp0=(tmp-tfreq(k-1))/(tfreq(k)-tfreq(k-1))
            tmp1=tmp0*(treal(k)-treal(k-1))+treal(k-1)
            tmp2=tmp0*(timag(k)-timag(k-1))+timag(k-1)
            evresh = cmplx(tmp1,tmp2)
            return
          endif
        end do
        evresh= cmplx(treal(niris),timag(niris))
        return
	end

      integer function lenb2(pathname)
c counts number of non-blank characters in the name and left justifies
c if there are leading blanks
      character*(*) pathname
      lenb2=0
      jchar=len(pathname)
      do 5 i=1,jchar
        k=i
        if(pathname(i:i).ne.' ') goto 10
    5   continue
      return
   10 do 20 i=k,jchar
        if(pathname(i:i).eq.' ') goto 30
   20   lenb2=lenb2+1
   30 if(k.eq.1) return
      do 40 i=1,lenb2
        j=k+i-1
   40   pathname(i:i)=pathname(j:j)
      return
      end

      function eval2(y,nn,u,s)
      implicit real*8(a-h,o-z)
c  performs spline interpolation of equally spaced data.
c  evaluates a spline interpolate in a set of equally spaced samples.
c  the routine  splneq  should be called first, to establish the array  s .
c  y  the  coordinate at which interpolate is required, with y=1 for 1st
c     sample point, y=2 for the 2nd, etc.  if actual spacing is  h  and  x1 is
c     the 1st sample coordinate use  y = 1.0 + (x-x1)/h
c  nn  number of samples of function in original set.
c  u  array containing function samples.
c  s  array of normalized 2nd derivatives, computed by  splneq.  the derivativesc     have been multiplied by h**2, where h is the sample spacing.
c  if  y  is out of the range (1,nn), the 1st or last sample value is used.
      dimension u(*),s(*)
      if (y.le.1.0d0) go to 1500
      if (y.ge.float(nn)) go to 2000
      k1=y
      k=k1+1
      dk=k-y
      dk1=y-k1
      ff1=s(k1)*dk*dk*dk
      ff2=s(k)*dk1*dk1*dk1
      eval2=(dk*(6.d0*u(k1)-s(k1))+dk1*(u(k)*6.d0-s(k))+ff1 +ff2)/6.d0
      return
c  out of range.  supply constant values
 1500 eval2=u(1)
      return
 2000 eval2=u(nn)
      return
      end


