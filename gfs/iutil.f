      program iutil
      common/bits/flag,nfmax
      character a*1, b*256 
      flag=5.e15           
      nfmax=150000
   10 call tnoua('> ')
      read(*,'(a256)') b
c....search for right most non-blank character 
      i=0
   20 i=i+1
      if(b(i:i).eq.' ') goto 20
      a=b(i:i)
      if(a.eq.'q'.or.a.eq.'Q') stop
      if(a.eq.'e'.or.a.eq.'E') call hdcor
      if(a.eq.'c'.or.a.eq.'C') call chopit
      if(a.eq.'p'.or.a.eq.'P') call purge
      nfl=0
      if(a.eq.'s'.or.a.eq.'S') call gstring(nfl)
      if(a.eq.'L') call lhead
      if(a.eq.'l') call lheds
      if(a.eq.'m'.or.a.eq.'M') call mergex
      if(a.eq.'u'.or.a.eq.'U') call unload
      if(a.eq.'G') call unlde1
      if(a.eq.'g') call unldel
      if(a.eq.'f') call flip
      if(a.eq.'F') call flip1
      if(a.eq.'D') call del1
      if(a.eq.'d') call dels
      if(a.eq.'h'.or.a.eq.'H') call propt
      if(a.eq.'z'.or.a.eq.'Z') call zero
      if(a.eq.'k') then
         print *,'enter flag and nstring; currently : ',flag,nfmax
         read(*,*) flag,nfmax
         print *,flag,nfmax
      end if
      goto 10
      end

      subroutine propt
      print *,'you have the following options ;'
      print *,'  q or Q     quit'
      print *,'  p or P     purge a file'
      print *,'  l          list headers by search'
      print *,'  L          list all headers in a file'
      print *,'  d          delete entry by search'
      print *,'  D          delete entry by index'
      print *,'  u or U     copy entry by search'
      print *,'  g          copy and delete entry by search'
      print *,'  G          copy and delete entry by index'
      print *,'  m or M     copy or merge files'      
      print *,'the following are only for itype=1'  
      print *,'  e or E     correct disc header'
      print *,'  f          flip sign of time series by search'
      print *,'  F          flip sign of time series by index'
      print *,'  c or C     chop time series'
      print *,'  s or S     string time series'
      print *,'  z          de-mean series'
      print *,'  k          set flag and string values'
      return
      end

      subroutine lheds
      common/wkXXXX/hed(10000)
c*** list by search        
      itype=-1
      call gfs_opena(1,'list headers of file : ',itype,ierr)
      if(ierr.lt.0) return
      call gfs_phdef(1)
      call gfs_sdef(1)
    5   kfl=0
        kount=0
   10     call gfs_search(1,hed,kfl,0)
          if(kfl.lt.0) goto 14
          kount=kount+1
          call prhdr(kfl,hed,itype)
        goto 10
   14     print *,'number of items found: ',kount
   15     call gfs_close(1)
          jtype=-1
          call gfs_opena(1,'next file : ',jtype,ierr)
          if(ierr.lt.0) return
          if(jtype.ne.itype) goto 15
      goto 5
      end

      subroutine lhead
      common/wkXXXX/hed(10000)
c*** list all headers in file(s)               
    5 itype=-1
      call gfs_opena(1,'list headers of file : ',itype,iret)
      if(iret.lt.0) return
      print *,iret,' entries in this file'
      do 10 i=1,iret
        call gfs_rwdir(1,hed,i,'r',ierr)
   10   call prhdr(i,hed,itype)
      call gfs_close(1)
      goto 5
      end

      subroutine purge
c*** purge file(s)
    5 itype=-1
      call gfs_opena(1,'purge file : ',itype,iret)
      if(iret.lt.0) return
      call gfs_delete(1)
      goto 5
      end

      subroutine mergex
c*** copy or merge file(s)
      itype=-1
      call gfs_opena(2,'copy file : ',itype,iret)
      if(iret.lt.0) return
      jtype=itype
      call gfs_opena(3,'to file : ',jtype,ierr)
      if(ierr.lt.0.or.itype.ne.jtype) goto 20
    5 do 10 i=1,iret
   10   call gfs_centry(2,3,i,0)
   15 call gfs_close(2)
      jtype=-1
      call gfs_opena(2,'next input file :',jtype,iret)
      if(iret.lt.0) goto 25
      if(jtype.ne.itype) goto 15
      goto 5              
   20 call gfs_close(2)
   25 call gfs_close(3)
      return
      end

      subroutine unload
      common/wkXXXX/hed(10000)
c*** copy by search
      itype=-1
      call gfs_opena(2,'unload from file : ',itype,ierr)
      if(ierr.lt.0) return
      jtype=itype
      call gfs_opena(1,'to file : ',jtype,ierr)
      if(ierr.lt.0.or.jtype.ne.itype) goto 20
      call gfs_phdef(1)
      call gfs_sdef(1)
      print *,'copying the following entries'
    5 kfl=0
   10   call gfs_search(2,hed,kfl,0)
        if(kfl.lt.0) goto 15
        call prhdr(kfl,hed,itype)
        call gfs_centry(2,1,kfl,0)
      goto 10                                
   15 call gfs_close(2)
      jtype=-1
      call gfs_opena(2,'next input file : ',jtype,ierr)
      if(ierr.lt.0) goto 25
      if(jtype.ne.itype) goto 15
      goto 5                    
   20 call gfs_close(2)
   25 call gfs_close(1)
      return
      end    

      subroutine unldel
      common/wkXXXX/hed(10000)
c*** copy by search
      itype=-1
      call gfs_opena(2,' unload and delete from file : ',
     *   itype,ierr)
      if(ierr.lt.0) return
      jtype=itype
      call gfs_opena(1,'to file : ',jtype,ierr)
      if(ierr.lt.0.or.jtype.ne.itype) goto 20
      call gfs_phdef(1)
      call gfs_sdef(1)
      print *,'copying the following entries'
    5 kfl=0
   10   call gfs_search(2,hed,kfl,0)
        if(kfl.lt.0) goto 15
        call prhdr(kfl,hed,itype)
        call gfs_centry(2,1,kfl,0)
        call gfs_dentry(2,kfl)
        kfl=kfl-1 
      goto 10                                
   15 call gfs_close(2)
      jtype=-1
      call gfs_opena(2,'next input file : ',jtype,ierr)
      if(ierr.lt.0) goto 25
      if(jtype.ne.itype) goto 15
      goto 5                    
   20 call gfs_close(2)
   25 call gfs_close(1)
      return
      end  
 
      subroutine unlde1
      common/wkXXXX/hed(10000)
c*** copy by index 
      itype=-1   
      call gfs_opena(2,' unload and delete from file : ',
     *   itype,ierr)
      ierr1=ierr
      if(ierr.lt.0) return
      jtype=itype
      call gfs_opena(1,'to file : ',jtype,ierr)
      if(ierr.lt.0.or.jtype.ne.itype) goto 20
      call tnoua('enter i1 and i2 > ')
      read(*,*) i1,i2
      if(i1.lt.1) goto 20
      i2=min0(ierr1,i2)
      if(i2.lt.i1) goto 20
      ndel=i2-i1+1
      if(ndel.gt.8000)then
        print*,' only 8000 entries can be deleted at once'
        goto 20
      endif
      print*,' ... copying ...'
      do 10 i=ndel,1,-1                         
        kfl=i1+i-1
        call gfs_rwdir(2,hed,kfl,'r',iret)
        call prhdr(kfl,hed,itype)
        call gfs_centry(2,1,kfl,0)
        call gfs_dentry(2,kfl)
 10   continue        
   20 call gfs_close(2)
   25 call gfs_close(1)
      return
      end     

      subroutine del1
      common/wkXXXX/hed(2000),kdel(8000)
c*** delete by index               
      itype=-1
      call gfs_opena(1,'delete entries i1 to i2 in file : ',itype,ierr)
      if(ierr.lt.0) return
      call tnoua('enter i1 and i2 > ')
      read(*,*) i1,i2
      if(i1.lt.1) goto 20
      i2=min0(ierr,i2)
      if(i2.lt.i1) goto 20
      ndel=i2-i1+1
      if(ndel.gt.8000) then
        print *,'only 8000 entries can be deleted at once'
        goto 20
      end if
      do 10 i=1,ndel
   10   kdel(i)=i1+i-1
      call dentries(1,ndel)
   20 call gfs_close(1)
      return
      end

      subroutine dels
      common/wkXXXX/hed(2000),kdel(8000)
      character*1 a
c*** delete by search
      itype=-1
      call gfs_opena(1,'delete (by search) from file : ',itype,ierr)
      if(ierr.lt.0) return
      call gfs_phdef(1)
      call gfs_sdef(1)
      print *,'the following entries earmarked for deletion'
    5 kfl=0                                                
      ndel=0
   10 call gfs_search(1,hed,kfl,0)
      if(kfl.lt.0) goto 15
      call prhdr(kfl,hed,itype)
      ndel=ndel+1
      kdel(ndel)=kfl
      if(ndel.eq.8000) then
         print *,'run out of space -- deleting first 8000'
         goto 15
      end if
      goto 10
   15 if(ndel.eq.0) goto 20
      call tnoua('ok to continue? <y/n> : ')
      read(*,'(a1)') a                    
      if(a.ne.'n'.and.a.ne.'N') call dentries(1,ndel)
   20 call gfs_close(1)
      jtype=-1
      call gfs_opena(1,'next input file : ',jtype,ierr)
      if(ierr.lt.0) return
      if(jtype.ne.itype) goto 20
      goto 5
      end                   

      subroutine DENTRIES(io,ndel)
c  for use with dels-deletes up to 8000 entries at once
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/wkXXXX/dum(2000),kdel(8000)
      if(nfil(io).le.ndel) goto 20
      call gfs_cldata(io)
      j=1
      m=1
      do 15 i=1,nfil(io)
      if(j.gt.ndel) goto 5
      if(i.ne.kdel(j)) goto 5
      if(nwb(io).ne.0) call gfs_dfil(io,i,iret)
      j=j+1
      goto 15
    5 if(i.eq.m) goto 10
      call gfs_rwdir(io,dum,i,'r',iret)
      call gfs_rwdir(io,dum,m,'w',iret)
      if(nwb(io).ne.0) call gfs_cname(io,i,m,iret)
   10 m=m+1
   15 continue
      nfil(io)=m-1
      call gfs_hddir(io,nfil(io),ktype(io),ierr)
      return
   20 call gfs_delete(io)  
      return
      end                    

      subroutine hdcor
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,spare(20)
      dimension hed(30)
      equivalence (nscan,hed)
      print *,'header correction option'
      itype=-1
    5 call gfs_opena(1,'input file : ',itype,iret)
      if(iret.lt.0) goto 5
      print *,'number of entries =',iret
    6 call tnoua('enter index of header to be changed : ')
      read(*,*) k
      if(k.lt.1.or.k.gt.iret) goto 100
      call gfs_rwdir(1,hed,k,'r',ierr)
      if(ierr.ne.0) goto 100
   10 print*,'<iopt>.le.0 : accept changes, leave e-mode' 
      print*,'<iopt>.gt.6 : accept changes, edit another header'
      print 900
  900 format('     sta<1> chn<2> typ<3>    start time<4>   ',
     + '    dt<5>   npts<6>')
      call prhdr(k,hed,1)
      read(*,*) iopt
      if(iopt.le.0.or.iopt.gt.6) goto 90 
      goto (21,22,23,24,25,26),iopt
   21 call tnoua('enter station name (a4 format) : ')
      read(*,'(a4)') nsta
      goto 10
   22 call tnoua('enter channel (a4 format) : ')
      read(*,'(a4)') nchn
      goto 10
   23 call tnoua('enter instrument type (a4 format) : ')
      read(*,'(a4)') ntyp
      goto 10
   24 call tnoua('enter start time(iy,id,ih,im,sec) : ')
      read(*,*) iy,id,ih,im,sec
      goto 10
   25 call tnoua('enter sample interval : ')
      read(*,*) dt
      goto 10
   26 call tnoua('enter # scans : ')
      read(*,*) nscan
      goto 10
   90 call gfs_rwdir(1,hed,k,'w',ierr)
      if(iopt.gt.6) goto 6  
  100 call gfs_close(1)
      return
      end

      subroutine chopit
      dimension hed1(30)
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,spare(20)
      equivalence (nscan,hed1)
      print *,'chop option-you can chop in one of two ways :'
      print *,'start time + duration(hrs) rel. to data start time <1>'
      print *,'start time(yr,dy,hr,mn,sec) + duration(hrs)        <2>'
      read(*,*) iopt
      if(iopt.lt.1.or.iopt.gt.2) return
      itype=-1
      call gfs_opena(1,'input file : ',itype,iret)
      if(iret.lt.0) return
      jtype=itype
      io2=2
      call gfs_opena(io2,'output file(ret if overwrite) : ',jtype,jret)
      if(jret.lt.0) then
        io2=1
        jret=0
      end if
      if(iopt.eq.2) then
        call tnoua('enter event time(yr,day,hr,mn,secs) : ')
        read(*,*) jy,jd,jh,jm,secs
        call tnoua('enter duration of record(hrs) : ')
        read(*,*) fhr
      else
        call tnoua('enter start time and duration(hrs) : ')
        read(*,*) rstart,fhr       
      end if
      indx=0
      jindx=0
   20 indx=indx+1
      if(indx.gt.iret) goto 25
      call gfs_rwdir(1,hed1,indx,'r',ierr)
      if(ierr.ne.0) goto 20
      if(iopt.ne.1) n1=ndiff(iy,id,ih,im,sec,jy,jd,jh,jm,secs,dt)
      if(iopt.eq.1) n1=max0(int(rstart*3600./dt+1.5),1)      
      nwd=fhr*3600./dt
      jindx=jindx+1
      call window(1,io2,indx,jret+jindx,n1,nwd,ierr)
      if(ierr.eq.0) call prhdr(indx,hed1,itype)
      if(io2.eq.1.and.ierr.ne.0) then 
         call gfs_dentry(1,indx)
         indx=indx-1
         jindx=jindx-1
         iret=iret-1
      end if          
      if(io2.eq.2.and.ierr.ne.0) then
         jindx=jindx-1
      end if
      goto 20
   25 call gfs_close(1)
      if(io2.ne.1) call gfs_close(io2)
      return
      end

      subroutine gstring(nfl)
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,spare(20)
      common/savhd/jscan,jsta,jchn,jtyp,jy,jd,jh,jm,sj,dj,sparj(20)
      dimension hed(30),hedj(30),idel(8000)
      equivalence (nscan,hed),(jscan,hedj)
      ndel=0
      if(nfl.gt.0) goto 6
    5 itype=-1             
      call gfs_opena(1,'input file : ',itype,iret)
      if(iret.lt.0) goto 5
      nfl=iret
      ktype=itype
      io2=2
      call gfs_opena(io2,'output file(ret if overwrite) : ',ktype,jret)
      if(jret.lt.0) goto 6
      do 7 i=1,nfl
    7 call gfs_centry(1,2,i,0)
      goto 8
    6 io2=1
      jret=0      
    8 kfl=jret+nfl
      i=jret
   10 i=i+1
      if(i.ge.kfl) goto 30
      do 11 kk=1,ndel
        if(i.eq.idel(kk)) goto 10
   11   continue
      call gfs_rwdir(io2,hed,i,'r',ierr)
      call prhdr(i,hed,1)
      if(ierr.ne.0) goto 10
      j=i
   20 j=j+1
   25 if(j.gt.kfl) goto 10
      do 26 kk=1,ndel
        if(j.eq.idel(kk)) goto 20
   26   continue
   27 call gfs_rwdir(io2,hedj,j,'r',ierr)
      if(ierr.ne.0) goto 20
      call glueit(io2,i,j,ierr)
      if(ierr.ne.0) goto 20
      call prhdr(i,hed,1)
      ndel=ndel+1
      idel(ndel)=j
      if(ndel.ge.8000) then
        print*,'run out of space - deleting first 8000 entries'
        print*,'stringing is probably not complete'
        goto 30
      end if
      goto 25
   30 if(ndel.gt.0) call delete(io2,idel,ndel)
      call gfs_close(1)
      if(io2.ne.1) call gfs_close(io2)
      return
      end

      subroutine glueit(io,ifl,jfl,ierr)
      common/bits/flag,nfmax
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,spare(20)
      common/savhd/jscan,jsta,jchn,jtyp,jy,jd,jh,jm,sj,dj,sparj(20)
      dimension hed(30),hedj(30)
      equivalence (nscan,hed),(jscan,hedj)  
      ierr=1
      if(nsta.ne.jsta.or.nchn.ne.jchn) return
      if(ntyp.ne.jtyp) return
      if(abs(dt-dj)/dj.gt.0.001) return
      i1=ifl
      i2=jfl
      n1=ndiff(iy,id,ih,im,ss,jy,jd,jh,jm,sj,dt)
      nflag=n1-nscan-1  
      nf=nscan+1                 
      ncopy=jscan
      if(n1.gt.1) goto 5
      i1=jfl
      i2=ifl
      n1=ndiff(jy,jd,jh,jm,sj,iy,id,ih,im,ss,dt)
      if(n1.le.1) return
      nflag=max0(0,n1-jscan-1)
      nf=jscan+1 
      ncopy=nscan
    5 ncop=nf-1
      call reccpy(io,io,i1,0,1,1,ncop)
      if(ncop.le.0) goto 30
      if(nflag.le.0) goto 10
      if(nflag.gt.nfmax) goto 30
      call flager(io,0,nf,nflag)
   10 call reccpy(io,io,i2,0,1,n1,ncopy)              
      if(ncopy.le.0) goto 30
      if(i1.eq.ifl) goto 20
      do 15 i=1,30
   15 hed(i)=hedj(i)
   20 call gfs_cldata(io)
      call gfs_dfil(io,ifl,iret)
      call gfs_cname(io,0,ifl,iret)
      nscan=n1+ncopy-1
      call gfs_rwdir(io,hed,ifl,'w',ierr)
      if(ierr.eq.0) return                
   30 call gfs_cldata(io)
      call gfs_dfil(io,0,iret)
      return
      end

      subroutine window(io,io1,k,k1,n1,nwd,ierr)
c window out segment of data nwd words long starting at point n1 in time series
c on unit io. If necessary ,flags are placed at the begining and/or the end
c of the new series which starts at point 1 in time series on unit io1
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,spare(20)
      common/workXX/a(30000)
      common/bits/flag,nfmax
      dimension dum(30)
      equivalence (dum,nscan)
      ierr=1
      if(n1.gt.nscan) return
      nrs=n1
      nflag=0
      if(n1.ge.1) goto 5
      nflag=iabs(n1-1)
      if(nflag.ge.nwd) return
      nrs=1
    5 ncopy=nwd-nflag 
      if(nflag.gt.nfmax) return
      ksave=k1
      if(io.eq.io1) k1=0
      call flager(io1,k1,1,nflag)
      nws=1+nflag
      call reccpy(io,io1,k,k1,nrs,nws,ncopy)   
      print *,ncopy,'  nccopy'
      if(ncopy.le.0) goto 50
      nend=nflag+ncopy
      nflage=nwd-nend          
      if(nflage.gt.nfmax) goto 50
      call flager(io1,k1,nend+1,nflage)
      call update(n1)
      nscan=nwd          
      call gfs_rwdir(io1,dum,ksave,'w',ierr)
   50 if(k1.ne.0) return
      call gfs_cldata(io1)
      call gfs_dfil(io1,ksave,iret)
      call gfs_cname(io1,0,ksave,iret)
      return
      end

      subroutine reccpy(io,io1,k,k1,n,n1,nwd)
c copy nwd data points from entry k on unit io starting at point n
c into entry k1 on unit io1 starting at point n1.If io1.le.0 then
c it is assumed that io1=io . nwd may be shorter
c on output if you asked for more data than available.
c Note that overwriting can cause problems if nwd.gt.30000.
      common/workXX/a(30000)
      lavail=30000
      jcopy=0
      if(nwd.le.0) goto 10
      if(n.le.0.or.n1.le.0) goto 10
      if(io1.le.0) io1=io
      jpts=nwd
      j1=n
      j2=n1
      if(io.eq.io1.and.k.eq.k1.and.jpts.gt.lavail) then
         print *,'warning -- overwriting long series is dangerous'
         stop
      end if
    5 nwr=min0(lavail,jpts)
      nwrs=nwr
      call gfs_rwdata(io,a,k,j1,nwr,'r',ierr)
      if(ierr.ne.0) goto 10                  
      call gfs_rwdata(io1,a,k1,j2,nwr,'w',ierr)
      if(ierr.ne.0) goto 10
      jpts=jpts-nwr
      j1=j1+nwr                  
      j2=j2+nwr
      jcopy=jcopy+nwr
      if(jpts.gt.0.and.nwr.eq.nwrs) goto 5
   10 nwd=jcopy            
      return   
      end

      subroutine flager(io,k,n1,nwd)
c write nwd flags into entry k on unit io starting at point n1
c nwd is returned as the actual number of flags written
      common/workXX/a(30000)
      common/bits/flag,nfmax
      lavail=30000          
      jcopy=0
      if(nwd.le.0.or.n1.le.0) goto 10
      nflag=min0(nwd,lavail)
      do 1 i=1,nflag
    1 a(i)=flag
      jpts=nwd
      j1=n1
    5 nwr=min0(lavail,jpts)
      call gfs_rwdata(io,a,k,j1,nwr,'w',ierr)
      if(ierr.ne.0) goto 10
      jpts=jpts-nwr
      j1=j1+nwr                  
      jcopy=jcopy+nwr
      if(jpts.gt.0) goto 5
   10 nwd=jcopy
      return   
      end

      subroutine update(n)
c updates header in myhed when start point of data is changed to n
c i.e. n=1 has no effect,n>1 makes start time later,n<1 makes
c start time earlier.
      real*8 t,ss,c1,c2,c3,c4,si
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,spare(20)
      data c1,c2,c3,c4/60.d0,24.d0,3600.d0,86400.d0/
      if(n.eq.1) return
      ss=sec
      si=dt
      t=((id*c2+ih)*c1+im)*c1+ss+(n-1)*si
    5 if(t.gt.0) goto 10
      iy=iy-1
      idpy=365
      if(iy-4*(iy/4).eq.0) idpy=366
      t=t+idpy*c4
      goto 5
   10 id=t/c4
      t=t-id*c4
      ih=t/c3
      t=t-ih*c3
      im=t/c1
      sec=t-im*c1
   15 idpy=365
      if(iy-4*(iy/4).eq.0) idpy=366
      if(id.le.idpy) return
      iy=iy+1
      id=id-idpy
      goto 15
      end

      function ndiff(iy,id,ih,im,s,jy,jd,jh,jm,sj,dt)
c computes relative start point of two time series , if the times are
c identical ndiff=1,if second time is greater then n1 is positive and
c vice versa.
      real*8 tstart,si,c1,c2,c3
      data c1,c2/60.d0,24.d0/
      c3=365.d0
      if(iy-4*(iy/4).eq.0) c3=366.d0
      tstart=((((jy-iy)*c3+jd-id)*c2+jh-ih)*c1+jm-im)*c1+sj-s
      si=dt
      if(tstart.ge.0.d0) ndiff=tstart/si+1.5d0
      if(tstart.lt.0.d0) ndiff=tstart/si+0.5d0
      return
      end

      subroutine flip1
      common/wkXXXX/dum(10000)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/bits/flag,nfmax
      equivalence (npts,dum)
      badun=0.7*flag
c*** flip by index 
      lavail=10000
      itype=-1
      call gfs_opena(2,'flip sign (by search) in file : ',itype,ierr)
      if(ierr.lt.0) return
      call tnoua('enter i1 and i2 > ')
      read(*,*)i1,i2
      if(i1.lt.1) goto 40
      i2=min0(ierr,i2)
      if(i2.lt.i1) goto 40
      ndel=i2-i1+1
      print *,'.....flipping ...'
      do 10 i=1,ndel                            
         kfl=i1+i-1
         call gfs_rwdir(2,dum,kfl,'r',iret)
         call prhdr(kfl,dum,itype)
      jpts=npts        
      j1=1
   15 nwr=min0(lavail,jpts)
      call gfs_rwdata(2,dum,kfl,j1,nwr,'r',ierr)
      do 20 k=1,nwr
 20      if (dum(k).lt.badun) dum(k)=-dum(k)
      call gfs_rwdata(2,dum,kfl,j1,nwr,'w',ierr)
      jpts=jpts-nwr
      j1=j1+nwr
      if(jpts.gt.0) goto 15
 10   continue                               
   40 call gfs_close(2)
      return
      end    

      subroutine flip
      common/wkXXXX/dum(10000)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/bits/flag,nfmax
      equivalence (npts,dum)
      badun=0.7*flag
c*** flip by search
      lavail=10000
      itype=-1
      call gfs_opena(2,'flip sign (by search) in file : ',itype,ierr)
      if(ierr.lt.0) return
      call gfs_phdef(2)
      call gfs_sdef(2)
      print *,'flipping the following entries'
    5 kfl=0
   10 call gfs_search(2,dum,kfl,0)
      if(kfl.lt.0) goto 40
      call prhdr(kfl,dum,itype)
      jpts=npts        
      j1=1
   15 nwr=min0(lavail,jpts)
      call gfs_rwdata(2,dum,kfl,j1,nwr,'r',ierr)
      do 20 i=1,nwr
   20    if (dum(i).lt.badun) dum(i)=-dum(i)
      call gfs_rwdata(2,dum,kfl,j1,nwr,'w',ierr)
      jpts=jpts-nwr
      j1=j1+nwr
      if(jpts.gt.0) goto 15
      goto 10                                
   40 call gfs_close(2)
      return
      end    

      subroutine zero
      common/bits/flag,nfmax
      real*8 sum
      common/wkXXXX/dum(10000)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      equivalence (npts,dum)
c*** remove mean
      badun=0.7*flag
      lavail=10000
      itype=-1
      call gfs_opena(2,'de-mean series in file : ',itype,ierr)
      if(ierr.lt.0) return
      nser=ierr
      do 10 kfl=1,nser
      call gfs_rwdir(2,dum,kfl,'r',ierr)
      call prhdr(kfl,dum,itype)
      sum=0.d0    
      nsum=0
      nscan=npts
      jpts=npts        
      j1=1
   15 nwr=min0(lavail,jpts)
      call gfs_rwdata(2,dum,kfl,j1,nwr,'r',ierr)
      do 20 i=1,nwr
      if(dum(i).lt.badun) then
        sum=sum+dum(i)
        nsum=nsum+1
      endif
   20 continue
      jpts=jpts-nwr
      j1=j1+nwr
      if(jpts.gt.0) goto 15
      if(nsum.eq.0)goto 10
      sum=sum/nsum 
      print *,'mean value is ',sum
      jpts=nscan
      j1=1
   25 nwr=min0(lavail,jpts)
      call gfs_rwdata(2,dum,kfl,j1,nwr,'r',ierr)
      do 30 i=1,nwr
      if(dum(i).lt.badun) dum(i)=dum(i)-sum
   30 continue
      call gfs_rwdata(2,dum,kfl,j1,nwr,'w',ierr)
      jpts=jpts-nwr
      j1=j1+nwr
      if(jpts.gt.0) goto 25
   10 continue
      call gfs_close(2)
      return
      end    

      SUBROUTINE SHELL(A,NCOL)
C  SHELL SORTS A(NC) INTO INCREASING ORDER
C  NOTE THAT THIS VERSION ASSUMES THAT A IS AN INTEGER ARRAY
      INTEGER A(NCOL),TE
      IG=NCOL
    5 IF(IG.LE.1) RETURN
      IG=IG/2
      IM=NCOL-IG
   10 IEX=0
      DO 20 I=1,IM
      IPG=I+IG
      IF(A(I).LE.A(IPG)) GO TO 20
      TE=A(I)
      A(I)=A(IPG)
      A(IPG)=TE
      IEX=IEX+1
   20 CONTINUE
      IF(IEX.GT.0) GO TO 10
      GO TO 5
      END

      subroutine delete(io,indx,num)
      dimension indx(1)
      common/wkXXXX/hed(2000),kdel(8000)
      call shell(indx,num)
      j=1
      kdel(j)=indx(1)
      do 20 i=2,num
      if (indx(i).ne.indx(i-1)) then
        j=j+1
        kdel(j)=indx(i)
      endif
   20 continue
c now delete the records from the bottom up
      call dentries(io,j)
      return
      end

