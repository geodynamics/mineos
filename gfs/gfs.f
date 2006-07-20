c  No guarantee is made that this code is bug-free. If a bug is found or
c  if you have made an improvement or would like a particular improvement
c  please contact Guy Masters (619) 534-4122 . I would also appreciate
c  being contacted if you want to give this code to someone else so that
c  they can be informed of any modifications.
c**************************************************************************

      subroutine GFS_OPENA(io,rem,itype,iret)
c file opener for interactive use,rem is a remark to be printed
      character*(*) rem
      character*256 pname
      do 5 i=1,256
    5   pname(i:i)=' '
      call tnoua(rem)
      read(*,'(a256)') pname
      call gfs_open(io,pname,itype,iret)
      return
      end

      subroutine tnoua(string)
c use dollar format to disable carriage return (works for Apollo and Sun)
      character*(*) string
      write(*,900) string
  900 format(a,$)
      return
      end

      subroutine GFS_DENTRY(io,kfl)
c  delete entry # kfl from file on unit io
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/wkXXXX/dum(10000)
      if(kfl.lt.1.or.kfl.gt.nfil(io)) return
      if(nfil(io).le.1) then
        call gfs_delete(io)
        return
      end if
      call gfs_cldata(io)
      if(nwb(io).ne.0) call gfs_dfil(io,kfl,iret)
      if(kfl.lt.nfil(io)) then
        do 10 i=kfl+1,nfil(io)
          call gfs_rwdir(io,dum,i,'r',iret)
          call gfs_rwdir(io,dum,i-1,'w',iret)
          if(nwb(io).ne.0) call gfs_cname(io,i,i-1,iret)
   10     continue
      end if
      nfil(io)=nfil(io)-1
      call gfs_hddir(io,nfil(io),ktype(io),ierr)
      return
      end

      subroutine GFS_CENTRY(io,io1,kfl,kfl1)
c  copy entry from unit io to unit io1
      common/wkXXXX/dum(10000)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      equivalence (npts,dum)
      if(nwb(io).ne.nwb(io1)) return
      if(kfl.lt.1.or.kfl.gt.nfil(io)) return
      call gfs_rwdir(io,dum,kfl,'r',iret)
      jpts=npts
      call gfs_rwdir(io1,dum,kfl1,'w',iret)
      if(iret.ne.0) goto 500
      if(jpts.eq.0.or.nwb(io).eq.0) return
      lavail=40000/nwb(io)
      jfl1=kfl1
      if(kfl1.lt.1.or.kfl1.gt.nfil(io1)) jfl1=nfil(io1)
      j1=1
    5   nwr=min0(lavail,jpts)
        call gfs_rwdata(io,dum,kfl,j1,nwr,'r',ierr)
        call gfs_rwdata(io1,dum,jfl1,j1,nwr,'w',ierr)
        if(ierr.ne.0) goto 500
        jpts=jpts-nwr
        j1=j1+nwr
        if(jpts.gt.0) goto 5
      return
  500 print *,'unable to copy entry ',kfl
      call gfs_dentry(io1,jfl1)
      return
      end

      subroutine GFS_RWENTRY(io,head,a,kfl,op)
      dimension head(*),a(*)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      character*1 op
      equivalence(dum,nscan)
      if(op.eq.'r') goto 5
      if(op.ne.'w') return
      if(kfl.ge.1.and.kfl.le.nfil(io)) goto 10
c*** append
      call gfs_rwdir(io,head,0,'w',iret)
      dum=head(1)
      call gfs_rwdata(io,a,nfil(io),1,nscan,'w',ierr)
      if(iret+ierr.eq.0)return
      print *,'unable to write entry ',nfil(io)
      call gfs_dentry(io,nfil(io))
      return
c*** read
    5 if(kfl.gt.nfil(io)) goto 6
      call gfs_rwdir(io,head,kfl,'r',iret)
      dum=head(1)
      call gfs_rwdata(io,a,kfl,1,nscan,'r',ierr)
      if(iret+ierr.eq.0) return
    6 print *,'unable to read entry ',kfl
      return
c*** overwrite
   10 call gfs_rwdir(io,head,kfl,'w',iret)
      dum=head(1)
      call gfs_rwdata(io,a,kfl,1,nscan,'w',ierr)
      if(iret+ierr.eq.0)return
      print *,'unable to overwrite entry ',kfl
      call gfs_dentry(io,kfl)
      return
      end

      subroutine GFS_DFIL(io,ifl,iret)
c  deletes time series # ifl
      common/nmXXXX/pathname(10),plen(10)
      character pathname*256,path*256,filnam*8
      integer unlink,plen
      call gfs_fname(filnam,ifl)
      path=pathname(io)(1:plen(io))//'/'//filnam(1:8)
      iret=unlink(path)
      return
      end

      subroutine GFS_CNAME(io,iold,inew,iret)
c  change name of iold to inew, inew must not exist
      common/nmXXXX/pathname(10),plen(10)
      character pathname*256,pathold*256,pathnew*256
      character*8 oldnam,newnam
      integer rename,plen
      call gfs_fname(oldnam,iold)
      call gfs_fname(newnam,inew)
      pathold=pathname(io)(1:plen(io))//'/'//oldnam(1:8)
      pathnew=pathname(io)(1:plen(io))//'/'//newnam(1:8)
      iret=rename(pathold,pathnew)
      return
      end

      subroutine GFS_FNAME(filname,n)
c  composes 8 character file name for entry # n
      character*8 filname
      filname(1:3)='ts.'
      in=n
      do i=5,1,-1
        ichr=mod(in,10)
        j=i+3
        write(filname(j:j),'(i1)') ichr
        in=(in-ichr)/10
      enddo
      return
      end

      subroutine GFS_SWAP(io,k1,k2)
c  swap entries k1 and k2 on unit io
      common/wkXXXX/dum1(5000),dum2(5000)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      kdum=99999
      if(k1.lt.1.or.k1.gt.nfil(io)) return
      if(k2.lt.1.or.k2.gt.nfil(io)) return
      call gfs_rwdir(io,dum1,k1,'r',iret)
      call gfs_rwdir(io,dum2,k2,'r',iret)
      call gfs_rwdir(io,dum1,k2,'w',iret)
      call gfs_rwdir(io,dum2,k1,'w',iret)
      if(nwb(io).eq.0) return
      call gfs_cname(io,k1,kdum,iret)
      call gfs_cname(io,k2,k1,iret)
      call gfs_cname(io,kdum,k2,iret)
      return
      end

      subroutine GFS_SDEF(io)
c*** interactively gets search parameters
      common/srXXXX/nlist,list(40),flist(10,40)
      common/atXXXX/natt(10),iatt(3,20,10),attrem(20,10)
      character*16 attrem
      real*8 dreel(5)
      dimension nam(10),reel(10)
      equivalence(nam,reel,dreel)
      nlist=0
   20 call tnoua('enter search index (0 quits) : ')
      read(*,*,err=20) is
      if(nlist.eq.40.or.is.eq.0) return
      if(is.gt.natt(io)) goto 40
      if(is.lt.0) goto 30
      call tnoua(attrem(is,io))
      ktype=iatt(3,is,io)+1
      goto (1,2,3,4,5,6,7,8,9),ktype
    1 read(*,100,err=20) nam
  100 format(10a4)
      goto 10
    2 read(*,*,err=20) nam(1)
      goto 10
    3 read(*,*,err=20) reel(1)
      goto 10
    4 read(*,*,err=20) dreel(1)
      goto 10
    5 read(*,*,err=20) nam(1),nam(2)
      goto 10
    6 read(*,*,err=20) reel(1),reel(2)
      goto 10
    7 read(*,*,err=20) dreel(1),dreel(2)
      goto 10
    8 read(*,*,err=20) jy,jd,jh,jm,sj
      call dattoe(dreel(1),jy,jd,jh,jm,sj)
      goto 10
    9 read(*,*,err=20) jy,jd,jh,jm,sj,ky,kd,kh,km,sk
      call dattoe(dreel(1),jy,jd,jh,jm,sj)
      call dattoe(dreel(2),ky,kd,kh,km,sk)
   10 nlist=nlist+1
      list(nlist)=is
      do i=1,10
        flist(i,nlist)=reel(i)
      enddo
      goto 20
   30 icanc=iabs(is)
      if(icanc.gt.nlist) goto 20
      if(icanc.lt.nlist) then
        do j=icanc+1,nlist
          list(j-1)=list(j)
          do i=1,10
            flist(i,j-1)=flist(i,j)
          enddo
        enddo
      end if
      nlist=nlist-1
   40 call gfs_prlist(io)
      goto 20
      end

      subroutine etodat(time,iy,id,ih,im,sec)
c converts epochal time to yr,day,hr,mn,sec ,CSS definition of epochal
c time is the time in secs from 1970 day 0
      real*8 time,t
      t=time
      id=t/86400.d0
      t=t-id*86400.d0
      ih=t/3600.d0
      t=t-ih*3600.d0
      im=t/60.d0
      sec=t-im*60.d0
      id=id+1
      iy=1970
    5   idpy=365
        if(iy-4*(iy/4).eq.0) idpy=366
        if(id.le.idpy) return
        iy=iy+1
        id=id-idpy
        goto 5
      end

      subroutine dattoe(time,iy,id,ih,im,sec)
c converts yr,day,hr,mn,sec to epochal time which is defined by CSS to
c be the time in secs from 1970 day 0
      real*8 time
      ky=iy
      time=(((id-1.d0)*24.d0+ih)*60.d0+im)*60.d0+sec
    5   if(ky.eq.1970) return
        ky=ky-1
        idpy=365
        if(ky-4*(ky/4).eq.0) idpy=366
        time=time+86400.d0*idpy
        goto 5
      end

      subroutine GFS_PRLIST(io)
c*** note that this prints epochal time for attribute type 8 or 9
      common/srXXXX/nlist,list(40),flist(10,40)
      common/atXXXX/natt(10),iatt(3,20,10),attrem(20,10)
      character*16 attrem
      real*8 dreel(5)
      dimension nam(10),reel(10)
      equivalence(nam,reel,dreel)
      if(nlist.le.0) return
      print *,'list #    attribute                 set values'
      do 10 i=1,nlist
        i3=iatt(3,list(i),io)+1
        do j=1,10
          reel(j)=flist(j,i)
        enddo
        goto (1,2,3,4,5,6,7,4,7),i3
    1   iwd=iatt(2,list(i),io)-iatt(1,list(i),io)+1
        print 110,i,attrem(list(i),io),(nam(j),j=1,iwd)
  110   format(i5,5x,a16,11x,10a4)
        goto 10
    2   print 120,i,attrem(list(i),io),nam(1)
  120   format(i5,5x,a16,5x,2i10)
        goto 10
    3   print 130,i,attrem(list(i),io),reel(1)
  130   format(i5,5x,a16,5x,2g14.6)
        goto 10
    4   print 130,i,attrem(list(i),io),dreel(1)
        goto 10
    5   print 120,i,attrem(list(i),io),nam(1),nam(2)
        goto 10
    6   print 130,i,attrem(list(i),io),reel(1),reel(2)
        goto 10
    7   print 130,i,attrem(list(i),io),dreel(1),dreel(2)
   10   continue
      return
      end

      subroutine GFS_PHDEF(io)
      common/atXXXX/natt(10),iatt(3,20,10),attrem(20,10)
      character*16 attrem,comment(9)
      data comment/'character string','integer (equal) ',
     +             'real*4  (equal) ','real*8  (equal) ',
     +             'integer (range) ','real*4  (range) ',
     +             'real*8  (range) ','time    (equal) ',
     +             'time    (range) '/
      if(natt(io).eq.0) return
      print *,'index    attribute               type'
      do i=1,natt(io)
        ipr=iatt(3,i,io)+1
        print 900,i,attrem(i,io),comment(ipr)
      enddo
  900 format(i4,5x,a16,5x,a16)
      return
      end

      subroutine GFS_SEARCH(io,hed,kfl,itype)
c  itype=0 specifies 'and' type search
c  itype.ne.0 specifies 'or' type search
c  modified so that this takes all entries if nlist = 0
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/srXXXX/nlist,list(40),flist(10,40)
      dimension hed(1)
      kfl=kfl+1
      kfl=max0(1,kfl)
      if(itype.eq.0.and.nlist.gt.1) goto 11
    5 if(kfl.gt.nfil(io)) goto 500
      call gfs_rwdir(io,hed,kfl,'r',iret)
      if(nlist.le.0) return
      do i=1,nlist
        if(matchX(io,i,hed).eq.1) return
      enddo
      kfl=kfl+1
      goto 5
   11 nind=nlist
      do 12 k=2,nlist
        iflag=1
        km=k-1
        do 13 i=1,km
          if(list(i).eq.list(k)) goto 12
   13     continue
        iflag=0
   12   nind=nind-iflag
   15 if(kfl.gt.nfil(io)) goto 500
      call gfs_rwdir(io,hed,kfl,'r',iret)
      match=0
      do i=1,nlist
        match=match+matchX(io,i,hed)
      enddo
      if(match.eq.nind) return
      kfl=kfl+1
      goto 15
  500 kfl=-1
      return
      end

      function MATCHX(io,k,hed)
c  returns 1 if a match,otherwise returns 0
      common/srXXXX/nlist,list(40),flist(10,40)
      common/atXXXX/natt(10),iatt(3,20,10),attrem(20,10)
      character*16 attrem
      real*8 dh,dconvX
      dimension hed(*)
      matchX=1
      i1=iatt(1,list(k),io)
      ktype=iatt(3,list(k),io)+1
      goto (1,22,2,3,4,5,6,7,8),ktype
    1 j=0
      i2=iatt(2,list(k),io)
      do 10 i=i1,i2
      j=j+1
      if(hed(i).ne.flist(j,k)) matchX=0
   10 continue
      return
   22 if(iconvX(hed(i1)).ne.iconvX(flist(1,k))) matchX=0
      return
    2 if(hed(i1).ne.flist(1,k)) matchX=0
      return
    3 if(dconvX(hed(i1)).ne.dconvX(flist(1,k))) matchX=0
      return
    4 ih=iconvX(hed(i1))
      if(ih.lt.iconvX(flist(1,k)).or.ih.gt.iconvX(flist(2,k))) matchX=0
      return
    5 if(hed(i1).lt.flist(1,k).or.hed(i1).gt.flist(2,k)) matchX=0
      return
    6 dh=dconvX(hed(i1))
      if(dh.lt.dconvX(flist(1,k)).or.dh.gt.dconvX(flist(3,k))) matchX=0
      return
    7 ky=iconvX(hed(i1))
      kd=iconvX(hed(i1+1))
      kh=iconvX(hed(i1+2))
      km=iconvX(hed(i1+3))
      call dattoe(dh,ky,kd,kh,km,hed(i1+4))
      if(dh.ne.dconvX(flist(1,k))) matchX=0
      return
    8 ky=iconvX(hed(i1))
      kd=iconvX(hed(i1+1))
      kh=iconvX(hed(i1+2))
      km=iconvX(hed(i1+3))
      call dattoe(dh,ky,kd,kh,km,hed(i1+4))
      if(dh.lt.dconvX(flist(1,k)).or.dh.gt.dconvX(flist(3,k))) matchX=0
      return
      end

      function ICONVX(reel)
      equivalence (jconv,a)
      a=reel
      iconvX=jconv
      return
      end

      real*8 function DCONVX(reel)
      real*4 reel(*),dum(2)
      real*8 a
      equivalence (a,dum)
      dum(1)=reel(1)
      dum(2)=reel(2)
      dconvX=a
      return
      end

      integer function CLENX(pathname)
c counts number of non-blank characters in the name and left justifies
c if there are leading blanks
      character*(*) pathname
      clenX=0
      jchar=len(pathname)
      do 5 i=1,jchar
        k=i
        if(pathname(i:i).ne.' ') goto 10
    5   continue
      return
   10 do 20 i=k,jchar
        if(pathname(i:i).eq.' ') goto 30
   20   clenX=clenX+1
   30 if(k.eq.1) return
      do 40 i=1,clenX
        j=k+i-1
   40   pathname(i:i)=pathname(j:j)
      return
      end

      subroutine GFS_CLOSE(ios)
c  closes directory and releases unit for future use
      common/stXXXX/lid(20)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/nmXXXX/pathname(10),plen(10)
      character*256 pathname
      integer plen
      io=abs(ios)
      if(ios.lt.0.or.nfil(io).eq.0) then
        call gfs_delete(io)
        return
      end if
      call gfs_cldata(io)
      call gfrefil(lid(io))
      plen(io)=0
      nfil(io)=0
      return
      end

      subroutine GFS_DELETE(io)
c  deletes file on unit io and releases unit for future use
      common/stXXXX/lid(20)
      common/nmXXXX/pathname(10),plen(10)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      character*(256) pathname
      integer system,plen
      call gfs_cldata(io)
      call gfrefil(lid(io))
      iret=system('rm -r '//pathname(io)(1:plen(io)))
      plen(io)=0
      nfil(io)=0
      return
      end

      subroutine GFS_ERASE(io)
c  erases contents of file on unit io
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/stXXXX/lid(20)
      if(nfil(io).eq.0) return
      if(nwb(io).gt.0) then
        call gfs_cldata(io)
        do  i=1,nfil(io)
          call gfs_dfil(io,i,iret)
        enddo
      end if
      nfil(io)=0
      return
      end

      subroutine GFS_RWDIR(io,hdr,kfl,op,ierr)
c  read and write directory entries
      common/stXXXX/lid(20)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      dimension hdr(*)
      character*1 op
      ierr=1
      idum=1
      l4=lhed(io)*4
      nb = kfl*l4
      if(op.eq.'r') then
c*** read existing header
c        call gpodiscb(lid(io),idum,nb)
        call gpodiscb(lid(io),nb)
        call grddiscb(lid(io),hdr,l4,istat)
        if(istat.ge.0) ierr=0
      else if(op.eq.'w') then
c*** append or write header and update directory header
        ierr=0
        if(kfl.le.0.or.kfl.gt.nfil(io)) nb=(nfil(io)+1)*l4
c        call gpodiscb(lid(io),idum,nb)
        call gpodiscb(lid(io),nb)
        call gwrdiscb(lid(io),hdr,l4,istat)
        if(istat.ne.l4) then
           ierr=1
           return
        end if
        if(kfl.ge.1.and.kfl.le.nfil(io)) return
        nfil(io)=nfil(io)+1
        call gfs_hddir(io,nfil(io),ktype(io),ierr)
      end if
      return
      end

      subroutine GFS_HDDIR(io,jentry,jtype,ierr)
c update directory header
      common/stXXXX/lid(20)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/wkXXXX/nentry,ntype,dum(9998)
      dimension dum2(10000)
      equivalence (nentry,dum2)
      l4=lhed(io)*4
      idum=1
      izer=0
      nentry=jentry
      ntype=jtype
c      call gpodiscb(lid(io),idum,izer)
      call gpodiscb(lid(io),izer)
      call gwrdiscb(lid(io),dum2,l4,istat)
      ierr=0
      if(istat.ne.l4) ierr=1
      return
      end

      subroutine GFS_RWDATA(io,buf,kfl,n1,nwd,op,ierr)
c read and write data blocks
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/stXXXX/lid(20)
      dimension buf(*)
      character*1 op
      ierr=0
      idum=1
      if(nwd.le.0.or.nwb(io).eq.0) return
      ierr=1
      if(kfl.lt.0) return
      if(kfl.ne.kopen(io)) then
        call gfs_cldata(io)
        call gfs_opdata(io,kfl,ierr)
        if(ierr.ne.0) return
        kopen(io)=kfl
      end if
      lun=lid(io+10)
      ipnt=(n1-1)*nwb(io)
c      call gpodiscb(lun,idum,ipnt)
      call gpodiscb(lun,ipnt)
      nwr=nwd*nwb(io)
      if(op.eq.'r') then
c*** read
        call grddiscb(lun,buf,nwr,ierr)
        if(ierr.lt.0) return
        nwd=ierr/nwb(io)
        ierr=0
      else if (op.eq.'w') then
c*** write 
        call gwrdiscb(lun,buf,nwr,istat)
        ierr=0
        if(istat.ne.nwr) ierr=1
      end if
      return
      end

      subroutine GFS_OPDATA(io,kfl,ierr)
c opens data file
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/stXXXX/lid(20)
      common/nmXXXX/pathname(10),plen(10)
      character pathname*256,path*256,filnam*8
      integer plen
      logical there
      if(nwb(io).eq.0) return
      call gfs_fname(filnam,kfl)
      path=pathname(io)(1:plen(io))//'/'//filnam(1:8)
c*** see if it exists
      inquire (file=path,exist=there)
      if (there) then
        call ggetfil(4,lun,path,ierr)
      else
        call ggetfil(3,lun,path,ierr)
      end if
      lid(io+10)=lun
      return
      end

      subroutine GFS_CLDATA(io)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/stXXXX/lid(20)
      if(nwb(io).eq.0.or.kopen(io).lt.0) return
      if(kopen(io).gt.0) then
         call gfs_truncate(io)
      else
        call gfrefil(lid(io+10))
        kopen(io)=-1
      end if
      return
      end

      subroutine GFS_TRUNCATE(io)
      common/stXXXX/lid(20)
      common/wkXXXX/dum(10000)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/nmXXXX/pathname(10),plen(10)
      character pathname*256,path*256,filnam*8,fdum*8
      integer plen,stat,statb(13),unlink,rename
      equivalence (npts,dum1)
c*** get number of points in header
      fdum(1:7)='ts.temp'
      idum=1
      nb=kopen(io)*lhed(io)*4
c      call gpodiscb(lid(io),idum,nb)
      call gpodiscb(lid(io),nb)
      call grddiscb(lid(io),dum1,4,ierr)
      if(ierr.ne.4) return
c*** get number of bytes in file
      call gfs_fname(filnam,kopen(io))
      path=pathname(io)(1:plen(io))//'/'//filnam(1:8)
      ierr = stat(path,statb)
      if(npts*nwb(io).ge.statb(8)) then
        call gfrefil(lid(io+10))
        kopen(io)=-1
        return
      end if
c*** copy npts of data to a temp file
      call ggetfil(3,lun,fdum,ierr)
      if(ierr.ne.0) return
      nbc=npts*nwb(io)
      j1=0
    5   nwr=min0(40000,nbc)
c        call gpodiscb(lid(io+10),idum,j1)
        call gpodiscb(lid(io+10),j1)
        call grddiscb(lid(io+10),dum,nwr,ierr)
        if(ierr.lt.0) goto 500
c        call gpodiscb(lun,idum,j1)
        call gpodiscb(lun,j1)
        call gwrdiscb(lun,dum,nwr,iret)
        if(iret.ne.nwr) goto 500
        nbc=nbc-nwr
        j1=j1+nwr
        if(nbc.gt.0) goto 5
c*** replace ts file with temp file
      call gfrefil(lun)
      call gfrefil(lid(io+10))
      call gfs_dfil(io,kopen(io),iret)
      iret=rename(fdum,path)
      kopen(io)=-1
      return
  500 print *,'unable to truncate entry ',kopen(io)
      call gfrefil(lun)
      call gfrefil(lid(io+10))
      kopen(io)=-1
      iret=unlink(fdum)
      return
      end

      subroutine GFS_OPEN(io,pname,itype,iret)
c  if gfs file exists :
c     attaches to sub-directory and returns the # of entries in 'iret'
c     and the header type in 'itype'
c  if gfs file doesnt exist :
c     creates file and directory, returns a value of zero in 'iret' if
c     successful, itype must be input and must be positive
c     or an error will be generated-this feature can be used when you want
c     to be sure that a file previously exists on opening.
c  iret = 0 successful call on new file
c  iret .gt. 0 successful call on old file(=nentry)
c  iret=-1 fatal error before creating anything
c  iret=-2 fatal error while opening existing file
c  iret=-3 fatal error while opening new file,any mess is cleaned up.
      common/nmXXXX/pathname(10),plen(10)
      common/hdXXXX/lhed(10),nwb(10),nfil(10),ktype(10),kopen(10)
      common/wkXXXX/dum(10000)
      common/stXXXX/lid(20)
      character pathname*256,treename*256,path*256
      character*(*) pname
      integer getcwd,system,chdir,clenX,tlen,plen
      logical there
      equivalence (nentry,dum(1)),(ntype,dum(2))
      save treename
      data istart/1/
      iret=-1
      idum=1
      izer=0
      if(istart.ne.0) then
        istart=0
        do i=1,10
          plen(i)=0
        enddo
      end if
c***  do this every time to avoid losing treename
      ierr=getcwd(treename)
      if(ierr.ne.0) goto 500
      tlen=clenX(treename)
c***
      if((io.lt.1.or.io.gt.10).or.plen(io).ne.0) then
        print *,'unit already open or out of range (1-10) :',io
        return
      end if
      nlen=clenX(pname)
      if(nlen.eq.0) return
      path=pname(1:nlen)//'/directry'
      inquire(file=path,exist=there)
      if(.not.there) then
        if(itype.lt.0) then
          print *,'GFS file does not exist'
          return
        end if
        ierr=system('mkdir '//path(1:nlen))
        if(ierr.ne.0) goto 500
      end if
      ierr=chdir(pname(1:nlen))
      if(ierr.ne.0) goto 500
      ierr=getcwd(pathname(io))
      if(ierr.ne.0) goto 500
      plen(io)=clenX(pathname(io))
      ierr=chdir(treename(1:tlen))
      if(ierr.ne.0) goto 500
      if(there) then
c*** open existing directory
        iret=-2
        call ggetfil(4,lid(io),path,ierr)
c        print *,'in OPEN:  ',lid(io),ierr
        if(ierr.ne.0) goto 500
c        call gpodiscb(lid(io),idum,izer)
        call gpodiscb(lid(io),izer)
        call grddiscb(lid(io),dum,8,ierr)
        if(ierr.ne.8) goto 500
        nfil(io)=nentry
        ktype(io)=ntype
        if(itype.ne.ntype) itype=ntype
c        itype=ntype
        call defhdr(io,itype)
      else
c*** open new directory
        iret=-3
        nentry=0
        nfil(io)=0
        ktype(io)=itype
        call defhdr(io,itype)
        if(lhed(io).le.0) goto 500
        call ggetfil(3,lid(io),path,ierr)
        if(ierr.ne.0) goto 500
        call gfs_hddir(io,nentry,itype,ierr)
        if(ierr.ne.0) goto 500
      end if
      iret=nentry
      kopen(io)=-1
      return
c  error in opening
  500 print *,'GFS file not opened, error code = ',ierr
      if(iret.eq.-3) call gfs_close(io)
      if(iret.ne.-2) return
      call gfrefil(lid(io))
      plen(io)=0
      nfil(io)=0
      return
      end
