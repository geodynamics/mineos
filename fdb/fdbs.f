c
c MINEOS version 1.0 by Guy Masters, John Woodhouse, and Freeman Gilbert
c
c This program is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version.
c
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
c
c**************************************************************************
c
c fdb utilities for site, sitechan, and wfdisc relations.
c
c**************************************************************************
c   Read an site relation
c			idbin -- database handle -- in fdb_io.h
c**************************************************************************
	subroutine read_site
	implicit none
        include 'fdb_site.h'
        include 'fdb_io.h'
c === Local variables ===
        integer*4 i, j, n, ind, idbsite, lnblnk
        character*200 str,str1
c		declarations specific to this routine

cxx	print *,'reading site relation.....'

        idbsite=58
        nrowsite = 0
        open(idbsite,file=dbin(1:lnblnk(dbin))//'.site',
     *        err = 99, status='old')

        i=1
 1      if(i.gt.msite) then
            print *, 'ERR032: fdb: Dimension of site arrays too small'
            print *,i,' > ',msite
            stop
        endif 
          read(idbsite,2,end=999) 
     *     sta_site(i), ondate_site(i), offdate_site(i),
     *     lat_site(i), lon_site(i), elev_site(i), staname_site(i),
     *     statype_site(i), refsta_site(i), dnorth_site(i),
     *     deast_site(i), lddate_site(i)
c
c fix IRIS bug in rdseed
c
        if(lnblnk(statype_site(i)).eq.0) then
          read(idbsite,'(a200)',end=999) str
          n = lnblnk(str)
          ind = 1
          do j = 1,n
            if(str(j:j).ne.' ') then
              ind = j
              goto 10
            endif
          enddo
  10      str1 = str(ind:n)
          read(str1,3)
     *     statype_site(i), refsta_site(i), dnorth_site(i),
     *     deast_site(i), lddate_site(i)
        endif
c
c fix end
c
    2   format(a6,1x,2(i8,1x),3(f9.4,1x),a50,1x,a4,1x,a6,1x,
     *        2(f9.4,1x),a17)
    3   format(a4,1x,a6,1x,2(f9.4,1x),a17)
        i=i+1
        goto 1
 999    nrowsite = i - 1
        close(idbsite)
  99    if(nrowsite.le.0) then
        write(*,*)
     *    'ERR033: fdb: site relation ',dbin(1:lnblnk(dbin)),
     *     ' appears to be empty, or does not exist'
        endif
    	return
	end

c*********************************************************************
c   Read an sitechan relation
c			idbin -- database handle -- in fdb_io.h
c*********************************************************************
	subroutine read_sitechan
	implicit none
        include 'fdb_sitechan.h'
        include 'fdb_io.h'
c === Local variables ===
        integer*4 i, idbsitechan, lnblnk
c		declarations specific to this routine

cxx	print *,'reading sitechan relation.....'

        idbsitechan=58
        nrowsitechan = 0
        open(idbsitechan,file=dbin(1:lnblnk(dbin))//'.sitechan',
     *        err = 99, status='old')

        i=1
 1      if(i.gt.msitechan) then
            print *, 'ERR032: Dimension of sitechan arrays too small'
            print *,i,' > ',msitechan
            stop
        endif 
          read(idbsitechan,2,end=999) sta_sitechan(i),chan_sitechan(i),
     *     ondate_sitechan(i),chanid_sitechan(i),offdate_sitechan(i),
     *     ctype_sitechan(i),edepth_sitechan(i),hang_sitechan(i),
     *     vang_sitechan(i),descrip_sitechan(i),lddate_sitechan(i)
    2  format(a6,1x,a8,1x,3(i8,1x),a4,1x,f9.4,1x,2(f6.1,1x),a50,1x,a17)
        i=i+1
        goto 1
 999    nrowsitechan = i - 1
        close(idbsitechan)
  99    if(nrowsitechan.le.0) then
        write(*,*)
     *    'ERR033: fdb: sitechan relation ',dbin(1:lnblnk(dbin)),
     *     ' appears to be empty, or does not exist'
        endif
	return
	end

c*********************************************************************
c   Write a site relation
c*********************************************************************
	subroutine write_site
	implicit none
        include 'fdb_site.h'
        include 'fdb_io.h'
c === Local variables ===
cxx	character*24 date
        integer*4 i, idbsite, lnblnk

cxx	print *,'writing site relation.....'

        idbsite=58
        open(idbsite,file=dbout(1:lnblnk(dbout))//'.site',
     *       access='APPEND')
cxx	call fdate(date)
cxx	lddate_site(1)=date(5:10)//','//date(21:24)

	do i=1,nrowsite
         write(idbsite,2) sta_site(i), ondate_site(i), offdate_site(i),
     *     lat_site(i), lon_site(i), elev_site(i), staname_site(i),
     *     statype_site(i), refsta_site(i), dnorth_site(i),
     *     deast_site(i), lddate_site(i)
    2   format(a6,1x,2(i8,1x),3(f9.4,1x),a50,1x,a4,1x,a6,1x,
     *         2(f9.4,1x),a17)
	enddo

        close(idbsite)
	return	
	end

c*********************************************************************
c   Write a sitechan relation
c	writes sitechan table located in common\csitechan
c	to the specified data base named dbout.
c	idbout -- input in fdb_io.h.
c	WARNING: if JSPC_DBPATH is set, it will override
c	the specified output database dbout.
c*********************************************************************
	subroutine write_sitechan
	implicit none
        include 'fdb_sitechan.h'
        include 'fdb_io.h'
c === Local variables ===
cxx	character*24 date
        integer*4 i, idbsitechan, lnblnk

cxx	print *,'writing sitechan relation.....'

        idbsitechan=58
        open(idbsitechan,file=dbout(1:lnblnk(dbout))//'.sitechan',
     *       access='APPEND')
cxx	call fdate(date)
cxx 	lddate_sitechan(1)=date(5:10)//','//date(21:24)

	do i=1,nrowsitechan
         write(idbsitechan,2) sta_sitechan(i),chan_sitechan(i),
     *     ondate_sitechan(i),chanid_sitechan(i),offdate_sitechan(i),
     *     ctype_sitechan(i),edepth_sitechan(i),hang_sitechan(i),
     *     vang_sitechan(i),descrip_sitechan(i),lddate_sitechan(i)
    2   format(a6,1x,a8,1x,3(i8,1x),a4,1x,f9.4,1x,2(f6.1,1x),a50,
     *         1x,a17)
	enddo

        close(idbsitechan)
	return	
	end

c*********************************************************************
c   sets default values for variables in common c_site, row = ituple
c*********************************************************************
	subroutine default_site(ituple)
	implicit none
        include 'fdb_site.h'
        integer*4 ituple
        character*17 loctime
c   a valid entry for sta is required under the CSS v. 3.0 standard
	sta_site(ituple) = '-'
c   a valid entry for ondate is required under the CSS v. 3.0 standard
	ondate_site(ituple) = -1
	offdate_site(ituple) = -1
	lat_site(ituple) = -999.0
	lon_site(ituple) = -999.0
	elev_site(ituple) = -999.0
	staname_site(ituple) = '-'
	statype_site(ituple) = '-'
	refsta_site(ituple) = '-'
	dnorth_site(ituple) = 0.0
	deast_site(ituple) = 0.0
	lddate_site(ituple) = loctime()

	return
	end

c*********************************************************************
c sets default values for variables in common c_sitechan, row = ituple
c*********************************************************************
	subroutine default_sitechan(ituple)
	implicit none
        include 'fdb_sitechan.h'
        integer*4 ituple
        character*17 loctime
c  a valid entry for sta is required under the CSS v. 3.0 standard
	sta_sitechan(ituple) = '-'
c  a valid entry for chan is required under the CSS v. 3.0 standard
	chan_sitechan(ituple) = '-'
c  a valid entry for ondate is required under the CSS v. 3.0 standard
	ondate_sitechan(ituple) = -1 
	chanid_sitechan(ituple) = -1
	offdate_sitechan(ituple) = -1
	ctype_sitechan(ituple) = '-'
	edepth_sitechan(ituple) = -9.9999
c  a valid entry for hang is required under the CSS v. 3.0 standard
	hang_sitechan(ituple) = -999.0
c  a valid entry for vang is required under the CSS v. 3.0 standard
	vang_sitechan(ituple) = -999.0
	descrip_sitechan(ituple) = '-'
	lddate_sitechan(ituple) = loctime()

	return
	end
c*********************************************************************
c  Read an wfdisc relation
c*********************************************************************
	subroutine read_wfdisc
	implicit none
        include 'fdb_wfdisc.h'
        include 'fdb_io.h'
c === Local variables ===
        integer*4 i, lnblnk
c		declarations specific to this routine

cxx	print *,'reading wfdisc relation.....'

        idbwfdisc=58
        nrowwfdisc = 0
        open(idbwfdisc,file=dbin(1:lnblnk(dbin))//'.wfdisc',
     *       err = 99, status='old')

        i=1
 1      if(i.gt.mwfdisc) then
            print *, 'ERR032: fdb:Dimension of wfdisc arrays too small'
            print *,i,' > ',mwfdisc
            stop
        endif 
          read(idbwfdisc,2,end=999) sta_wfdisc(i), chan_wfdisc(i),
     *     time_wfdisc(i), wfid_wfdisc(i), chanid_wfdisc(i), 
     *     jdate_wfdisc(i),
     *     endtime_wfdisc(i), nsamp_wfdisc(i), samprate_wfdisc(i),
     *     calib_wfdisc(i), calper_wfdisc(i), instype_wfdisc(i),
     *     segtype_wfdisc(i), datatype_wfdisc(i), clip_wfdisc(i),
     *     dir_wfdisc(i), dfile_wfdisc(i), foff_wfdisc(i),
     *     commid_wfdisc(i), lddate_wfdisc(i)
    2   format(a6,1x,a8,1x,f17.5,1x,3(i8,1x),f17.5,1x,i8,1x,f11.7,1x,
     *        2(f16.6,1x),a6,1x,a1,1x,a2,1x,a1,1x,a64,1x,a32,1x,i10,
     *        1x,i8,1x,a17)
        i=i+1
        goto 1
 999    nrowwfdisc = i - 1
        close(idbwfdisc)
  99    if(nrowwfdisc.le.0) then
        write(*,*)
     *    'ERR033: fdb: wfdisc relation ',dbin(1:lnblnk(dbin)),
     *     ' appears to be empty, or does not exist'
        endif
	return
	end

c*********************************************************************
c   Write a wfdisc relation
c*********************************************************************
	subroutine write_wfdisc
	implicit none
        include 'fdb_wfdisc.h'
        include 'fdb_io.h'
c === Local variables ===
cxx	character*24 date
        integer*4 i, lnblnk

cxx	print *,'writing wfdisc relation.....'

        idbwfdisc=58
        open(idbwfdisc,file=dbout(1:lnblnk(dbout))//'.wfdisc',
     *       access='APPEND')
cxx	call fdate(date)
cxx     ddate_wfdisc(1)=date(5:10)//','//date(21:24)

	do i=1,nrowwfdisc
          write(idbwfdisc,2) sta_wfdisc(i), chan_wfdisc(i),
     *     time_wfdisc(i), wfid_wfdisc(i), chanid_wfdisc(i), 
     *     jdate_wfdisc(i),
     *     endtime_wfdisc(i), nsamp_wfdisc(i), samprate_wfdisc(i),
     *     calib_wfdisc(i), calper_wfdisc(i), instype_wfdisc(i),
     *     segtype_wfdisc(i), datatype_wfdisc(i), clip_wfdisc(i),
     *     dir_wfdisc(i), dfile_wfdisc(i), foff_wfdisc(i),
     *     commid_wfdisc(i), lddate_wfdisc(i)
    2   format(a6,1x,a8,1x,f17.5,1x,3(i8,1x),f17.5,1x,i8,1x,f11.7,1x,
     *         2(f16.6,1x),a6,1x,a1,1x,a2,1x,a1,1x,a64,1x,a32,1x,i10,
     *         1x,i8,1x,a17)
	enddo

        close(idbwfdisc)
	return	
	end

c***************************************************************
c select valid sitechan relatons, sort, and split into chn-sta groups
c***************************************************************
      subroutine select_sitechan(jdate,nchn,numchan,numsta)
      implicit none
      include 'fdb_site.h'
      include 'fdb_sitechan.h'
      include 'fdb_io.h'
      integer*4 jdate,nchn
      integer*4 numchan(msitechan),numsta(msite)
c --- local variables
      integer*4 i,i1,i2,iflag,ioff,ii,n,j
      character*6 stanew,staold
      nchn = 0
c create chanid for .sitechan
      do i = 1,nrowsitechan
         chanid_sitechan(i)=i
      enddo     
c select subset of chanid by ondate
      nchn = 0
      do i = 1,nrowsitechan
        if(ondate_sitechan(i).ne.-1) then
          ioff = offdate_sitechan(i)
          if(ioff.eq.-1) ioff = 3001001
          if(ondate_sitechan(i).le.jdate.and.jdate.le.ioff) then
            nchn = nchn+1
            numchan(nchn) = i
          else
            chanid_sitechan(i) = -1
          endif
        endif
      enddo     
      if(nchn.le.1) goto 9
c sort sitechan relation by sta and channel
      do i = nchn-1,1,-1
        do j = 1,i
           i1 = numchan(j)
           i2 = numchan(j+1)
           iflag = 0
           if(sta_sitechan(i2).lt.sta_sitechan(i1)) iflag = 1
           if(sta_sitechan(i2).eq.sta_sitechan(i1).and.
     *        chan_sitechan(i2).gt.chan_sitechan(i1)) iflag = 1
           if(iflag.eq.1) then
             ii = numchan(j)
             numchan(j) = numchan(j+1)
             numchan(j+1) = ii
           endif
        enddo
      enddo
c  remove duplicates from sitechan relation
      n = 1
      i1 = iabs(numchan(1))
      do i = 2,nchn
        i2 = iabs(numchan(i))
        if(sta_sitechan(i1).ne.sta_sitechan(i2).or.
     *     chan_sitechan(i1).ne.chan_sitechan(i2)) then
          if(sta_sitechan(i1).ne.sta_sitechan(i2).or.
     *       chan_sitechan(i1)(1:2).ne.chan_sitechan(i2)(1:2)) then
              numchan(n) = -numchan(n)
          endif
          n = n+1
          numchan(n) = i2
          i1 = iabs(numchan(n))
        endif
      enddo
      numchan(n) = -numchan(n)
      nchn = n
    9 continue
      if(nchn.le.1) goto 99
c remove sitechan for nonexistent stations
      staold = ' '
      n = 1
      do i = 1,nchn
        i1 = iabs(numchan(i))
        stanew = sta_sitechan(i1)
        if(stanew.ne.staold) then
          iflag = 0
          do j = 1,nrowsite
          if(stanew.eq.sta_site(j)) then
            if(ondate_site(j).ne.-1) then
              ioff = offdate_site(j)
              if(ioff.eq.-1) ioff = 3001001
              if(ondate_site(j).le.jdate.and.jdate.le.ioff) then
                iflag = j
                goto 8
              endif
            endif
          endif
          enddo
    8     continue
        endif
        if(iflag.ne.0) then
          numchan(n) = numchan(i)
          numsta(n) = iflag
          n = n+1
        endif
        staold = stanew
      enddo
      nchn = n-1
   99 continue
      return
      end

c***************************************************************
c output wfdisc data
c***************************************************************
      subroutine put_wfdisc(ituple,n,data,ierr)
      implicit none
      include 'fdb_wfdisc.h'
      include 'fdb_io.h'
      integer*4 ituple,n,ierr
      real*4    data(n)
cxx   real*4    data1(n)
      integer*4 i,i1,iwf,l,n1,nd,lnblnk,factor2
      character*256 named,cmd
cxx   character*2 endian
      logical tf
      
      ierr = 0
      idbwfdisc = 59
c check that data directory exists
      l = lnblnk(dbout)
      named = dbout(1:l)//'.wfdisc.dat'
      inquire(file=named,exist=tf)
      if(.not.tf) then
c create new dir for data
        cmd = 'mkdir -p '//named
        call system(cmd)
      endif
c select relative subdurectory
      l = lnblnk(named)
      do i = l,1,-1
        if(named(i:i).eq.'/'.or.named(i:i).eq.' ') then
          dir_wfdisc(ituple) = named(i+1:l)
          goto 1
        endif
      enddo
      dir_wfdisc(ituple) =  named(1:l)
c write data
    1 dbwfdisc = named(1:l)//'/'//dfile_wfdisc(ituple)
      n1 = factor2(n)
      nd = n1*4
cxx     if(datatype_wfdisc(ituple).ne.endian()) then
cxx       call swap(data,data1,4,n)
cxx     endif
      open(idbwfdisc,err=9,file=dbwfdisc,access='direct',recl=nd)
      iwf = 0
      do i = 1,n,n1
        iwf = iwf+1
        i1 = i+n1-1
        write(idbwfdisc,rec=iwf,err=9) (data(l),l=i,i1)
      enddo
      close(idbwfdisc)
      return
    9 ierr =1
      return
      end

c***************************************************************
c read wfdisc data
c***************************************************************
      subroutine get_wfdisc(ituple,n,data,ierr)
      implicit none
      include 'fdb_wfdisc.h'
      include 'fdb_io.h'
      integer*4 ituple,n,ierr
      real*4    data(n)
      integer*4 i,i1,iwf,l,ll,n1,nd,lnblnk,factor2
      character*256 dir,named
      character*2 endian
      logical tf
c--
      ierr = 0
      idbwfdisc = 59
c create name to file
      l = lnblnk(dbin)
      named = dbin(1:l)//'.wfdisc.dat'
c path to directory with .wfdisc relation
      l = lnblnk(named)
      do i = l,1,-1
        if(named(i:i).eq.'/'.or.named(i:i).eq.' ') then
          dir = named(1:i)
          goto 1
        endif
      enddo
      dir = ''
   1  ll = lnblnk(dir_wfdisc(ituple))
      if(lnblnk(dir).eq.0) then
        dbwfdisc = dir_wfdisc(ituple)(1:ll)//'/'//dfile_wfdisc(ituple)
      else
        dbwfdisc = dir(1:lnblnk(dir))//dir_wfdisc(ituple)(1:ll)//'/'//
     *             dfile_wfdisc(ituple)
      endif
      inquire(file=dbwfdisc,exist=tf)
      if(.not.tf) then
        l = lnblnk(dbwfdisc)
        write(*,*) 'ERR030: fdb: file ',dbwfdisc(1:l),' does not exist'
        goto 9
      endif
c read data
      n1 = factor2(n)
      nd = n1*4
      open(idbwfdisc,err=9,file=dbwfdisc,access='direct',recl=nd)
      iwf = 0
      do i = 1,n,n1
        iwf = iwf+1
        i1 = i+n1-1
        read(idbwfdisc,rec=iwf,err=9) (data(l),l=i,i1)
      enddo
      close(idbwfdisc)
        if(datatype_wfdisc(ituple).ne.endian()) then
          call swap1(data,4,n)
        endif
      return
    9 ierr =1
      return
      end

c*********************************************************************
c   sets default values for variables in common c_wfdisc, row = ituple
c*********************************************************************
	subroutine default_wfdisc(ituple)
	implicit none
        include 'fdb_wfdisc.h'
        integer*4 ituple
        character*2 endian
        character*17 loctime
c  a valid entry for sta is required under the CSS v. 3.0 standard
	sta_wfdisc(ituple) = '-'
c  a valid entry for chan is required under the CSS v. 3.0 standard
	chan_wfdisc(ituple) = '-'
c  a valid entry for wfid is required under the CSS v. 3.0 standard
	wfid_wfdisc(ituple) = 0
	time_wfdisc(ituple) = -9999999999.0
	endtime_wfdisc(ituple) = 9999999999.0
c  a valid entry for nsamp is required under the CSS v. 3.0 standard
	nsamp_wfdisc(ituple) = -1
c a valid entry for samprate is required under the CSS v. 3.0 standard
	samprate_wfdisc(ituple) = 0.0
c  a valid entry for calib is required under the CSS v. 3.0 standard
	calib_wfdisc(ituple) = 0.0
c  a valid entry for calper is required under the CSS v. 3.0 standard
	calper_wfdisc(ituple) = 0.0
	instype_wfdisc(ituple) = '-'
	segtype_wfdisc(ituple) = '-'
	datatype_wfdisc(ituple) = endian()
	clip_wfdisc(ituple) = '-'
c  a valid entry for dir is required under the CSS v. 3.0 standard
	dir_wfdisc(ituple) = '-'
c  a valid entry for dfile is required under the CSS v. 3.0 standard
	dfile_wfdisc(ituple) = '-'
c  a valid entry for foff is required under the CSS v. 3.0 standard
	foff_wfdisc(ituple) = 0
	commid_wfdisc(ituple) = -1
	lddate_wfdisc(ituple) = loctime()

	return
	end

c****************************************************************
c get number of factors 2 3 5 7 close to 2000
c****************************************************************
      integer*4 function factor2(n)
      implicit none
      integer*4 n
      integer*4 i,j,n7,nb,ne,icon(4)
      data icon/2,5,3,7/

      n7 = 0
      nb = n
      ne = 1
c investigate factor 7
      do i = 1,4
        if(mod(nb,7).eq.0) then
          n7 = n7+1
          nb = nb/7
          ne = ne*7
        endif
      enddo
      if(n7.ge.4) then
        factor2 = 7*7*7*7
        return
      endif
c investigate other factors
      do i =1,11
        do j = 1,4
           if(mod(nb,icon(j)).eq.0) then
             nb = nb/icon(j)
             ne = ne*icon(j)
             goto 2
           endif
        enddo
   2    continue
        if(ne.ge.1000) goto 1
      enddo
   1  factor2 = ne
      return
      end
