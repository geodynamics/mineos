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
c simpledit converts ascii cintrol file to .site and .siechan relations
c
c**************************************************************************
      include "../fdb/fdb_site.h"
      include "../fdb/fdb_sitechan.h"
      include "../fdb/fdb_io.h"
      character*180 str
      character*256 name,dbname
      character*1  stax
      character*6  sta
      character*8  chan
      character*50 staname

c ---
      narg = iargc()
      if(narg .ne.2) stop 'Usage: simpledit ascii_file db_name'
      call getarg(1,name)
      call getarg(2,dbname)
      nrowsite = 0
      nrowsitechan = 0
      dbout = dbname  
      open(7,file=name,status='old')
   1  str = ' '
      read(7,1000,end=99,err=99) str
      read(str,1001) stax
      if(stax.eq.'@') goto 5
c station name
      nrowsite = nrowsite+1
      call default_site(nrowsite)
      read(str,*) sta,slat,slon,elev,staname
      sta_site(nrowsite) = sta
      lat_site(nrowsite) = slat
      lon_site(nrowsite) = slon
      elev_site(nrowsite) = elev
      staname_site(nrowsite) = staname
      ondate_site(nrowsite) = 1960001
      offdate_site(nrowsite) = -1
      goto 1
c channel name
    5 read(str,*) stax,chan,cdepth,hang,vang
      nrowsitechan = nrowsitechan+1
      call default_sitechan(nrowsitechan)
      sta_sitechan(nrowsitechan) = sta
      chan_sitechan(nrowsitechan) = chan
      edepth_sitechan(nrowsitechan) = cdepth
      hang_sitechan(nrowsitechan) = hang
      vang_sitechan(nrowsitechan) = vang
      ondate_sitechan(nrowsitechan) = 1960001
      offdate_sitechan(nrowsitechan) = -1
      ctype_sitechan(nrowsitechan) = 'n'
      goto 1
  99  call write_site
      call write_sitechan
      close(7)
 1000 format(a180)
 1001 format(a)
      end
