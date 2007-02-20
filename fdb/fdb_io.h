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
c  Header: fdb_io.h
c  Data base io declarations (3.0)
c
c**************************************************************************
	character*256 dbin, dbout, dbcmt, dbegn, dbwfdisc
	integer*4    idbin, idbout, idbcmt, idbegn, idbwfdisc
	common/c_dbio/idbin,idbout,dbin,dbout,dbcmt,idbcmt,dbegn,
     *                idbegn,dbwfdisc, idbwfdisc
