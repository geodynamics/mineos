## Process this file with automake to produce Makefile.in
##
## MINEOS by Guy Masters, John Woodhouse, and Freeman Gilbert
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##

bin_PROGRAMS = minos_bran syndat green eigcon \
	endi eigen2asc simpledit cucss2sac

# scripts

dist_bin_SCRIPTS = scripts/creat_origin

# demo

dist_pkgdata_DATA = \
	doc/mineos.pdf \
	DEMO/DEMO.tar.gz \
	DEMO/README

# main programs

minos_bran_SOURCES = minos_bran.f
syndat_SOURCES = syndat.f $(fdb) $(time) fftsubs.f green.h
green_SOURCES = green.f $(fdb) $(time) fftsubs.f green.h
eigcon_SOURCES = eigcon.f $(fdb) $(time)

# FDB utils

fdb = \
	fdb/fdb_eigen.f \
	fdb/fdbs.f \
	fdb/fdb_io.h \
	fdb/fdb_site.h \
	fdb/fdb_sitechan.h \
	fdb/fdb_wfdisc.h \
	fdb/fdb_eigen.h \
	fdb/swapn.c

# time utils
time = \
	time/time.f

# Utilities
endi_SOURCES = utils/endi.c utils/swap.c
simpledit_SOURCES = utils/simpledit.f $(fdb) $(time)
cucss2sac_SOURCES = utils/cucss2sac.c utils/swap.c utils/cucss2sac.h
eigen2asc_SOURCES = utils/eigen2asc.f $(fdb) $(time)

## end of Makefile.am
