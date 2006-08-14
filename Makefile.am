## Process this file with automake to produce Makefile.in
##
## SynDat by Guy Masters, John Woodhouse, and Freeman Gilbert
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

# $Id: Makefile.am 2452 2005-10-19 20:03:01Z leif $

bin_PROGRAMS = minos_bran syndat green eigcon \
	ghead glist gtail iutil \
	curse

# main programs
minos_bran_SOURCES = minos_bran.f
syndat_SOURCES = syndat.f $(gfs) fftsubs.f
green_SOURCES = green.f $(gfs) fftsubs.f rcode.f
eigcon_SOURCES = eigcon.f $(gfs)

# GFS utils
ghead_SOURCES = gfs/ghead.f $(gfs)
glist_SOURCES = gfs/glist.f $(gfs)
gtail_SOURCES = gfs/gtail.f $(gfs)
iutil_SOURCES = gfs/iutil.f $(gfs)

# curse
curse_SOURCES = gfs/curse.f $(gfs) $(leo)
curse_LDADD = -L/usr/X11R6/lib -lX11

gfs = \
	gfs/disk2.c \
	gfs/gfs.f \
	gfs/usr.f

leo = \
	gfs/leolib.c \
	gfs/psscreendump.f \
	gfs/scr1.f

# included by leolib.c
leoinc = \
	gfs/ftoc.c \
	gfs/leodraw.c \
	gfs/leoinp.c \
	gfs/leotext.c \
	$(xvertext)

# xvertext 5.0 by Alan Richardson
xvertext = \
	gfs/rotated.c \
	gfs/rotated.h

EXTRA_DIST = \
	LIST.STA \
	locale.cmt.big \
	stations \
	gfs/gfs.doc \
	$(leoinc)

## end of Makefile.am
