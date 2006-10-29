c********************************************************************
c  Header: fdb_site.h
c  site declarations (3.0)
c  (R) University of Colorado at Boulder,CIEI.      Date: 02/24/2004
c********************************************************************
        integer*4 msite, nrowsite
	parameter (msite = 10000)
	character*4 statype_site 
	character*6 sta_site, refsta_site
	character*17 lddate_site 
	character*50 staname_site
	integer*4 ondate_site, offdate_site	
	real*4    lat_site,lon_site,elev_site, deast_site, dnorth_site
	common/c_site/sta_site(msite), ondate_site(msite), 
     *        offdate_site(msite),lat_site(msite), lon_site(msite), 
     *        elev_site(msite), staname_site(msite), 
     *        statype_site(msite), refsta_site(msite), 
     *        dnorth_site(msite), deast_site(msite), 
     *        lddate_site(msite), nrowsite
