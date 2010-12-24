/*
 * MINEOS version 1.0 by Guy Masters, John Woodhouse, and Freeman Gilbert
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ***************************************************************************
 * CSS to SAC converter. Works only for t4 and f4 formats
 ***************************************************************************
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <math.h>
#include "cucss2sac.h"

int main(int argc, char *argv[])
{
DIR *pdir;				/* i/o file descriptors	*/
FILE *in,*wfd_fid, *ori_fid, *sit_fid;
SAC_HD sach;				/* SAC header		*/
Origin ori;				/* .origin relation	*/
Site sit[3000];				/* .site array		*/
Wfdisc wfd;				/* current .wfdisc	*/
float *sig;				/* binary data		*/
char dname[256],fname[256],wfd_name[256],sit_name[256],ori_name[256];
char character[10];
char buf[500];
char cmd[100];
char s1[10],s2[10],s3[10];
char endi[3];
int nzyear,nzjday,nzhour,nzmin;
int i,j,z1,z2,nargc,iasc,ihed,nn=0,n=0;
float calib,calper;
double sec;

/* check command parameters */
nargc = 3;
iasc = 0;
ihed = 0;
for(i = 1; i < argc; i++) {
    if(strncmp(argv[i],"-a",2) == 0) {nargc++; iasc = 1;}
    if(strncmp(argv[i],"-n",2) == 0) {nargc++; ihed = 1;}
}
if(argc != nargc) {
    printf("Usage: cucss2sac [-a [-n]] db_name out_SAC_dir\n");
    printf("       where,\n");
    printf("       -a - convert CSS waveforms to ASCII format,\n");
    printf("       -n - supress ASCII header output, uses only with option -a,\n");
    printf("       db_name - CSS database, must include .wfdisc relation.\n");
    printf("                 .origin and .site relations are desirable \n");
    printf("                 to improve quality of SAC header,\n");
    printf("       out_SAC_dir - path to directory with binary SAC or ASCII output files\n");

    exit(-1);
}
/* check that .wfdisc relation exists */
strcpy(wfd_name,argv[nargc-2]);
strcat(wfd_name,".wfdisc");
if((wfd_fid = fopen(wfd_name,"r")) == NULL) {
     printf("Relation %s does not exist.\n",wfd_name);
     exit(-1);
}
/* check that .origin relation exists, and read the first line */
strcpy(ori_name,argv[nargc-2]);
strcat(ori_name,".origin");
if((ori_fid = fopen(ori_name,"r")) == NULL) {
     printf("Relation %s does not exist. Event info is not included in SAC header.\n",ori_name);
} else {
fscanf(ori_fid,"%f %f %f %lf %d %d %d",&ori.lat,&ori.lon,&ori.depth,&ori.time,
    &z1,&z2,&ori.jdate);
fclose(ori_fid);
}
/* check that .site relation exists, and read the station list */
strcpy(sit_name,argv[nargc-2]);
strcat(sit_name,".site");
if((sit_fid = fopen(sit_name,"r")) == NULL) {
     printf("Relation %s does not exist. Stations info is not included in SAC header.\n",sit_name);
} else {
    n = 0;
    while(fgets(buf,400,sit_fid) != NULL) {
        nn = sscanf(buf,"%s %d %d %f %f %f",sit[n].sta,&sit[n].ondate,
            &sit[n].offdate, &sit[n].lat,&sit[n].lon,&sit[n].depth);
        if(nn != 6) continue;
        n++;
    }
fclose(sit_fid);
}
strcpy(dname,argv[nargc-1]); 
/* check that output dir exsists                  */
if((pdir = opendir(dname)) == NULL) {
    sprintf(cmd,"mkdir -p %s",dname);
    system(cmd);
} else {
    closedir(pdir);
}
/* 
 *                 MAIN LOOP by .wfdisc
 */
i = 0;
while(fgets(buf,400,wfd_fid) != NULL) {
    sscanf(buf,"%s %s %lf %d %d %d %lf %d %f %f %f %s %s %s %s %s %s %d",
        wfd.sta,wfd.chan,&wfd.time,&z1,&z2,&wfd.jdate,&wfd.endtime,
        &wfd.nsamp,&wfd.samprate,&calib,&calper,s1,s2,wfd.datatype,
        s3,wfd.dir,wfd.file,&wfd.foff);
    if((strcmp(wfd.datatype,"f4") != 0) & (strcmp(wfd.datatype,"t4") != 0)) {
        printf("Format %s does not supported.\n",wfd.datatype);
        continue;
    }
    strcpy(fname,wfd.dir);
    strcat(fname,"/");
    strcat(fname,wfd.file);
    sach = sac_null;
    sach.npts = wfd.nsamp;
    sach.delta = 1.0/wfd.samprate;
    sach.b = 0.0;
    sach.e = sach.b+sach.delta*(sach.npts-1);

    sprintf (character, "%-8.8s", wfd.sta);
    strncpy (sach.kstnm, character, 8);
    sprintf (character, "%-8.8s", wfd.chan);
    strncpy (sach.kcmpnm, character, 8);
    sprintf (character, "%-8.8s", "NA");
    strncpy(sach.knetwk, character, 8);
/* station info   */
    if(sit_fid != NULL) {
        for(i=0; i < n; i++) {
            if(strcmp(sit[i].sta, wfd.sta) == 0) {
                sach.stla = sit[i].lat;
                sach.stlo = sit[i].lon;
                sach.stdp = 0.0;
            }
        }
    } else {
        sach.stla = 0.0;
        sach.stlo = 0.0;
        sach.stdp = 0.0;
    }
/* event info    */
    if(ori_fid != NULL) {
        sach.evla = ori.lat;
        sach.evlo = ori.lon;
        sach.evdp = ori.depth*1000;
    } else {
        sach.evla = 0.0;
        sach.evlo = 0.0;
        sach.evdp = 0.0;
        ori.time = wfd.time;
    }
        e2h(wfd.time,&nzyear,&nzjday,&nzhour,&nzmin,&sec);
        sach.nzyear = nzyear;
        sach.nzjday = nzjday;
        sach.nzhour = nzhour;
        sach.nzmin  = nzmin;
        j = sec;
        sach.nzsec  = j; 
        j = (sec-j)*1000+0.5;
        sach.nzmsec = j;
        sach.o = ori.time-wfd.time;
/* read binary part of CSS file		*/
if((sig = (float *)malloc(wfd.nsamp*4)) == NULL) {
    printf("Can not alloc memory for data file\n");
    exit(-1);
}
if((in = fopen(fname,"r")) == NULL) {
   printf("Can not open file %s\n",fname);
   continue;
}
fread(sig,sizeof(float),wfd.nsamp,in);
fclose(in);
/* create output SAC name and write SAC file   */
strcpy(fname,dname);
strcat(fname,"/");
/* check bytes order */
endian(endi);
if(strcmp(wfd.datatype,endi) != 0) swapn((unsigned char *)sig,4,wfd.nsamp);
/* Output data into file  */
if(iasc) {
    sprintf(cmd,"%s.%7d:%2d:%2d:%2d.%s.%s.ASC",argv[nargc-2],wfd.jdate,
        nzhour,nzmin,(int)sach.nzsec,wfd.sta,wfd.chan);
    strcat(fname,cmd);
    write_asc (fname, ihed, s2, wfd.time,ori.time, sig, &sach); /* Write ASCII file  */
} else {
    sprintf(cmd,"%s.%7d:%2d:%2d:%2d.%s.%s.SAC",argv[nargc-2],wfd.jdate,
        nzhour,nzmin,(int)sach.nzsec,wfd.sta,wfd.chan);
    strcat(fname,cmd);
    write_sac (fname, sig, &sach); /* Write SAC file  */
}
free(sig);
}
 return 0;
}

/**************************************************************************
 * Write SAC file
 ************************************************************************* */
void write_sac (char *fname, float *sig, SAC_HD *SHD)
{
  FILE *fsac;
  int i;
  if((fsac = fopen(fname, "wb"))==NULL) {
    fprintf(stderr,"write_sac: Could not open %s to write\n", fname);
    exit(-1);
  }

    SHD->iftype = (int)ITIME;
    SHD->leven = (int)TRUE;
    SHD->lovrok = (int)TRUE;
    SHD->internal4 = 6L;
    SHD->iztype = IB;
/* mysterious LLL-set values */
                SHD->internal1 = 2.0;
                SHD->internal4 = 6;
                SHD->internal5 = 0;
                SHD->internal6 = 0;
                SHD->unused27 = FALSE;

/* not sure if these are needed, but they might be. Values might be wrong*/
                SHD->lpspol = FALSE;
                SHD->lcalda = TRUE;
    SHD->depmin = sig[0];
    SHD->depmax = sig[0];

    for ( i = 0; i < SHD->npts ; i++ ) {
      if ( SHD->depmin > sig[i] ) {
        SHD->depmin = sig[i];
      }
      if ( SHD->depmax < sig[i] ) {
        SHD->depmax = sig[i];
      }
    }

    fwrite(SHD,sizeof(SAC_HD),1,fsac);
    fwrite(sig,sizeof(float),(int)(SHD->npts),fsac);

    fclose (fsac);
}
/**************************************************************************
 * Write ASCII file
 ************************************************************************* */
void write_asc (char *fname, int ihed, char *dtype, double wtime, 
                double otime, float *sig, SAC_HD *SHD)
{
  FILE *fsac;
  long i,n;
  int year,jday,hour,nmin;
  double sec;
  if((fsac = fopen(fname, "wb"))==NULL) {
    fprintf(stderr,"write_sac: Could not open %s to write\n", fname);
    exit(-1);
  }
  if(ihed == 0) { /* output ascii header */
      e2h(otime,&year,&jday,&hour,&nmin,&sec);
      fprintf(fsac,
          "EVENT:   %04d(%03d)-%02d:%02d:%06.3lf  %17.5lf  %9.4f %9.4f %9.4f\n",
          year,jday,hour,nmin,sec,otime,SHD->evla,SHD->evlo,SHD->evdp/1000.0);
      SHD->kstnm[7] = 0;
      SHD->kcmpnm[7] = 0;
      fprintf(fsac,
          "STATION: %-7.7s %-7.7s %9.4f %9.4f %9.4f\n",SHD->kstnm,SHD->kcmpnm,
          SHD->stla, SHD->stlo, SHD->stdp);
      fprintf(fsac,
          "DATA:    %04ld(%03ld)-%02ld:%02ld:%02ld.%03ld  %17.5lf  %ld %f\n",
          SHD->nzyear,SHD->nzjday,SHD->nzhour,SHD->nzmin,SHD->nzsec,
          SHD->nzmsec,wtime,SHD->npts,SHD->delta);
  }
  if(dtype[0] == 'w')
      for(i = 0; i < SHD->npts; i++)
          fprintf(fsac,"%15.7E %15.7E\n",SHD->delta*i,sig[i]);
  if(dtype[0] == 'g') {
      n = SHD->npts/6;
      for(i = 0; i < n; i++)
          fprintf(fsac,"%15.7E %15.7E %15.7E %15.7E %15.7E %15.7E %15.7E \n",
              SHD->delta*i,sig[i],sig[i+n],sig[i+2*n],sig[i+3*n],
              sig[i+4*n],sig[i+5*n]);
  }
  fclose(fsac);
}
/************************************************************************
 * Convert epoch time to human
 ************************************************************************ */
void e2h(double t, int *year,int *doy,int *hour,int *miin,double *sec)
{
double fsec;
int idate, itime, irest,iysupp,idsupp;
/*    date parts			*/
      idate = t/86400.0;
      iysupp = idate/365;
      idsupp = iysupp*365+(iysupp+1)/4;
      if(idate < idsupp) iysupp = iysupp-1;
      idsupp = iysupp*365+(iysupp+1)/4;
/*   extract year			*/
      *year = 1970+iysupp;
/*   extract doy			*/
      *doy = idate - idsupp+1;
/*   time part				*/
      fsec = t-idate*86400.0;
      irest = fsec;
      fsec = fsec - irest;
      itime = irest;
      irest = itime/60;
/*   extract seconds			*/
      *sec = itime - irest*60 + fsec;
      itime = irest;
      irest = itime/60;
/*   extract mininutes			*/
      *miin = itime - irest*60;
/*   extract hours			*/
      *hour = irest;
}
/**********************************************************
 * Check CPU endian order: t4 - BIG, f4 - LOW
 ********************************************************** */
void endian(char *cind)
{
char ch[4];
int *ich;
ich = (int *)&ch[0];
*ich = 0x00000000;
ch[3] =0x02;
strcpy(cind,"f4");      /* LOW */
if(*ich == 2) strcpy(cind,"t4"); /* BIG */
}
