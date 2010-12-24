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
 * Header for cucss2sac utility.
 ***************************************************************************
 */
#ifndef MYSAC
#define MYSAC
/* Preselected filds from CSS relations which are needed for SAC header */
/* Part of .origin structure */
typedef struct ORIGIN {
    double time;
    int jdate;
    float lat;
    float lon;
    float depth;
} Origin;
/* Part of .site structure */
typedef struct SITE {
    char sta[8];
    int ondate;
    int offdate;
    float lat;
    float lon;
    float depth;
} Site;
/* Part of .wfdisc structure */
typedef struct WFDISC {
    char sta[8];
    char chan[8];
    double time;
    double endtime;
    int jdate;
    int nsamp;
    float samprate;
    int foff;
    char datatype[4];
    char dir[100];
    char file[100];
} Wfdisc;

/* The SAC header, binary version */
typedef struct sac {                    
float	delta,     depmin,    depmax,    scale,     odelta;    
float	b,         e,         o,         a,         internal1; 
float	t0,        t1,        t2,        t3,        t4;        
float	t5,        t6,        t7,        t8,        t9;        
float	f,         resp0,     resp1,     resp2,     resp3;     
float	resp4,     resp5,     resp6,     resp7,     resp8;     
float	resp9,     stla,      stlo,      stel,      stdp;      
float	evla,      evlo,      evel,      evdp,      unused1;   
float	user0,     user1,     user2,     user3,     user4;     
float	user5,     user6,     user7,     user8,     user9;     
float	dist,      az,        baz,       gcarc,     internal2; 
float	internal3, depmen,    cmpaz,     cmpinc,    unused2;   
float	unused3,   unused4,   unused5,   unused6,   unused7;   
float	unused8,   unused9,   unused10,  unused11,  unused12;  
int	nzyear,    nzjday,    nzhour,    nzmin,     nzsec;     
int	nzmsec,    internal4, internal5, internal6, npts;      
int	internal7, internal8, unused13,  unused14,  unused15;  
int	iftype,    idep,      iztype,    unused16,  iinst;     
int	istreg,    ievreg,    ievtyp,    iqual,     isynth;    
int	unused17,  unused18,  unused19,  unused20,  unused21;  
int	unused22,  unused23,  unused24,  unused25,  unused26;  
int	leven,     lpspol,    lovrok,    lcalda,    unused27;  
char	kstnm[8],  kevnm[16];           
char	khole[8],  ko[8],     ka[8];               
char	kt0[8],    kt1[8],    kt2[8];              
char	kt3[8],    kt4[8],    kt5[8];              
char	kt6[8],    kt7[8],    kt8[8];              
char	kt9[8],    kf[8],     kuser0[8];           
char	kuser1[8], kuser2[8], kcmpnm[8];           
char	knetwk[8], kdatrd[8], kinst[8];            
} SAC_HD;

/* Default values of the SAC header */
static SAC_HD sac_null = {
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345., -12345., -12345., -12345., -12345.,
-12345, -12345, -12345, -12345, -12345,
-12345, -12345, -12345, -12345, -12345,
-12345, -12345, -12345, -12345, -12345,
-12345, -12345, -12345, -12345, -12345,
-12345, -12345, -12345, -12345, -12345,
-12345, -12345, -12345, -12345, -12345,
-12345, -12345, -12345, -12345, -12345,
-12345, -12345, -12345, -12345, -12345,
{ '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }, { '-','1','2','3','4','5',' ',' ' },
{ '-','1','2','3','4','5',' ',' ' }
};

/* defines for logical data types */
#define TRUE    1
#define FALSE   0

/* defines for enumerated data types */
#define IREAL   0 
#define ITIME   1 
#define IRLIM   2 
#define IAMPH   3 
#define IXY     4 
#define IUNKN   5 
#define IDISP   6 
#define IVEL    7 
#define IACC    8 
#define IB      9 
#define IDAY   10 
#define IO     11 
#define IA     12 
#define IT0    13 
#define IT1    14 
#define IT2    15 
#define IT3    16 
#define IT4    17 
#define IT5    18 
#define IT6    19 
#define IT7    20 
#define IT8    21 
#define IT9    22 
#define IRADNV 23 
#define ITANNV 24 
#define IRADEV 25 
#define ITANEV 26 
#define INORTH 27 
#define IEAST  28 
#define IHORZA 29 
#define IDOWN  30 
#define IUP    31 
#define ILLLBB 32 
#define IWWSN1 33 
#define IWWSN2 34 
#define IHGLP  35 
#define ISRO   36 
#define INUCL  37 
#define IPREN  38 
#define IPOSTN 39 
#define IQUAKE 40 
#define IPREQ  41 
#define IPOSTQ 42 
#define ICHEM  43 
#define IOTHER 44 
#define IGOOD  45 
#define IGLCH  46 
#define IDROP  47 
#define ILOWSN 48 
#define IRLDTA 49 
#define IVOLTS 50 
#define INIV51 51 
#define INIV52 52 
#define INIV53 53 
#define INIV54 54 
#define INIV55 55 
#define INIV56 56 
#define INIV57 57 
#define INIV58 58 
#define INIV59 59 
#define INIV60 60 

#define FCS "%15.7f%15.7f%15.7f%15.7f%15.7f\n"
#define ICS "%10d%10d%10d%10d%10d\n"
#define CCS1 "%-8.8s%-8.8s%-8.8s\n"
#define CCS2 "%-8.8s%-16.16s\n"

/* Finction prototypes		*/
SAC_HD *read_sac (char *fname, float *sig, SAC_HD *SHD, int nmax);
void write_sac (char *fname, float *sig, SAC_HD *SHD);
void write_asc (char *fname, int ihed, char *dtype, double wtime, double otime,float *sig, SAC_HD *SHD);
void e2h(double t, int *year,int *doy,int *hour,int *miin,double *sec);
void endian(char *cind);
void swapn(unsigned char *b, int N, int n);
#endif
