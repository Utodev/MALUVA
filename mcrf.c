/*                     
Create a run file for the Amstrad CPC system. This combines the required
interpreter with a given database and graphics database (which has a CPC
header), adds a CPC header.
*/

#define VERSION 01
#define FILEV 02

#include <stdio.h>
#include <string.h>

#define TRUE 1
#define FALSE 0

#define PATHLEN 64
#define IPSSIZE 128
#define DBADD 0x2880
#define INTAT 0x0840
#define SPARE 0x20L

typedef unsigned int WORD;
typedef char BYTE;
typedef char *charptr; 

extern FILE *openfp();
extern char *mname();
extern char *strupper();

char fn[9],dfp[PATHLEN+1],fe[4],wfn[PATHLEN+13];
char ofn[9],ofp[PATHLEN+1],ofe[4];
FILE *fpi,*fpo;

struct {
BYTE null1;
BYTE name[11];
WORD null2[3];
BYTE two;
WORD null3;
WORD start;
BYTE null4;
WORD len1;
WORD stadd;
BYTE null6[36];
WORD len2;
BYTE null7;
WORD csum;
BYTE null8[59];
       } cpchead;

#define SF_OK 0
#define SF_WILD 1
#define SF_BADPATH 2
#define SF_NONAME 3
#define SF_BADEXT 4

char *sf_text[]={"OK","Wildcard files","Invalid Path","No file name",
 "Invalid Extension"};
int optlist[1][2]={-1,0};
char *filep[4]={NULL,NULL,NULL,NULL};
int countfile=0;

int main(argc,argv)
int argc;
char *argv[];
{
  char argv_buffer[MAXARGS*2+132];

  int fd,c,fflag;
  unsigned gdbl,length,word,ail;

  cpm_cmd_line(&argc,&argv,argv_buffer);

  printf("Make CPC run file on CPM. V%d.%02d\n",VERSION,FILEV);
  printf("(c) 1989 Infinite Imaginations.\n");
  printf("Written by T.J.Gilberts using Hisoft C.\n");

  if(argc>1) {
    setopts(argv,argc);

    if(countfile<4){
      printf("Insufficient files specified.\n");
      exit(0);
    }


    if((fflag=splitfn(dfp,fn,fe,filep[0]))>SF_OK){
      printf("Error: %s.\n",sf_text[fflag]);
      exit(0);
    }
    if((fpo=fopen(mname(dfp,fn,fe,"BIN"),"wb"))==NULL){
      printf("Unable to open output file.\n");
      exit(0);
    }
    printf("'%s.%s' open for output.\n",fn,(fe[0]=='\0'?"BIN":fe));

    if((fpi=openfp(1,"rb","Z80"))==NULL){
      fclose(fpo);
      exit(0);
    }
    printf("interpreter ");
    fread(&cpchead,sizeof(BYTE),128,fpi); /* Get CPC header */
    fwrite(&cpchead,sizeof(BYTE),128,fpo); /* Make dummy CPC header */
    ail=cpchead.len1;
    printf("%u (0x%04X) bytes.\n",ail,ail);
    for(c=ail;c!=0;c--)
      putc(getc(fpi),fpo);
    fclose(fpi);

    if((DBADD-INTAT)>ail){
      printf("Padding to db position using %u bytes.\n",DBADD-INTAT-ail);
      for(c=0;c<DBADD-INTAT-ail;c++)
putc(0,fpo);
    }

    if((fpi=openfp(2,"rb","BIN"))==NULL){
      fclose(fpo);
      exit(0);
    }
    printf("text database.\n");
    if(ofe[0]=='\0'||(strncmp(ofe,"BIN",3)==0)){      /* BIN, length from hdr */
      printf("Length from CPC header ");
      fread(&cpchead,sizeof(BYTE),128,fpi);                 /* Get CPC header */
      ail=cpchead.len1;  /* Find exact length of data */
    }else{
      printf("Length from spare ");
      fseek(fpi,SPARE,0); /* Get length from SPARE */
      ail=getw(fpi)-DBADD;
      fseek(fpi,0L,0); /* Rewind file */
    }
    printf("%u (0x%04X) bytes.\n",ail,ail);
    for(c=ail;c!=0;c--)
      putc(getc(fpi),fpo);
    fclose(fpi);

    length=DBADD-INTAT+ail;

    if((fpi=openfp(3,"rb","BIN"))==NULL){
      fclose(fpo);
      exit(0);
    }
    printf("graphic database ");
    fread(&cpchead,sizeof(BYTE),128,fpi); /* Get CPC header */
    gdbl=cpchead.len1; /* Find exact length of data */
    printf("%u (0x%04X) bytes.\n",gdbl,gdbl);
    for(c=gdbl;c!=0;c--)
      putc(getc(fpi),fpo);
    fclose(fpi);

    length+=gdbl;

    printf("Creating valid CPC header block.\n");
    fseek(fpo,0L,0); /* Rewind file to overwrite CPC interp header */
    sethead(INTAT,length,fn,fe,INTAT);
    fwrite(&cpchead,sizeof(BYTE),128,fpo); /* Make CPC header valid */
    fseek(fpo,3L,1); /* Move on to patch length of graphic data */
    putw(gdbl,fpo);
    fclose(fpo);

    puts("Complete. Files closed.\n");

  } else {

  printf("Error: Invalid command line.\n");
  printf("Usage: MCRF outfile{.BIN} interp{.Z80} tdb{.BIN} gdb{.BIN}\n");
  }
}

void setopts(argv,argc)
char *argv[];
int argc;
{
  int z,p,a;
  for(z=1;z!=argc;z++)
    if(argv[z][0]=='-'){
      p=1;
      while(argv[z][p]!='\0'){
a=0;
while(optlist[a][0]!=-1&&optlist[a][0]!=argv[z][p]) a++;
if(optlist[a][0]==-1){
  printf("Invalid command option '%c' ignored.\n",argv[z][p]);
  break;
} else optlist[a][1]=TRUE;
      p++;
      }
    } else
      if(countfile<4) filep[countfile++]=argv[z];
      else if (countfile++==4) printf("Too many files, extra ignored.\n");
}


sethead(loada,length,fnam,ext,runa)
WORD loada,length,runa;
char *fnam,*ext;
{
  int j;
  char *exu;

  exu=(ext[0]=='\0')?fe:ext;
  for(j=0;j<128;j++)
    *(cast(charptr)&cpchead+j)=0; /* Clear header */
  cpchead.two=2;
  cpchead.start=loada;
  cpchead.stadd=runa;
  cpchead.len1=length;
  cpchead.len2=length;
  strncpy(cpchead.name,"           ",11);
  strncpy(cpchead.name,fnam,strlen(fnam));
  strncpy(&cpchead.name[8],exu,strlen(exu));
  strupper(cpchead.name); /* Null words following allow a C string */
  for(j=0;j<67;j++)
    cpchead.csum+=*(cast(charptr)&cpchead+j);
}


FILE *openfp(fnum,opt,dext)
char *opt,*dext;
int fnum;
{
  FILE *fp;
  char *wfn;
  int fflag;

  if((fflag=splitfn(ofp,ofn,ofe,filep[fnum]))>SF_OK){
    printf("Error: %s.\n",sf_text[fflag]);
    return(NULL);
  }
  fp=fopen((wfn=mname(ofp,ofn,ofe,dext)),opt);
  if(fp==0) {
    printf("Can't open file '%s'.\n",wfn);
    return(NULL);
  }
  printf("File %s.%s open - ",ofn,(ofe[0]=='\0'?dext:ofe));
  return(fp);
}


char *mname(path,name,ext,dext) /* filename with given name or defaults */
char *path,*name,*ext,*dext;
{
  static char wfn[PATHLEN+13];

  strcpy(wfn,(path[0]=='\0')?dfp:path);
  strcat(wfn,name);
  strcat(wfn,".");
  strcat(wfn,(ext[0]=='\0')?dext:ext);
  return(wfn);
}


splitfn(path,name,ext,buf)   /* Extracts parts of filename */
char *path,*name,*ext,*buf;
{
  char *pp,*sp,*lp;
  int done,retval,relpath,z,c,dots;

  dots=0;
  retval=SF_OK;
  relpath=TRUE;
  done=FALSE;
  path[0]='\0';
  name[0]='\0';
  ext[0]='\0';
  pp=path;
  sp=buf;
  if(sp[1]==':'){
    *pp++=*sp++;
    *pp++=*sp++;
  }
  lp=sp;
  do{
    switch(*lp++){
    case '\\': /* Found a bit more of path description */
      while(sp!=lp){
c=(*pp++=*sp++);
if(c=='?'||c=='*') retval=SF_BADPATH; /* Can't have wild directories */
      }
      relpath=TRUE;
      dots=0;
      break;
    case '.':
      if(relpath){
*pp++=*sp++; /* Copy dot into path */
if(dots++>1) retval=SF_BADPATH; /* Max of two dots after each other */
break;
      }else{
        for(z=0;z<3;z++)
  if(lp[z]!='\0'&&lp[z]!='.'&&lp[z]!='\\')
            ext[z]=lp[z];
  else
            break;
if (lp[z]!='\0') retval=SF_BADEXT;
        ext[z]='\0';
      }
    case '\0':
      for(z=0;z<8;z++)
if(sp[z]!='.'&&sp[z]!='\0') {
  c=(name[z]=sp[z]);
  if(c=='?'||c=='*') retval=SF_WILD;
} else
  break;
      name[z]='\0';
      if(z==0) retval=SF_NONAME;
      done=TRUE;
      break;
    default:
      if(dots!=0) retval=SF_BADPATH; /* can't have path or name after dots */
      relpath=FALSE;
      break;
    }
  }while(!done);
  *pp='\0';
  return(retval);
}
