/*#ident mc.c from deck mc */                                                   /*!mc          1*/
#include <stdio.h>                                                              /*!mc4c        1*/
#if defined(UNIX) || defined(UNICOS)                                            /*!mc4c        2*/
#include <unistd.h>                                                             /*!mc4c        3*/
#endif                                                                          /*!cpp         1*/
#ifndef UNIX                                                                    /*!mc4c        4*/
#if defined(DEC) || defined(PCDOS)                                              /*!mc4c        5*/
#include <stdlib.h>                                                             /*!mc4c        6*/
#endif                                                                          /*!cpp         3*/
#endif                                                                          /*!cpp         2*/
#if defined(UNIX) || defined(UNICOS)                                            /*!mc4c2       1*/
#include <time.h>                                                               /*!mc4c2       2*/
#include <sys/stat.h>                                                           /*!mc4b        5*/
#endif                                                                          /*!cpp         4*/
#ifdef UNICOS                                                                   /*!mc4c2       3*/
#include <fortran.h>                                                            /*!mc4b        6*/
#endif                                                                          /*!cpp         5*/
#ifdef CHEAP                                                                    /*!mc4c        8*/
#define IDEF int                                                                /*!mc4c        9*/
#define FDEF float                                                              /*!mc4c       10*/
#endif                                                                          /*!cpp         6*/
#ifndef CHEAP                                                                   /*!mc4c       11*/
#define IDEF long                                                               /*!mc4c       12*/
#define FDEF double                                                             /*!mc4c       13*/
#endif                                                                          /*!cpp         7*/
#ifndef UNICOS                                                                  /*!mc4c       14*/
#define CDEF char                                                               /*!mc4c       15*/
#endif                                                                          /*!cpp         8*/
#ifdef UNICOS                                                                   /*!mc4c       16*/
#define CDEF _fcd                                                               /*!mc4c       17*/
#endif                                                                          /*!cpp         9*/
#ifdef XLIB                                                                     /*!mc          6*/
/*                                                                    */        /*!mc          7*/
/* Enable plotting with the XLIB window system.                       */        /*!mc          8*/
/*                                                                    */        /*!mc          9*/
/* Get the XLIB include files.                                        */        /*!mc         10*/
#include <X11/Xlib.h>                                                           /*!mc         14*/
#include <X11/Xutil.h>                                                          /*!mc         15*/
#include <X11/Xos.h>                                                            /*!mc         16*/
#include <X11/Xatom.h>                                                          /*!mc         17*/
#include <X11/cursorfont.h>                                                     /*!mc         18*/
#include <X11/keysym.h>                                                         /*!mc4c2       4*/
#include <sys/types.h>                                                          /*!mc4c       18*/
#define NCOLOR 64                                                               /*!mcxk        1*/
/* Define the icon bitmap - created by "bitmap filename 40x40".       */        /*!mc         19*/
#define icon_bitmap_width 40                                                    /*!mc         20*/
#define icon_bitmap_height 40                                                   /*!mc         21*/
#define MAX_BUF 100                                                             /*!mc4c2       5*/
static char icon_bitmap_bits[] = {                                              /*!mc         22*/
 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x07, 0xe0, 0x0f, 0x38, 0x00, 0x00,        /*!mc         23*/
 0x1c, 0x70, 0x20, 0xfe, 0x07, 0x43, 0x80, 0x31, 0x00, 0x80, 0x40, 0x00,        /*!mc         24*/
 0x22, 0xfe, 0x47, 0x40, 0x00, 0x3c, 0x00, 0x20, 0x40, 0xe0, 0x08, 0xfe,        /*!mc         25*/
 0x17, 0x7c, 0x80, 0x10, 0x00, 0x10, 0x83, 0x41, 0x10, 0x00, 0x88, 0x00,        /*!mc         26*/
 0x22, 0x20, 0x00, 0x88, 0x00, 0xe2, 0x20, 0x00, 0x48, 0x00, 0x7c, 0x20,        /*!mc         27*/
 0x00, 0x44, 0x00, 0x87, 0x41, 0x00, 0x44, 0x80, 0x04, 0x42, 0x00, 0x44,        /*!mc         28*/
 0x80, 0x04, 0x42, 0x00, 0x44, 0x40, 0x04, 0x44, 0x00, 0x84, 0x40, 0x02,        /*!mc         29*/
 0x44, 0x00, 0x84, 0x40, 0x02, 0x44, 0x00, 0x04, 0xc3, 0x01, 0x44, 0x00,        /*!mc         30*/
 0x08, 0x7c, 0x00, 0x24, 0x00, 0x08, 0x80, 0x00, 0x22, 0x00, 0x08, 0x80,        /*!mc         31*/
 0x00, 0x22, 0x00, 0x10, 0x00, 0x83, 0x11, 0x00, 0x10, 0x00, 0x7c, 0x10,        /*!mc         32*/
 0x00, 0x20, 0x00, 0x00, 0x08, 0x00, 0x40, 0x00, 0x00, 0x04, 0x00, 0x80,        /*!mc         33*/
 0x00, 0x00, 0x02, 0x00, 0x00, 0x03, 0x80, 0x01, 0x00, 0x00, 0x1c, 0x70,        /*!mc         34*/
 0x00, 0x00, 0x00, 0xe0, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,        /*!mc         35*/
 0x00, 0x00, 0x00, 0x00, 0x20, 0xe2, 0x23, 0xc4, 0x07, 0x60, 0x23, 0x60,        /*!mc         36*/
 0x44, 0x04, 0xa0, 0x22, 0xa0, 0x44, 0x04, 0x20, 0x22, 0xa0, 0xc4, 0x07,        /*!mc         37*/
 0x20, 0x22, 0x20, 0x45, 0x00, 0x20, 0x22, 0x20, 0x46, 0x00, 0x20, 0xe2,        /*!mc         38*/
 0x23, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};                               /*!mc         39*/
/* Define the XLIB variable structures.                               */        /*!mc         40*/
Colormap       cmap;                                                            /*!mc         41*/
Cursor         cursor;                                                          /*!mc         42*/
Display        *display;                                                        /*!mc         43*/
GC             gc;                                                              /*!mc         44*/
KeySym         keysym;                                                          /*!mc4c2       6*/
Pixmap         icon_pixmap;                                                     /*!mc         45*/
Window         win,root,child;                                                  /*!mc         46*/
XCharStruct    char_info;                                                       /*!mc         47*/
XColor         exact,color_fore,color_back,color;                               /*!mcxk        2*/
XComposeStatus compose;                                                         /*!mc4c2       7*/
XEvent         report;                                                          /*!mc         49*/
XFontStruct    *font_info, *font_scl_info;                                      /*!mc         50*/
XSetWindowAttributes attributes;                                                /*!mc4b       13*/
XSizeHints     *size_hints;                                                     /*!mc         51*/
XTextProperty  windowName,iconName;                                             /*!mc         52*/
XVisualInfo    visual_info;                                                     /*!mc         53*/
XWMHints       *wm_hints;                                                       /*!mc         54*/
/* Define the global variables.                                       */        /*!mc         55*/
char  *display_name=NULL,                                                       /*!mcxk        3*/
      *font_bas[]={"-*-courier-bold-*-*-*-10-100-75-75-*-*-iso8859-1",          /*!mc         61*/
                   "-*-courier-*-*-*-*-*-*-75-75-*-*-iso8859-1",                /*!mc         62*/
                   "-*-*-*-*-*-*-*-*-75-75-*-*-iso8859-1",""},                  /*!mc         63*/
      font_scl[]="-itc-courier-bold-*-*-*-*-0-75-75-*-*-iso8859-1",             /*!mc         64*/
      font_scl_beg[100], font_scl_end[100],                                     /*!mc         65*/
      font_std[]="-*-courier-bold-*-*-*-*-100-75-75-*-*-iso8859-1",             /*!mc         66*/
      font_std_beg[100], font_std_end[100],                                     /*!mc         67*/
      key_buf[MAX_BUF], blanks[MAX_BUF],                                        /*!mc4c2       8*/
      *icon_name="plot", *window_name="PLOT WINDOW";                            /*!mc4c       20*/
int   border_width=10, ch_flag, ch_old, ch_pixel, color_flag,                   /*!mc         69*/
      cursor_shape[]={XC_cross, XC_cross, XC_top_left_corner,                   /*!mc4c2       9*/
      XC_bottom_right_corner}, depth, display_height,                           /*!mc         71*/
      display_height_mm, display_width, display_width_mm, height,               /*!mc         72*/
      pnt_x, pnt_y, res_x, res_y, scrnnm, width, win_flag, win_shape,           /*!mc4b       14*/
      xo=0, yo=0, key_len, key_x, key_y;                                        /*!mc4c2      10*/
unsigned short rgb[NCOLOR+7][3];                                                /*!mcxk        4*/
long  event_mask=ExposureMask|KeyPressMask|ButtonPressMask|                     /*!mc4c2      11*/
                 StructureNotifyMask;                                           /*!mc4c2      12*/
float ch_cm, xmax, xmin, ymax, ymin;                                            /*!mc         76*/
int cnint(float x);                                                             /*!mc4c       21*/
#endif                                                                          /*!cpp        10*/
#ifdef UNIX                                                                     /*!mc         78*/
#include <sys/times.h>                                                          /*!mc4c       22*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mc4c       23*/
FDEF etime_(FDEF a[2])                                                          /*!mc4c       24*/
#else                                                                           /*!mcx         1*/
FDEF etime (FDEF a[2])                                                          /*!mc4c       26*/
#endif                                                                          /*!cpp        13*/
/* Provide a standard unix timing routine.                            */        /*!mc         80*/
{                                                                               /*!mc         82*/
 static int first_call = 1;                                                     /*!mc         83*/
 static FDEF ticks;                                                             /*!mc4c       27*/
 struct tms buffer;                                                             /*!mc         85*/
 if(first_call){                                                                /*!mc         87*/
   first_call = 0;                                                              /*!mc         88*/
   ticks = (FDEF)sysconf(_SC_CLK_TCK);                                          /*!mc4c       28*/
  }                                                                             /*!mc         90*/
 times(&buffer);                                                                /*!mc         91*/
 a[0] = ((FDEF)buffer.tms_utime)/ticks;                                         /*!mc4c       29*/
 a[1] = ((FDEF)buffer.tms_stime)/ticks;                                         /*!mc4c       30*/
 return (a[0]+a[1]);                                                            /*!mc         94*/
}                                                                               /*!mc         95*/
#endif                                                                          /*!cpp        11*/
#if defined(MULTT) || defined(MULTP)                                            /*!mc4c       31*/
#include <sys/time.h>                                                           /*!mcxk        5*/
#ifdef UNIX                                                                     /*!mcxk        6*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcxk        7*/
FDEF wclock_()                                                                  /*!mc4c       33*/
#else                                                                           /*!mcxk        8*/
FDEF wclock ()                                                                  /*!mc4c       35*/
#endif                                                                          /*!cpp        16*/
#endif                                                                          /*!mcxk        9*/
#ifndef UNIX                                                                    /*!mcxk       10*/
#ifdef DEC                                                                      /*!mcxk       11*/
FDEF __stdcall WCLOCK ()                                                        /*!mcxk       12*/
#endif                                                                          /*!mcxk       13*/
#ifdef PCDOS                                                                    /*!mcxk       14*/
FDEF wclock_()                                                                  /*!mcxk       15*/
#endif                                                                          /*!mcxk       16*/
#ifdef UNICOS                                                                   /*!mcxk       17*/
FDEF WCLOCK ()                                                                  /*!mcxk       18*/
#endif                                                                          /*!mcxk       19*/
#endif                                                                          /*!mcxk       20*/
{                                                                               /*!mc4c       38*/
 struct timeval t;                                                              /*!mc4c       39*/
 gettimeofday( &t, (struct timezone *)0 );                                      /*!mc4c       40*/
 return (t.tv_sec + t.tv_usec*1.0e-6);                                          /*!mc4c       41*/
}                                                                               /*!mc4c       42*/
#endif                                                                          /*!cpp        14*/
#ifdef LINUX                                                                    /*!mc4b       19*/
void idate_(IDEF jd[3])                                                         /*!mc4c       43*/
/* Get date for LINUX.                                                */        /*!mc4b       21*/
{                                                                               /*!mc4b       22*/
  struct tm * time_info;                                                        /*!mc4b       23*/
  time_t timep;                                                                 /*!mc4b       24*/
  time(&timep);                                                                 /*!mc4b       25*/
  time_info=localtime(&timep);                                                  /*!mc4b       26*/
  jd[0]=time_info->tm_mday;                                                     /*!mc4b       27*/
  jd[1]=time_info->tm_mon;                                                      /*!mc4b       28*/
  jd[2]=time_info->tm_year;                                                     /*!mc4b       29*/
}                                                                               /*!mc4b       30*/
void itime_(IDEF jd[3])                                                         /*!mc4c       44*/
/* Get time for LINUX.                                                */        /*!mc4b       32*/
{                                                                               /*!mc4b       33*/
  struct tm * time_info;                                                        /*!mc4b       34*/
  time_t timep;                                                                 /*!mc4b       35*/
  time(&timep);                                                                 /*!mc4b       36*/
  time_info=localtime(&timep);                                                  /*!mc4b       37*/
  jd[0]=time_info->tm_hour;                                                     /*!mc4b       38*/
  jd[1]=time_info->tm_min;                                                      /*!mc4b       39*/
  jd[2]=time_info->tm_sec;                                                      /*!mc4b       40*/
}                                                                               /*!mc4b       41*/
#include<signal.h>                                                              /*!mc4b       42*/
lsignal_(IDEF * snum, void (*handler)(int))                                     /*!mc4c       45*/
{                                                                               /*!mc4b       44*/
      signal(*snum,handler);                                                    /*!mc4b       45*/
}                                                                               /*!mc4b       46*/
#endif                                                                          /*!cpp        18*/
#ifdef XLIB                                                                     /*!mc         97*/
/*                                                                    */        /*!mc         98*/
/*   Provide routines that follow the gks structure.                  */        /*!mc         99*/
/*                                                                    */        /*!mc        100*/
#ifdef UNIX                                                                     /*!mc4c       46*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx         2*/
void xgopwk_(IDEF *error_flag)                                                  /*!mcx         3*/
#else                                                                           /*!mcx         4*/
void xgopwk (IDEF *error_flag)                                                  /*!mc4c       50*/
#endif                                                                          /*!cpp        22*/
#endif                                                                          /*!cpp        20*/
#ifndef UNIX                                                                    /*!mc4c       51*/
#ifdef DEC                                                                      /*!mc4c       52*/
void __stdcall XGOPWK (IDEF *error_flag)                                        /*!mc4c       53*/
#endif                                                                          /*!cpp        24*/
#ifdef PCDOS                                                                    /*!mc4c       54*/
void xgopwk_(IDEF *error_flag)                                                  /*!mc4c       55*/
#endif                                                                          /*!cpp        25*/
#ifdef UNICOS                                                                   /*!mc4c       56*/
void XGOPWK (IDEF *error_flag)                                                  /*!mc4c       57*/
#endif                                                                          /*!cpp        26*/
#endif                                                                          /*!cpp        23*/
/* Connect to the server display and get its dimensions.              */        /*!mc        107*/
/* The display can be set using "setenv DISPLAY host_name:0".         */        /*!mc        108*/
{                                                                               /*!mc        110*/
int   i;                                                                        /*!mc        111*/
/* Allocate needed memory and connect to the display.                 */        /*!mc        112*/
if(!(size_hints=XAllocSizeHints()))                                             /*!mc        113*/
 { printf("xlib:  unable to allocate needed memory.\n");                        /*!mc        114*/
   *error_flag=-1; return;                                                      /*!mc4c       58*/
 }                                                                              /*!mc        116*/
if(!(wm_hints=XAllocWMHints()))                                                 /*!mc        117*/
 { printf("xlib:  unable to allocate needed memory.\n");                        /*!mc        118*/
   *error_flag=-1; return;                                                      /*!mc4c       59*/
 }                                                                              /*!mc        120*/
if((display=XOpenDisplay(display_name))==NULL)                                  /*!mc        121*/
 { printf("xlib:  unable to connect to server display.\n");                     /*!mc        122*/
   *error_flag=-2; return;                                                      /*!mc4c       60*/
 }                                                                              /*!mc        124*/
/* Store the window and icon names.                                   */        /*!mc        125*/
if(XStringListToTextProperty(&window_name,1,&windowName)==0)                    /*!mc        126*/
 { XCloseDisplay(display);                                                      /*!mc        127*/
   printf("xlib: unable to perform structure allocation.\n");                   /*!mc        128*/
   *error_flag=-3; return;                                                      /*!mc4c       61*/
 }                                                                              /*!mc        130*/
if(XStringListToTextProperty(&icon_name,1,&iconName)==0)                        /*!mc        131*/
 { XCloseDisplay(display);                                                      /*!mc        132*/
   printf("xlib: unable to perform structure allocation.\n");                   /*!mc        133*/
   *error_flag=-3; return;                                                      /*!mc4c       62*/
 }                                                                              /*!mc        135*/
/* Load the basic font - search several representations for a match.  */        /*!mc        136*/
for(i=0; *font_bas[i] != '\0'; i++)                                             /*!mc4b       48*/
   if((font_info=XLoadQueryFont(display,font_bas[i]))!=NULL) break;             /*!mc        138*/
if(font_info==NULL)                                                             /*!mc        139*/
 { XCloseDisplay(display);                                                      /*!mc        140*/
   printf("xlib: unable to open a font file.\n");                               /*!mc        141*/
   *error_flag=-4; return;                                                      /*!mc4c       63*/
 }                                                                              /*!mc        143*/
/* Get the screen number and display dimensions.                      */        /*!mc        144*/
scrnnm=DefaultScreen(display);                                                  /*!mc4b       49*/
display_width=DisplayWidth(display,scrnnm);                                     /*!mc4b       50*/
display_width_mm=DisplayWidthMM(display,scrnnm);                                /*!mc4b       51*/
display_height=DisplayHeight(display,scrnnm);                                   /*!mc4b       52*/
display_height_mm=DisplayHeightMM(display,scrnnm);                              /*!mc4b       53*/
res_x=25.4*display_width/display_width_mm;                                      /*!mc        150*/
res_y=25.4*display_height/display_height_mm;                                    /*!mc        151*/
}                                                                               /*!mc        152*/
#ifdef UNIX                                                                     /*!mc4c       64*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx         5*/
void xgclwk_()                                                                  /*!mcx         6*/
#else                                                                           /*!mcx         7*/
void xgclwk ()                                                                  /*!mc4c       68*/
#endif                                                                          /*!cpp        29*/
#endif                                                                          /*!cpp        27*/
#ifndef UNIX                                                                    /*!mc4c       69*/
#ifdef DEC                                                                      /*!mc4c       70*/
void __stdcall XGCLWK ()                                                        /*!mc4c       71*/
#endif                                                                          /*!cpp        31*/
#ifdef PCDOS                                                                    /*!mc4c       72*/
void xgclwk_()                                                                  /*!mc4c       73*/
#endif                                                                          /*!cpp        32*/
#ifdef UNICOS                                                                   /*!mc4c       74*/
void XGCLWK ()                                                                  /*!mc4c       75*/
#endif                                                                          /*!cpp        33*/
#endif                                                                          /*!cpp        30*/
/* Release font memory and disconnect from display and server.        */        /*!mc        159*/
{                                                                               /*!mc        160*/
XUnloadFont(display,font_info->fid);                                            /*!mc        161*/
XCloseDisplay(display);                                                         /*!mc        162*/
}                                                                               /*!mc        163*/
#ifdef UNIX                                                                     /*!mc4c       76*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx         8*/
void xgacwk_()                                                                  /*!mcx         9*/
#else                                                                           /*!mcx        10*/
void xgacwk ()                                                                  /*!mc4c       80*/
#endif                                                                          /*!cpp        36*/
#endif                                                                          /*!cpp        34*/
#ifndef UNIX                                                                    /*!mc4c       81*/
#ifdef DEC                                                                      /*!mc4c       82*/
void __stdcall XGACWK ()                                                        /*!mc4c       83*/
#endif                                                                          /*!cpp        38*/
#ifdef PCDOS                                                                    /*!mc4c       84*/
void xgacwk_()                                                                  /*!mc4c       85*/
#endif                                                                          /*!cpp        39*/
#ifdef UNICOS                                                                   /*!mc4c       86*/
void XGACWK ()                                                                  /*!mc4c       87*/
#endif                                                                          /*!cpp        40*/
#endif                                                                          /*!cpp        37*/
/* Initialize all window parameters.                                  */        /*!mc        170*/
{                                                                               /*!mc        171*/
extern void xgstxfp();                                                          /*!mc4c       88*/
/* Note xo,yo is the window origin relative to upper lefthand corner. */        /*!mc        172*/
/* Set window height to min(80% display_height, display_height-yo).   */        /*!mc        173*/
height=cnint(0.8*display_height);                                               /*!mc4b       54*/
if(height>display_height-yo)height=display_height-yo;                           /*!mc        175*/
/* Start with a rectangle viewport (scale factor in sub. tekdvr).     */        /*!mc        176*/
win_shape=0;                                                                    /*!mc        177*/
width=cnint(height*4./3.);                                                      /*!mc4b       55*/
if(width>display_width-xo)                                                      /*!mc        179*/
 { width=display_width-xo;                                                      /*!mc        180*/
   height=cnint(width*3./4.);                                                   /*!mc4b       56*/
 }                                                                              /*!mc        182*/
/* Create an unmapped subwindow of the root.                          */        /*!mc        183*/
win_flag=0;                                                                     /*!mc        184*/
win=XCreateSimpleWindow(display,RootWindow(display,scrnnm),xo,yo,width,         /*!mc4b       57*/
                      height,border_width,BlackPixel(display,scrnnm),           /*!mc4b       58*/
                      WhitePixel(display,scrnnm));                              /*!mc4b       59*/
/* Create pixel map for icon of plot window.                          */        /*!mc        188*/
icon_pixmap=XCreateBitmapFromData(display,win,icon_bitmap_bits,                 /*!mc        189*/
                                  icon_bitmap_width,icon_bitmap_height);        /*!mc        190*/
/* Initialize various window manager properties.                      */        /*!mc        191*/
size_hints->flags=PPosition|PSize|PMinSize;                                     /*!mc        192*/
size_hints->min_width=cnint(width/2.0);                                         /*!mc4b       60*/
size_hints->min_height=cnint(height/2.0);                                       /*!mc4b       61*/
wm_hints->initial_state=NormalState;                                            /*!mc        195*/
wm_hints->input=True;                                                           /*!mc        196*/
wm_hints->icon_pixmap=icon_pixmap;                                              /*!mc        197*/
wm_hints->flags=StateHint|IconPixmapHint|InputHint;                             /*!mc        198*/
XSetWMProperties(display,win,&windowName,&iconName,NULL,0,size_hints,           /*!mc        199*/
                 wm_hints,NULL);                                                /*!mc        200*/
/* Select the event types - most are not currently used.              */        /*!mc        201*/
XSelectInput(display,win,event_mask);                                           /*!mc        202*/
/* Create the graphics context.                                       */        /*!mc        203*/
gc=XCreateGC(display,win,0,NULL);                                               /*!mc        204*/
XSetForeground(display,gc,BlackPixel(display,scrnnm));                          /*!mc4b       62*/
XSetBackground(display,gc,WhitePixel(display,scrnnm));                          /*!mc4b       63*/
if(XDoesBackingStore(DefaultScreenOfDisplay(display)) != NotUseful)             /*!mc4b       64*/
 { attributes.backing_store=Always;                                             /*!mc4b       65*/
   XChangeWindowAttributes(display,win,CWBackingStore,&attributes);             /*!mc4b       66*/
 }                                                                              /*!mc4b       67*/
/* Initialize the fonts.                                              */        /*!mc        207*/
xgstxfp ();                                                                     /*!mc        208*/
ch_flag=0;                                                                      /*!mc        209*/
ch_old=0;                                                                       /*!mc        210*/
}                                                                               /*!mc        211*/
#ifdef UNIX                                                                     /*!mc4c       89*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        11*/
void xgdawk_()                                                                  /*!mcx        12*/
#else                                                                           /*!mcx        13*/
void xgdawk ()                                                                  /*!mc4c       93*/
#endif                                                                          /*!cpp        43*/
#endif                                                                          /*!cpp        41*/
#ifndef UNIX                                                                    /*!mc4c       94*/
#ifdef DEC                                                                      /*!mc4c       95*/
void __stdcall XGDAWK ()                                                        /*!mc4c       96*/
#endif                                                                          /*!cpp        45*/
#ifdef PCDOS                                                                    /*!mc4c       97*/
void xgdawk_()                                                                  /*!mc4c       98*/
#endif                                                                          /*!cpp        46*/
#ifdef UNICOS                                                                   /*!mc4c       99*/
void XGDAWK ()                                                                  /*!mc4c      100*/
#endif                                                                          /*!cpp        47*/
#endif                                                                          /*!cpp        44*/
/* Release window memory and remove plot window.                      */        /*!mc        218*/
{                                                                               /*!mc        219*/
if(font_scl_info!=NULL)XUnloadFont(display,font_scl_info->fid);                 /*!mc        220*/
XFreeGC(display,gc);                                                            /*!mc        221*/
XDestroyWindow(display,win);                                                    /*!mc        222*/
XFlush(display);                                                                /*!mc        223*/
}                                                                               /*!mc        224*/
#ifdef UNIX                                                                     /*!mc4c      101*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        14*/
void xgclrw_()                                                                  /*!mcx        15*/
#else                                                                           /*!mcx        16*/
void xgclrw ()                                                                  /*!mc4c      105*/
#endif                                                                          /*!cpp        50*/
#endif                                                                          /*!cpp        48*/
#ifndef UNIX                                                                    /*!mc4c      106*/
#ifdef DEC                                                                      /*!mc4c      107*/
void __stdcall XGCLRW ()                                                        /*!mc4c      108*/
#endif                                                                          /*!cpp        52*/
#ifdef PCDOS                                                                    /*!mc4c      109*/
void xgclrw_()                                                                  /*!mc4c      110*/
#endif                                                                          /*!cpp        53*/
#ifdef UNICOS                                                                   /*!mc4c      111*/
void XGCLRW ()                                                                  /*!mc4c      112*/
#endif                                                                          /*!cpp        54*/
#endif                                                                          /*!cpp        51*/
/* Clear the window.                                                  */        /*!mc        231*/
{                                                                               /*!mc        232*/
XClearWindow(display,win);                                                      /*!mc        233*/
}                                                                               /*!mc        234*/
#ifdef UNIX                                                                     /*!mc4c      113*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        17*/
void xguwk_()                                                                   /*!mcx        18*/
#else                                                                           /*!mcx        19*/
void xguwk ()                                                                   /*!mc4c      117*/
#endif                                                                          /*!cpp        57*/
#endif                                                                          /*!cpp        55*/
#ifndef UNIX                                                                    /*!mc4c      118*/
#ifdef DEC                                                                      /*!mc4c      119*/
void __stdcall XGUWK ()                                                         /*!mc4c      120*/
#endif                                                                          /*!cpp        59*/
#ifdef PCDOS                                                                    /*!mc4c      121*/
void xguwk_()                                                                   /*!mc4c      122*/
#endif                                                                          /*!cpp        60*/
#ifdef UNICOS                                                                   /*!mc4c      123*/
void XGUWK ()                                                                   /*!mc4c      124*/
#endif                                                                          /*!cpp        61*/
#endif                                                                          /*!cpp        58*/
/* Sound the bell and process any queued events.                      */        /*!mc        241*/
{                                                                               /*!mc        242*/
XBell(display,100);                                                             /*!mc        243*/
XFlush(display);                                                                /*!mc        244*/
}                                                                               /*!mc        245*/
#ifdef UNIX                                                                     /*!mc4c      125*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        20*/
void xgpl_(FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                                 /*!mcx        21*/
#else                                                                           /*!mcx        22*/
void xgpl (FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                                 /*!mc4c      129*/
#endif                                                                          /*!cpp        64*/
#endif                                                                          /*!cpp        62*/
#ifndef UNIX                                                                    /*!mc4c      130*/
#ifdef DEC                                                                      /*!mc4c      131*/
void __stdcall XGPL (FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                       /*!mc4c      132*/
#endif                                                                          /*!cpp        66*/
#ifdef PCDOS                                                                    /*!mc4c      133*/
void xgpl_(FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                                 /*!mc4c      134*/
#endif                                                                          /*!cpp        67*/
#ifdef UNICOS                                                                   /*!mc4c      135*/
void XGPL (FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                                 /*!mc4c      136*/
#endif                                                                          /*!cpp        68*/
#endif                                                                          /*!cpp        65*/
/* Draw a line from point x1,y1 to x2,y2.                             */        /*!mc        252*/
{                                                                               /*!mc        254*/
int   xa,ya,xb,yb;                                                              /*!mc        255*/
xa=cnint(width*(*x1-xmin)/(xmax-xmin));                                         /*!mc4b       68*/
xb=cnint(width*(*x2-xmin)/(xmax-xmin));                                         /*!mc4b       69*/
ya=cnint(height*(ymax-*y1)/(ymax-ymin));                                        /*!mc4b       70*/
yb=cnint(height*(ymax-*y2)/(ymax-ymin));                                        /*!mc4b       71*/
XDrawLine(display,win,gc,xa,ya,xb,yb);                                          /*!mc        260*/
}                                                                               /*!mc        261*/
#ifdef UNIX                                                                     /*!mc4c      137*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        23*/
void xgtx_(CDEF *string,IDEF *length,FDEF *x,FDEF *y)                           /*!mcx        24*/
#else                                                                           /*!mcx        25*/
void xgtx (CDEF *string,IDEF *length,FDEF *x,FDEF *y)                           /*!mc4c      141*/
#endif                                                                          /*!cpp        71*/
#endif                                                                          /*!cpp        69*/
#ifndef UNIX                                                                    /*!mc4c      142*/
#ifdef DEC                                                                      /*!mc4c      143*/
void __stdcall XGTX (CDEF *string,IDEF *l,IDEF *length,FDEF *x,FDEF *y)         /*!mc4c      144*/
#endif                                                                          /*!cpp        73*/
#ifdef PCDOS                                                                    /*!mc4c      145*/
void xgtx_(CDEF *string,IDEF *length,FDEF *x,FDEF *y)                           /*!mc4c      146*/
#endif                                                                          /*!cpp        74*/
#ifdef UNICOS                                                                   /*!mc4c      147*/
void XGTX (CDEF text,IDEF *length,FDEF *x,FDEF *y)                              /*!mc4c      148*/
#endif                                                                          /*!cpp        75*/
#endif                                                                          /*!cpp        72*/
/* Draw a character string starting at point x,y.                     */        /*!mc        268*/
{                                                                               /*!mc        272*/
char  *ch_str;                                                                  /*!mc4b       78*/
int   xa,ya,i;                                                                  /*!mc        273*/
extern void xscfont();                                                          /*!mc4c      149*/
#ifdef UNICOS                                                                   /*!mc4b       79*/
char  *string;                                                                  /*!mc4b       80*/
string=_fcdtocp(text);                                                          /*!mc4b       81*/
#endif                                                                          /*!cpp        76*/
ch_str=string;                                                                  /*!mc4b       85*/
if(ch_pixel!=ch_old)xscfont();                                                  /*!mc        274*/
xa=cnint(width*(*x-xmin)/(xmax-xmin));                                          /*!mc4b       86*/
ya=cnint(height*(ymax-*y)/(ymax-ymin));                                         /*!mc4b       87*/
if(ch_flag)for(i=*length-1; i>=0; i--)                                          /*!mc        277*/
         {  XDrawImageString(display,win,gc,xa,ya,ch_str+i,1);                  /*!mc4b       88*/
              ya=ya-font_scl_info->ascent-font_scl_info->descent;               /*!mc        279*/
           }                                                                    /*!mc        280*/
else XDrawImageString(display,win,gc,xa,ya,ch_str,*length);                     /*!mc4b       89*/
}                                                                               /*!mc        282*/
void xgstxfp ()                                                                 /*!mc4c      150*/
/* Initialize the scalable, standard, and basic fonts.                */        /*!mc        284*/
/* This is called by xgacwk() rather than sub. gstxfp in MCNP which   */        /*!mc        285*/
/* is positioned poorly in sub. gxon.                                 */        /*!mc        286*/
{                                                                               /*!mc        287*/
int   i,j,field,end;                                                            /*!mc        288*/
/* Setup the scalable font - insert the display resolutions.          */        /*!mc        289*/
for(i=j=field=end=0; field<=14&&font_scl[i]!='\0'; i++)                         /*!mc        290*/
 { if(end)font_scl_end[j++]=font_scl[i];                                        /*!mc        291*/
   else font_scl_beg[j++]=font_scl[i];                                          /*!mc        292*/
   if(font_scl[i]=='-')                                                         /*!mc        293*/
   {  field++;                                                                  /*!mc        294*/
      switch (field)                                                            /*!mc        295*/
      {  case 8:                                                                /*!mc        296*/
            font_scl_beg[j]='\0';                                               /*!mc        297*/
            while(font_scl[i+1]!='-')i++;                                       /*!mc        298*/
            end=1;                                                              /*!mc        299*/
            j=0;                                                                /*!mc        300*/
            break;                                                              /*!mc        301*/
         case 9:                                                                /*!mc        302*/
            sprintf(&font_scl_end[j],"%d",res_x);                               /*!mc        303*/
            while(font_scl_end[j]!='\0')j++;                                    /*!mc        304*/
            while(font_scl[i+1]!='-')i++;                                       /*!mc        305*/
            break;                                                              /*!mc        306*/
         case 10:                                                               /*!mc        307*/
            sprintf(&font_scl_end[j],"%d",res_y);                               /*!mc        308*/
            while(font_scl_end[j]!='\0')j++;                                    /*!mc        309*/
            while(font_scl[i+1]!='-')i++;                                       /*!mc        310*/
            break;                                                              /*!mc        311*/
      }                                                                         /*!mc        312*/
   }                                                                            /*!mc        313*/
 }                                                                              /*!mc        314*/
font_scl_end[j]='\0';                                                           /*!mc        315*/
/* Setup the standard font for scaling.                               */        /*!mc        316*/
for(i=j=field=end=0; field<=14&&font_std[i]!='\0'; i++)                         /*!mc        317*/
 { if(end)font_std_end[j++]=font_std[i];                                        /*!mc        318*/
   else font_std_beg[j++]=font_std[i];                                          /*!mc        319*/
   if(font_std[i]=='-')                                                         /*!mc        320*/
   {  field++;                                                                  /*!mc        321*/
      switch (field)                                                            /*!mc        322*/
      {  case 8:                                                                /*!mc        323*/
            font_std_beg[j]='\0';                                               /*!mc        324*/
            while(font_std[i+1]!='-')i++;                                       /*!mc        325*/
            end=1;                                                              /*!mc        326*/
            j=0;                                                                /*!mc        327*/
            break;                                                              /*!mc        328*/
      }                                                                         /*!mc        329*/
   }                                                                            /*!mc        330*/
 }                                                                              /*!mc        331*/
font_std_end[j]='\0';                                                           /*!mc        332*/
/* Load the basic font.                                               */        /*!mc        333*/
XSetFont(display,gc,font_info->fid);                                            /*!mc        334*/
}                                                                               /*!mc        335*/
#ifdef UNIX                                                                     /*!mc4c      151*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        26*/
void xgschh_(FDEF *ch)                                                          /*!mcx        27*/
#else                                                                           /*!mcx        28*/
void xgschh (FDEF *ch)                                                          /*!mc4c      155*/
#endif                                                                          /*!cpp        79*/
#endif                                                                          /*!cpp        77*/
#ifndef UNIX                                                                    /*!mc4c      156*/
#ifdef DEC                                                                      /*!mc4c      157*/
void __stdcall XGSCHH (FDEF *ch)                                                /*!mc4c      158*/
#endif                                                                          /*!cpp        81*/
#ifdef PCDOS                                                                    /*!mc4c      159*/
void xgschh_(FDEF *ch)                                                          /*!mc4c      160*/
#endif                                                                          /*!cpp        82*/
#ifdef UNICOS                                                                   /*!mc4c      161*/
void XGSCHH (FDEF *ch)                                                          /*!mc4c      162*/
#endif                                                                          /*!cpp        83*/
#endif                                                                          /*!cpp        80*/
/* Set the character height in pixels.                                */        /*!mc        342*/
{                                                                               /*!mc        344*/
ch_cm=*ch;                                                                      /*!mc        345*/
if(ch_flag)ch_pixel=ch_cm*width/(xmax-xmin);                                    /*!mc        346*/
else ch_pixel=ch_cm*height/(ymax-ymin);                                         /*!mc        347*/
}                                                                               /*!mc        348*/
#ifdef UNIX                                                                     /*!mc4c      163*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        29*/
void xgschu_(FDEF *x,FDEF *y)                                                   /*!mcx        30*/
#else                                                                           /*!mcx        31*/
void xgschu (FDEF *x,FDEF *y)                                                   /*!mc4c      167*/
#endif                                                                          /*!cpp        86*/
#endif                                                                          /*!cpp        84*/
#ifndef UNIX                                                                    /*!mc4c      168*/
#ifdef DEC                                                                      /*!mc4c      169*/
void __stdcall XGSCHU (FDEF *x,FDEF *y)                                         /*!mc4c      170*/
#endif                                                                          /*!cpp        88*/
#ifdef PCDOS                                                                    /*!mc4c      171*/
void xgschu_(FDEF *x,FDEF *y)                                                   /*!mc4c      172*/
#endif                                                                          /*!cpp        89*/
#ifdef UNICOS                                                                   /*!mc4c      173*/
void XGSCHU (FDEF *x,FDEF *y)                                                   /*!mc4c      174*/
#endif                                                                          /*!cpp        90*/
#endif                                                                          /*!cpp        87*/
/* Set the character up vector - ch_flag=1 for rotated text.          */        /*!mc        355*/
{                                                                               /*!mc        357*/
float xa,ya;                                                                    /*!mc4b       90*/
xa=*x;                                                                          /*!mc4b       91*/
ya=*y;                                                                          /*!mc4b       92*/
if(xa < 0.)xa=-1.*xa;                                                           /*!mc4b       93*/
if(ya < 0.)ya=-1.*ya;                                                           /*!mc4b       94*/
if(xa > ya)                                                                     /*!mc4b       95*/
ch_flag=1;                                                                      /*!mc4b       96*/
else                                                                            /*!mc        362*/
ch_flag=0;                                                                      /*!mc4b       97*/
}                                                                               /*!mc        366*/
#ifdef UNIX                                                                     /*!mc4c      175*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        32*/
void xgswn_(FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                                /*!mcx        33*/
#else                                                                           /*!mcx        34*/
void xgswn (FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                                /*!mc4c      179*/
#endif                                                                          /*!cpp        93*/
#endif                                                                          /*!cpp        91*/
#ifndef UNIX                                                                    /*!mc4c      180*/
#ifdef DEC                                                                      /*!mc4c      181*/
void __stdcall XGSWN (FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                      /*!mc4c      182*/
#endif                                                                          /*!cpp        95*/
#ifdef PCDOS                                                                    /*!mc4c      183*/
void xgswn_(FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                                /*!mc4c      184*/
#endif                                                                          /*!cpp        96*/
#ifdef UNICOS                                                                   /*!mc4c      185*/
void XGSWN (FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                                /*!mc4c      186*/
#endif                                                                          /*!cpp        97*/
#endif                                                                          /*!cpp        94*/
/* Save the window size.                                              */        /*!mc4b       98*/
{                                                                               /*!mc        375*/
xmin=*x1;                                                                       /*!mc        377*/
xmax=*x2;                                                                       /*!mc        378*/
ymin=*y1;                                                                       /*!mc        379*/
ymax=*y2;                                                                       /*!mc        380*/
}                                                                               /*!mc4b       99*/
#ifdef UNIX                                                                     /*!mc4c      187*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        35*/
void xgswk_(FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                                /*!mcx        36*/
#else                                                                           /*!mcx        37*/
void xgswk (FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                                /*!mc4c      191*/
#endif                                                                          /*!cpp       100*/
#endif                                                                          /*!cpp        98*/
#ifndef UNIX                                                                    /*!mc4c      192*/
#ifdef DEC                                                                      /*!mc4c      193*/
void __stdcall XGSWK (FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                      /*!mc4c      194*/
#endif                                                                          /*!cpp       102*/
#ifdef PCDOS                                                                    /*!mc4c      195*/
void xgswk_(FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                                /*!mc4c      196*/
#endif                                                                          /*!cpp       103*/
#ifdef UNICOS                                                                   /*!mc4c      197*/
void XGSWK (FDEF *x1,FDEF *x2,FDEF *y1,FDEF *y2)                                /*!mc4c      198*/
#endif                                                                          /*!cpp       104*/
#endif                                                                          /*!cpp       101*/
/* Draw the window on the display.                                    */        /*!mc4b      106*/
{                                                                               /*!mc4b      108*/
float scale;                                                                    /*!mc4b      109*/
int   pre_width;                                                                /*!mc4b      110*/
long int i;                                                                     /*!mc4b      111*/
extern void xevent();                                                           /*!mc4c      199*/
/* Check if the window has been resized.                              */        /*!mc        381*/
xevent(0);                                                                      /*!mc        382*/
pre_width=width;                                                                /*!mc4b      112*/
/* Scale = 1.25 for a rectangular window and 1.00 for for square.     */        /*!mc4b      113*/
scale=(*x2-*x1)/(*y2-*y1);                                                      /*!mc4b      114*/
win_shape=0;                                                                    /*!mc4b      115*/
if(scale <= 1.01)win_shape=1;                                                   /*!mc4b      116*/
width=cnint(height*scale);                                                      /*!mc4b      117*/
if(width>display_width-xo)                                                      /*!mc4b      118*/
 { width=display_width-xo;                                                      /*!mc4b      119*/
   height=cnint(width/scale);                                                   /*!mc4b      120*/
 }                                                                              /*!mc4b      121*/
if(width != pre_width)                                                          /*!mc4b      122*/
 { XResizeWindow(display,win,width,height);                                     /*!mc4b      123*/
   xevent(2);                                                                   /*!mc4b      124*/
 }                                                                              /*!mc4b      125*/
/* Raise the window on the display.                                   */        /*!mc4b      126*/
XMapRaised(display,win);                                                        /*!mc        405*/
/* First time wait for an expose event - process all other events.    */        /*!mc4b      127*/
if(!win_flag)xevent(3);                                                         /*!mc        408*/
win_flag=1;                                                                     /*!mc        409*/
xevent(0);                                                                      /*!mc        410*/
for(i=0; i<1000000; i++);                                                       /*!mc4b      128*/
}                                                                               /*!mc        411*/
#ifdef UNIX                                                                     /*!mc4c      200*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        38*/
void xgrqlc_(IDEF *flag,FDEF *x,FDEF *y)                                        /*!mcx        39*/
#else                                                                           /*!mcx        40*/
void xgrqlc (IDEF *flag,FDEF *x,FDEF *y)                                        /*!mc4c      204*/
#endif                                                                          /*!cpp       107*/
#endif                                                                          /*!cpp       105*/
#ifndef UNIX                                                                    /*!mc4c      205*/
#ifdef DEC                                                                      /*!mc4c      206*/
void __stdcall XGRQLC (IDEF *flag,FDEF *x,FDEF *y)                              /*!mc4c      207*/
#endif                                                                          /*!cpp       109*/
#ifdef PCDOS                                                                    /*!mc4c      208*/
void xgrqlc_(IDEF *flag,FDEF *x,FDEF *y)                                        /*!mc4c      209*/
#endif                                                                          /*!cpp       110*/
#ifdef UNICOS                                                                   /*!mc4c      210*/
void XGRQLC (IDEF *flag,FDEF *x,FDEF *y)                                        /*!mc4c      211*/
#endif                                                                          /*!cpp       111*/
#endif                                                                          /*!cpp       108*/
/* Get pointer position.                                              */        /*!mc        418*/
{                                                                               /*!mc        421*/
long int i;                                                                     /*!mc4b      129*/
int   x1,x2,y1,y2;                                                              /*!mc        423*/
extern void xevent();                                                           /*!mc4c      212*/
/* Set cursor, flush previous ButtonPress events, and sound the bell. */        /*!mc        424*/
cursor=XCreateFontCursor(display, cursor_shape[*flag]);                         /*!mc4c2      13*/
XDefineCursor(display,win,cursor);                                              /*!mc        426*/
if(color_flag)XRecolorCursor(display,cursor,&color_fore,&color_back);           /*!mc        427*/
xevent(0);                                                                      /*!mc        428*/
XBell(display,100);                                                             /*!mc        429*/
/* Wait for user input via any mouse button.                          */        /*!mc        430*/
xevent(1);                                                                      /*!mc        431*/
/* Place the appropriate cross-hair at the chosen pixel.              */        /*!mc        432*/
if(*flag!=0)                                                                    /*!mc4c2      14*/
 { if(*flag==1)                                                                 /*!mc4c2      15*/
    { x1=pnt_x-height/80;                                                       /*!mc4c2      16*/
      x2=pnt_x+height/80;                                                       /*!mc4c2      17*/
      y1=pnt_y-height/80;                                                       /*!mc4c2      18*/
      y2=pnt_y+height/80;                                                       /*!mc4c2      19*/
    }                                                                           /*!mc4c2      20*/
   else                                                                         /*!mc4c2      21*/
    { x1=xo+width*(1-win_shape)/4;                                              /*!mc4c2      22*/
      x2=width;                                                                 /*!mc4c2      23*/
      y1=yo;                                                                    /*!mc4c2      24*/
      y2=height;                                                                /*!mc4c2      25*/
    }                                                                           /*!mc4c2      26*/
   XDrawLine(display,win,gc,pnt_x,y1,pnt_x,y2);                                 /*!mc4c2      27*/
   XDrawLine(display,win,gc,x1,pnt_y,x2,pnt_y);                                 /*!mc4c2      28*/
   XFlush(display);                                                             /*!mc4c2      29*/
   for(i=0; i<1000000; i++);                                                    /*!mc4c2      30*/
 }                                                                              /*!mc4c2      31*/
/* Return the pixel location.                                         */        /*!mc        450*/
*x=xmin+pnt_x*(xmax-xmin)/width;                                                /*!mc        451*/
*y=ymax-pnt_y*(ymax-ymin)/height;                                               /*!mc        452*/
XUndefineCursor(display,win);                                                   /*!mc4c2      32*/
XFreeCursor(display,cursor);                                                    /*!mc4c2      33*/
}                                                                               /*!mc        454*/
#ifdef UNIX                                                                     /*!mc4c      213*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        41*/
void xginst_(CDEF *text,IDEF *size,IDEF *length,FDEF *x,FDEF *y)                /*!mcx        42*/
#else                                                                           /*!mcx        43*/
void xginst (CDEF *text,IDEF *size,IDEF *length,FDEF *x,FDEF *y)                /*!mc4c2      36*/
#endif                                                                          /*!cpp       114*/
#endif                                                                          /*!cpp       112*/
#ifndef UNIX                                                                    /*!mc4c2      37*/
#ifdef DEC                                                                      /*!mc4c2      38*/
void __stdcall                                                                  /*!mc4c2      39*/
     XGINST (CDEF *text,IDEF *l,IDEF *size,IDEF *length,FDEF *x,FDEF *y)        /*!mc4c2      40*/
#endif                                                                          /*!cpp       116*/
#ifdef PCDOS                                                                    /*!mc4c2      41*/
void xginst_(CDEF *text,IDEF *size,IDEF *length,FDEF *x,FDEF *y)                /*!mc4c2      42*/
#endif                                                                          /*!cpp       117*/
#ifdef UNICOS                                                                   /*!mc4c2      43*/
void XGINST (CDEF *text,IDEF *size,IDEF *length,FDEF *x,FDEF *y)                /*!mc4c2      44*/
#endif                                                                          /*!cpp       118*/
#endif                                                                          /*!cpp       115*/
/* GKS function 'initialise string'                                   */        /*!mc4c2      45*/
/* Set size and location of string input and display initial text     */        /*!mc4c2      46*/
{                                                                               /*!mc4c2      47*/
int   i;                                                                        /*!mc4c2      48*/
/* extern void xscfont();                                             */        /*!mc4c2      49*/
key_len=*size;                                                                  /*!mc4c2      50*/
if(key_len >= MAX_BUF)key_len=MAX_BUF-1;                                        /*!mc4c2      51*/
for(i=0; i<key_len; i++)blanks[i]=' ';                                          /*!mc4c2      52*/
blanks[key_len]='\0';                                                           /*!mc4c2      53*/
key_buf[0]='\0';                                                                /*!mc4c2      54*/
#ifdef UNICOS                                                                   /*!mc4c2      55*/
strncpy(key_buf,_fcdtocp(text),*length);                                        /*!mc4c2      56*/
#endif                                                                          /*!cpp       119*/
#ifndef UNICOS                                                                  /*!mc4c2      57*/
strncpy(key_buf,text,*length);                                                  /*!mc4c2      58*/
#endif                                                                          /*!cpp       120*/
/* if(ch_pixel!=ch_old)xscfont();                                     */        /*!mc4c2      59*/
key_x=cnint(width*(*x-xmin)/(xmax-xmin));                                       /*!mc4c2      60*/
key_y=cnint(height*(ymax-*y)/(ymax-ymin));                                      /*!mc4c2      61*/
XDrawImageString(display,win,gc,key_x,key_y,blanks,key_len);                    /*!mc4c2      62*/
XDrawString(display,win,gc,key_x,key_y,key_buf,strlen(key_buf));                /*!mc4c2      63*/
}                                                                               /*!mc4c2      64*/
#ifdef UNIX                                                                     /*!mc4c2      65*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        44*/
void xgrqst_(CDEF *text)                                                        /*!mcx        45*/
#else                                                                           /*!mcx        46*/
void xgrqst (CDEF *text)                                                        /*!mc4c2      69*/
#endif                                                                          /*!cpp       123*/
#endif                                                                          /*!cpp       121*/
#ifndef UNIX                                                                    /*!mc4c2      70*/
#ifdef DEC                                                                      /*!mc4c2      71*/
void __stdcall XGRQST (CDEF *text,IDEF *l)                                      /*!mc4c2      72*/
#endif                                                                          /*!cpp       125*/
#ifdef PCDOS                                                                    /*!mc4c2      73*/
void xgrqst_(CDEF *text)                                                        /*!mc4c2      74*/
#endif                                                                          /*!cpp       126*/
#ifdef UNICOS                                                                   /*!mc4c2      75*/
void XGRQST (CDEF *text)                                                        /*!mc4c2      76*/
#endif                                                                          /*!cpp       127*/
#endif                                                                          /*!cpp       124*/
/* GKS function 'request string'                                      */        /*!mc4c2      77*/
/* Get and echo keyboard input.                                       */        /*!mc4c2      78*/
{                                                                               /*!mc4c2      79*/
int   i;                                                                        /*!mc4c2      80*/
extern void xevent();                                                           /*!mc4c2      81*/
xevent(4);                                                                      /*!mc4c2      82*/
for(i=strlen(key_buf)+1; i<=key_len; i++)strcat(key_buf," ");                   /*!mc4c2      83*/
#ifdef UNICOS                                                                   /*!mc4c2      84*/
strncpy(_fcdtocp(text),key_buf,key_len);                                        /*!mc4c2      85*/
#endif                                                                          /*!cpp       128*/
#ifndef UNICOS                                                                  /*!mc4c2      86*/
strncpy(text,key_buf,key_len);                                                  /*!mc4c2      87*/
#endif                                                                          /*!cpp       129*/
}                                                                               /*!mc4c2      88*/
#ifdef UNIX                                                                     /*!mc4c2      89*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        47*/
void xgfa_(FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                                 /*!mcx        48*/
#else                                                                           /*!mcx        49*/
void xgfa (FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                                 /*!mc4c      217*/
#endif                                                                          /*!cpp       132*/
#endif                                                                          /*!cpp       130*/
#ifndef UNIX                                                                    /*!mc4c      218*/
#ifdef DEC                                                                      /*!mc4c      219*/
void __stdcall XGFA (FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                       /*!mc4c      220*/
#endif                                                                          /*!cpp       134*/
#ifdef PCDOS                                                                    /*!mc4c      221*/
void xgfa_(FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                                 /*!mc4c      222*/
#endif                                                                          /*!cpp       135*/
#ifdef UNICOS                                                                   /*!mc4c      223*/
void XGFA (FDEF *x1,FDEF *y1,FDEF *x2,FDEF *y2)                                 /*!mc4c      224*/
#endif                                                                          /*!cpp       136*/
#endif                                                                          /*!cpp       133*/
/* Fill the rectangular area with the initialized color.              */        /*!mc        461*/
{                                                                               /*!mc        463*/
int   xa,ya,xb,yb;                                                              /*!mc        464*/
xa=cnint(width*(*x1-xmin)/(xmax-xmin));                                         /*!mc4b      131*/
xb=cnint(width*(*x2-xmin)/(xmax-xmin));                                         /*!mc4b      132*/
ya=cnint(height*(ymax-*y1)/(ymax-ymin));                                        /*!mc4b      133*/
yb=cnint(height*(ymax-*y2)/(ymax-ymin));                                        /*!mc4b      134*/
XFillRectangle(display,win,gc,xa,ya,xb-xa,yb-ya);                               /*!mc        469*/
}                                                                               /*!mc        470*/
#ifdef UNIX                                                                     /*!mc4c      225*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        50*/
void xgsplc_(IDEF *ic)                                                          /*!mcx        51*/
#else                                                                           /*!mcx        52*/
void xgsplc (IDEF *ic)                                                          /*!mc4c      229*/
#endif                                                                          /*!cpp       139*/
#endif                                                                          /*!cpp       137*/
#ifndef UNIX                                                                    /*!mc4c      230*/
#ifdef DEC                                                                      /*!mc4c      231*/
void __stdcall XGSPLC (IDEF *ic)                                                /*!mc4c      232*/
#endif                                                                          /*!cpp       141*/
#ifdef PCDOS                                                                    /*!mc4c      233*/
void xgsplc_(IDEF *ic)                                                          /*!mc4c      234*/
#endif                                                                          /*!cpp       142*/
#ifdef UNICOS                                                                   /*!mc4c      235*/
void XGSPLC (IDEF *ic)                                                          /*!mc4c      236*/
#endif                                                                          /*!cpp       143*/
#endif                                                                          /*!cpp       140*/
/* Set the line color.                                                */        /*!mc        477*/
{                                                                               /*!mc        479*/
color.red=rgb[*ic-1][0];                                                        /*!mcxk       21*/
color.green=rgb[*ic-1][1];                                                      /*!mcxk       22*/
color.blue=rgb[*ic-1][2];                                                       /*!mcxk       23*/
if(XAllocColor(display,cmap,&color))                                            /*!mcxk       24*/
  XSetForeground(display,gc,color.pixel);                                       /*!mcxk       25*/
}                                                                               /*!mc        482*/
#ifdef UNIX                                                                     /*!mc4c      237*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        53*/
void xgqcf_(IDEF *type,IDEF *nc)                                                /*!mcx        54*/
#else                                                                           /*!mcx        55*/
void xgqcf (IDEF *type,IDEF *nc)                                                /*!mc4c      241*/
#endif                                                                          /*!cpp       146*/
#endif                                                                          /*!cpp       144*/
#ifndef UNIX                                                                    /*!mc4c      242*/
#ifdef DEC                                                                      /*!mc4c      243*/
void __stdcall XGQCF (IDEF *type,IDEF *nc)                                      /*!mc4c      244*/
#endif                                                                          /*!cpp       148*/
#ifdef PCDOS                                                                    /*!mc4c      245*/
void xgqcf_(IDEF *type,IDEF *nc)                                                /*!mc4c      246*/
#endif                                                                          /*!cpp       149*/
#ifdef UNICOS                                                                   /*!mc4c      247*/
void XGQCF (IDEF *type,IDEF *nc)                                                /*!mc4c      248*/
#endif                                                                          /*!cpp       150*/
#endif                                                                          /*!cpp       147*/
/* Set the color flag if a color display is used.                     */        /*!mc        489*/
{                                                                               /*!mc        491*/
int   i,j;                                                                      /*!mc        492*/
color_flag=0;                                                                   /*!mc        493*/
if((depth=DefaultDepth(display,scrnnm)) == 1)return;                            /*!mc4b      135*/
cmap=DefaultColormap(display,scrnnm);                                           /*!mc4b      136*/
for(i=5; i>=StaticColor; i--)                                                   /*!mc        496*/
   if(XMatchVisualInfo(display,scrnnm,depth,i,&visual_info))                    /*!mc4b      137*/
   {  color_flag=1;                                                             /*!mc        498*/
      XAllocNamedColor(display,cmap,"white",&color_back,&exact);                /*!mc        499*/
      *type=1; *nc=NCOLOR; return;                                              /*!mcxk       26*/
   }                                                                            /*!mc        502*/
}                                                                               /*!mc        503*/
#ifdef UNIX                                                                     /*!mcxk       27*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcxk       28*/
void xgscr_(IDEF *ic,FDEF *r,FDEF *g,FDEF *b)                                   /*!mcxk       29*/
#else                                                                           /*!mcxk       30*/
void xgscr (IDEF *ic,FDEF *r,FDEF *g,FDEF *b)                                   /*!mcxk       31*/
#endif                                                                          /*!mcxk       32*/
#endif                                                                          /*!mcxk       33*/
#ifndef UNIX                                                                    /*!mcxk       34*/
#ifdef DEC                                                                      /*!mcxk       35*/
void __stdcall XGSCR (IDEF *ic,FDEF *r,FDEF *g,FDEF *b)                         /*!mcxk       36*/
#endif                                                                          /*!mcxk       37*/
#ifdef PCDOS                                                                    /*!mcxk       38*/
void xgscr_(IDEF *ic,FDEF *r,FDEF *g,FDEF *b)                                   /*!mcxk       39*/
#endif                                                                          /*!mcxk       40*/
#ifdef UNICOS                                                                   /*!mcxk       41*/
void XGSCR (IDEF *ic,FDEF *r,FDEF *g,FDEF *b)                                   /*!mcxk       42*/
#endif                                                                          /*!mcxk       43*/
#endif                                                                          /*!mcxk       44*/
/* Set the color rgb values.                                          */        /*!mcxk       45*/
{                                                                               /*!mcxk       46*/
rgb[*ic-1][0]=cnint(65535.*(*r));                                               /*!mcxk       47*/
rgb[*ic-1][1]=cnint(65535.*(*g));                                               /*!mcxk       48*/
rgb[*ic-1][2]=cnint(65535.*(*b));                                               /*!mcxk       49*/
}                                                                               /*!mcxk       50*/
/*                                                                    */        /*!mc        504*/
/*   Provide auxiliary c routines.                                    */        /*!mc        505*/
/*                                                                    */        /*!mc        506*/
#ifdef UNIX                                                                     /*!mc4c      250*/
#if defined(LINUX) || defined(SUN) || defined(DEC)                              /*!mcx        56*/
void xlnwid_(FDEF *scale)                                                       /*!mcx        57*/
#else                                                                           /*!mcx        58*/
void xlnwid (FDEF *scale)                                                       /*!mc4c      254*/
#endif                                                                          /*!cpp       153*/
#endif                                                                          /*!cpp       151*/
#ifndef UNIX                                                                    /*!mc4c      255*/
#ifdef DEC                                                                      /*!mc4c      256*/
void __stdcall XLNWID (FDEF *scale)                                             /*!mc4c      257*/
#endif                                                                          /*!cpp       155*/
#ifdef PCDOS                                                                    /*!mc4c      258*/
void xlnwid_(FDEF *scale)                                                       /*!mc4c      259*/
#endif                                                                          /*!cpp       156*/
#ifdef UNICOS                                                                   /*!mc4c      260*/
void XLNWID (FDEF *scale)                                                       /*!mc4c      261*/
#endif                                                                          /*!cpp       157*/
#endif                                                                          /*!cpp       154*/
/* Set the line attributes, scale the line width.                     */        /*!mc        513*/
{                                                                               /*!mc        515*/
int   np;                                                                       /*!mc4b      139*/
np=cnint(*scale);                                                               /*!mc4b      140*/
XSetLineAttributes(display,gc,np,LineSolid,CapRound,JoinRound);                 /*!mc4b      141*/
}                                                                               /*!mc        517*/
void xevent (IDEF wait)                                                         /*!mc4c      262*/
/* Process the various events - wait specifies an event to wait for.  */        /*!mc        519*/
{                                                                               /*!mc        521*/
int   x,y;                                                                      /*!mc        522*/
int   length;                                                                   /*!mc4c2      91*/
char  buffer[10];                                                               /*!mc4c2      92*/
unsigned int mask;                                                              /*!mc        523*/
switch (wait)                                                                   /*!mc        524*/
 { case 0:                                                                      /*!mc        525*/
/* Don't wait for any specific event - process all events.            */        /*!mc        526*/
      while (1)                                                                 /*!mc        527*/
      {  if(!XCheckWindowEvent(display,win,event_mask,&report))return;          /*!mc        528*/
         switch (report.type)                                                   /*!mc        529*/
         {  case ConfigureNotify:                                               /*!mc        530*/
               width=report.xconfigure.width;                                   /*!mc        531*/
               height=report.xconfigure.height;                                 /*!mc        532*/
               break;                                                           /*!mc        533*/
            default:                                                            /*!mc4c      263*/
               break;                                                           /*!mc        536*/
         }                                                                      /*!mc        537*/
      }                                                                         /*!mc        538*/
   case 1:                                                                      /*!mc        540*/
/* Wait to process a ButtonPress event and get pointer location.      */        /*!mc        541*/
      XWindowEvent(display,win,ButtonPressMask,&report);                        /*!mc        542*/
      XQueryPointer(display,win,&root,&child,&x,&y,&pnt_x,&pnt_y,&mask);        /*!mc        543*/
      break;                                                                    /*!mc        544*/
   case 2:                                                                      /*!mc        545*/
/* Wait to process a ConfigureNotify event and get the window size.   */        /*!mc        546*/
      XWindowEvent(display,win,StructureNotifyMask,&report);                    /*!mc        547*/
      width=report.xconfigure.width;                                            /*!mc        548*/
      height=report.xconfigure.height;                                          /*!mc        549*/
      break;                                                                    /*!mc        550*/
   case 3:                                                                      /*!mc        551*/
/* Wait to process an Expose event.                                   */        /*!mc        552*/
      XWindowEvent(display,win,ExposureMask,&report);                           /*!mc        553*/
      break;                                                                    /*!mc4c2      93*/
   case 4:                                                                      /*!mc4c2      94*/
/* Process KeyPress events until a return is entered.                 */        /*!mc4c2      95*/
      while (1)                                                                 /*!mc4c2      96*/
      {  XWindowEvent(display,win,event_mask,&report);                          /*!mc4c2      97*/
         switch (report.type)                                                   /*!mc4c2      98*/
         {  case KeyPress:                                                      /*!mc4c2      99*/
               length=XLookupString(&report.xkey,buffer,1,&keysym,              /*!mc4c2     100*/
                                    &compose);                                  /*!mc4c2     101*/
               buffer[1]='\0';                                                  /*!mc4c2     102*/
/* Return when an ENTER or RETURN key is pressed.                     */        /*!mc4c2     103*/
               if((keysym == XK_Return) || (keysym == XK_KP_Enter) ||           /*!mc4c2     104*/
                  (keysym == XK_Linefeed))return;                               /*!mc4c2     105*/
/* Accept only standard characters. Sound the bell if buffer is full. */        /*!mc4c2     106*/
               else if(((keysym >= XK_KP_Space) && (keysym <= XK_KP_9))         /*!mc4c2     107*/
                   || ((keysym >= XK_space)&&(keysym <= XK_asciitilde)))        /*!mc4c2     108*/
                     { if((strlen(key_buf) + length) > key_len)                 /*!mc4c2     109*/
                          XBell(display,100);                                   /*!mc4c2     110*/
                       else                                                     /*!mc4c2     111*/
                          strcat(key_buf,buffer);                               /*!mc4c2     112*/
                     }                                                          /*!mc4c2     113*/
/* Sound the bell if a non-printable character is pressed.            */        /*!mc4c2     114*/
               else if(((keysym >= XK_Shift_L)&&(keysym <= XK_Hyper_R))         /*!mc4c2     115*/
                       || ((keysym >= XK_F1) && (keysym <= XK_F35)))            /*!mc4c2     116*/
                       XBell(display,100);                                      /*!mc4c2     117*/
/* Remove a character with DELETE. Sound bell when buffer is empty.   */        /*!mc4c2     118*/
               else if((keysym == XK_BackSpace)||(keysym <= XK_Delete))         /*!mc4c2     119*/
                     { if((length = strlen(key_buf)) > 0)                       /*!mc4c2     120*/
                          key_buf[length-1] = '\0';                             /*!mc4c2     121*/
                       else                                                     /*!mc4c2     122*/
                          XBell(display,100);                                   /*!mc4c2     123*/
                     }                                                          /*!mc4c2     124*/
/* Sound the bell if a non-printable character is pressed.            */        /*!mc4c2     125*/
               else XBell(display,100);                                         /*!mc4c2     126*/
/* Blank the screen and echo the buffer.                              */        /*!mc4c2     127*/
               XDrawImageString(display,win,gc,key_x,key_y,blanks,              /*!mc4c2     128*/
                                key_len);                                       /*!mc4c2     129*/
               XDrawString(display,win,gc,key_x,key_y,key_buf,                  /*!mc4c2     130*/
                           strlen(key_buf));                                    /*!mc4c2     131*/
        /*     XFlush(display);  */                                             /*!mc4c2     132*/
               break;                                                           /*!mc4c2     133*/
            case MappingNotify:                                                 /*!mc4c2     134*/
               XRefreshKeyboardMapping(&report.xmapping);                       /*!mc4c2     135*/
               break;                                                           /*!mc4c2     136*/
/* All other StructureNotifyMask events are thrown away here.         */        /*!mc4c2     137*/
            default:                                                            /*!mc4c2     138*/
               break;                                                           /*!mc4c2     139*/
         }                                                                      /*!mc4c2     140*/
      }                                                                         /*!mc4c2     141*/
 }                                                                              /*!mc        555*/
}                                                                               /*!mc        556*/
void xscfont ()                                                                 /*!mc4c      264*/
/* Load a font that matches the character height.                     */        /*!mc        558*/
{                                                                               /*!mc        559*/
char  ch_point[4],font[300];                                                    /*!mc        560*/
int   i;                                                                        /*!mc4b      142*/
ch_old=ch_pixel;                                                                /*!mc        562*/
/* First, attempt to load a scalable font.                            */        /*!mc        563*/
font[0]='\0';                                                                   /*!mc        564*/
sprintf(&ch_point[0],"%d",10*ch_pixel);                                         /*!mc        565*/
strcat(font,font_scl_beg);                                                      /*!mc        566*/
strcat(font,ch_point);                                                          /*!mc        567*/
strcat(font,font_scl_end);                                                      /*!mc        568*/
if((font_scl_info=XLoadQueryFont(display,font))!=NULL)                          /*!mc        569*/
 { XSetFont(display,gc,font_scl_info->fid);                                     /*!mc        570*/
   return;                                                                      /*!mc        571*/
 }                                                                              /*!mc        572*/
/* Next, attempt to load a scaled standard font (8,10,12,or 14 point).*/        /*!mc        573*/
font[0]='\0';                                                                   /*!mc        574*/
for(i=8; ch_pixel>=i+2&&i<=14; i=i+2);                                          /*!mc        575*/
sprintf(&ch_point[0],"%d",10*i);                                                /*!mc        576*/
strcat(font,font_std_beg);                                                      /*!mc        577*/
strcat(font,ch_point);                                                          /*!mc        578*/
strcat(font,font_std_end);                                                      /*!mc        579*/
if((font_scl_info=XLoadQueryFont(display,font))!=NULL)                          /*!mc        580*/
 { XSetFont(display,gc,font_scl_info->fid);                                     /*!mc        581*/
   return;                                                                      /*!mc        582*/
 }                                                                              /*!mc        583*/
/* Otherwise use the basic font.                                      */        /*!mc        584*/
XSetFont(display,gc,font_info->fid);                                            /*!mc        585*/
}                                                                               /*!mc        586*/
int cnint (float x)                                                             /*!mc4c      265*/
/* Provide nearest integer function.                                  */        /*!mc        589*/
{                                                                               /*!mc        591*/
return (int) (x+.5);                                                            /*!mc4b      144*/
}                                                                               /*!mc        593*/
#endif                                                                          /*!cpp        19*/
#ifdef UNICOS                                                                   /*!mc4b      147*/
void CGTENV (CDEF name,CDEF string)                                             /*!mc4c      297*/
{                                                                               /*!mc4b      149*/
extern char *getenv __((const char *_Name));                                    /*!mc4b      150*/
int   j;                                                                        /*!mc4b      151*/
char  *value;                                                                   /*!mc4b      152*/
if((value=getenv(_fcdtocp(name))) == '\0')return;                               /*!mc4b      153*/
j=strlen(value);                                                                /*!mc4b      154*/
if(j > 80)j=80;                                                                 /*!mc4b      155*/
strncpy(_fcdtocp(string),value,j);                                              /*!mc4b      156*/
}                                                                               /*!mc4b      157*/
#endif                                                                          /*!cpp       175*/
