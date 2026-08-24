/*                copyright notice for mcnp version 4b
   this program was prepared by the regents of the university of
   california at los alamos national laboratory (the university) under
   contract number w-7405-eng-36 with the u.s. department of energy
   (doe).  the university has certain rights in the program pursuant to
   the contract and the program should not be copied or distributed
   outside your organization.  all rights in the program are reserved by
   the doe and the university.  neither the u.s. government nor the
   university makes any warranty, express or implied, or assumes any
   liability or responsibility for the use of this software.

------------------------------------------------------------------------
                                                                      */
#include "fmangle.h"
#ifdef UPPER_WITHOUT
#define F_XGOPWK XGOPWK
#define F_XGCLWK XGCLWK
#define F_XGACWK XGACWK
#define F_XGDAWK XGDAWK
#define F_XGCLRW XGCLRW
#define F_XGUWK XGUWK
#define F_XGPL XGPL
#define F_XGTX XGTX
#define F_XGSCHH XGSCHH
#define F_XGSCHU XGSCHU
#define F_XGSWN XGSWN
#define F_XGSWK XGSWK
#define F_XGRQLC XGRQLC
#define F_XGFA XGFA
#define F_XGSPLC XGSPLC
#define F_XGQCF XGQCF
#define F_XLNWID XLNWID
#define F_MALLOF MALLOF
#define F_REALLF REALLF
#endif
#ifdef UPPER_WITH
#define F_XGOPWK XGOPWK_
#define F_XGCLWK XGCLWK_
#define F_XGACWK XGACWK_
#define F_XGDAWK XGDAWK_
#define F_XGCLRW XGCLRW_
#define F_XGUWK XGUWK_
#define F_XGPL XGPL_
#define F_XGTX XGTX_
#define F_XGSCHH XGSCHH_
#define F_XGSCHU XGSCHU_
#define F_XGSWN XGSWN_
#define F_XGSWK XGSWK_
#define F_XGRQLC XGRQLC_
#define F_XGFA XGFA_
#define F_XGSPLC XGSPLC_
#define F_XGQCF XGQCF_
#define F_XLNWID XLNWID_
#define F_MALLOF MALLOF_
#define F_REALLF REALLF_
#endif
#ifdef LOWER_WITH
#define F_XGOPWK xgopwk_
#define F_XGCLWK xgclwk_
#define F_XGACWK xgacwk_
#define F_XGDAWK xgdawk_
#define F_XGCLRW xgclrw_
#define F_XGUWK xguwk_
#define F_XGPL xgpl_
#define F_XGTX xgtx_
#define F_XGSCHH xgschh_
#define F_XGSCHU xgschu_
#define F_XGSWN xgswn_
#define F_XGSWK xgswk_
#define F_XGRQLC xgrqlc_
#define F_XGFA xgfa_
#define F_XGSPLC xgsplc_
#define F_XGQCF xgqcf_
#define F_XLNWID xlnwid_
#define F_MALLOF mallof_
#define F_REALLF reallf_
#endif
#ifdef LOWER_WITHOUT
#define F_XGOPWK xgopwk
#define F_XGCLWK xgclwk
#define F_XGACWK xgacwk
#define F_XGDAWK xgdawk
#define F_XGCLRW xgclrw
#define F_XGUWK xguwk
#define F_XGPL xgpl
#define F_XGTX xgtx
#define F_XGSCHH xgschh
#define F_XGSCHU xgschu
#define F_XGSWN xgswn
#define F_XGSWK xgswk
#define F_XGRQLC xgrqlc
#define F_XGFA xgfa
#define F_XGSPLC xgsplc
#define F_XGQCF xgqcf
#define F_XLNWID xlnwid
#define F_MALLOF mallof
#define F_REALLF reallf
#endif
#if defined(UNIX) || defined(UNICOS) || defined(XLIB)
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#ifndef PCDOS
#include <sys/times.h>
#endif
#endif
#ifdef UNICOS
#include <sys/stat.h>
#include <fortran.h>
#endif
#ifdef XLIB
/*                                                                    */
/* Enable plotting with the XLIB window system.                       */
/*                                                                    */
/* Get the XLIB include files.                                        */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#ifdef PCDOS
/* Define a structure for use with Lahey FORTRAN and MetaWare C.      */
typedef struct
 { char  *text;
   long  leng;
 } Lahey_Char;
#endif
/* Define the icon bitmap - created by "bitmap filename 40x40".       */
#define icon_bitmap_width 40
#define icon_bitmap_height 40
static char icon_bitmap_bits[] = {
 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x07, 0xe0, 0x0f, 0x38, 0x00, 0x00,
 0x1c, 0x70, 0x20, 0xfe, 0x07, 0x43, 0x80, 0x31, 0x00, 0x80, 0x40, 0x00,
 0x22, 0xfe, 0x47, 0x40, 0x00, 0x3c, 0x00, 0x20, 0x40, 0xe0, 0x08, 0xfe,
 0x17, 0x7c, 0x80, 0x10, 0x00, 0x10, 0x83, 0x41, 0x10, 0x00, 0x88, 0x00,
 0x22, 0x20, 0x00, 0x88, 0x00, 0xe2, 0x20, 0x00, 0x48, 0x00, 0x7c, 0x20,
 0x00, 0x44, 0x00, 0x87, 0x41, 0x00, 0x44, 0x80, 0x04, 0x42, 0x00, 0x44,
 0x80, 0x04, 0x42, 0x00, 0x44, 0x40, 0x04, 0x44, 0x00, 0x84, 0x40, 0x02,
 0x44, 0x00, 0x84, 0x40, 0x02, 0x44, 0x00, 0x04, 0xc3, 0x01, 0x44, 0x00,
 0x08, 0x7c, 0x00, 0x24, 0x00, 0x08, 0x80, 0x00, 0x22, 0x00, 0x08, 0x80,
 0x00, 0x22, 0x00, 0x10, 0x00, 0x83, 0x11, 0x00, 0x10, 0x00, 0x7c, 0x10,
 0x00, 0x20, 0x00, 0x00, 0x08, 0x00, 0x40, 0x00, 0x00, 0x04, 0x00, 0x80,
 0x00, 0x00, 0x02, 0x00, 0x00, 0x03, 0x80, 0x01, 0x00, 0x00, 0x1c, 0x70,
 0x00, 0x00, 0x00, 0xe0, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 0x00, 0x00, 0x00, 0x00, 0x20, 0xe2, 0x23, 0xc4, 0x07, 0x60, 0x23, 0x60,
 0x44, 0x04, 0xa0, 0x22, 0xa0, 0x44, 0x04, 0x20, 0x22, 0xa0, 0xc4, 0x07,
 0x20, 0x22, 0x20, 0x45, 0x00, 0x20, 0x22, 0x20, 0x46, 0x00, 0x20, 0xe2,
 0x23, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
/* Define the XLIB variable structures.                               */
Colormap       cmap;
Cursor         cursor;
Display        *display;
GC             gc;
Pixmap         icon_pixmap;
Window         win,root,child;
XCharStruct    char_info;
XColor         exact,color_fore,color_back;
XEvent         report;
XFontStruct    *font_info, *font_scl_info;
XSetWindowAttributes attributes;
XSizeHints     *size_hints;
XTextProperty  windowName,iconName;
XVisualInfo    visual_info;
XWMHints       *wm_hints;
/* Define the global variables.                                       */
char  *cname[]={"purple","blue","green","yellow","pink",
                "light sky blue","orange","grey","light blue",
                "pale green","black","blue","red","green","purple",
                "yellow",""},
      *display_name=NULL,
      *font_bas[]={"-*-courier-bold-*-*-*-10-100-75-75-*-*-iso8859-1",
                   "-*-courier-*-*-*-*-*-*-75-75-*-*-iso8859-1",
                   "-*-*-*-*-*-*-*-*-75-75-*-*-iso8859-1",""},
      font_scl[]="-itc-courier-bold-*-*-*-*-0-75-75-*-*-iso8859-1",
      font_scl_beg[100], font_scl_end[100],
      font_std[]="-*-courier-bold-*-*-*-*-100-75-75-*-*-iso8859-1",
      font_std_beg[100], font_std_end[100],
      *icon_name="plot", *window_name="MCNP PLOT WINDOW";
int   border_width=10, ch_flag, ch_old, ch_pixel, color_flag,
      cursor_shape[]={XC_cross, XC_top_left_corner,
      XC_bottom_right_corner}, depth, display_height,
      display_height_mm, display_width, display_width_mm, height,
      pnt_x, pnt_y, res_x, res_y, scrnnm, width, win_flag, win_shape,
      xo=0, yo=0;
long  event_mask=ExposureMask|ButtonPressMask|StructureNotifyMask;
float ch_cm, xmax, xmin, ymax, ymin;
#endif
#ifdef UNIX
/* Provide a standard unix timing routine.                            */
#if defined(LINUX) && defined(G77)
float etime_(float a[2])
#else
float etime(a)
float a[2];
#endif
{
 static int first_call = 1;
 static float ticks;
 struct tms buffer;
 if(first_call){
   first_call = 0;
   ticks = (float)sysconf(_SC_CLK_TCK);
  }
 times(&buffer);
 a[0] = ((float)buffer.tms_utime)/ticks;
 a[1] = ((float)buffer.tms_stime)/ticks;
 return (a[0]+a[1]);
}
#endif
#if defined(LINUX) && defined(G77)
void idate_(int jd[3])
/* Get date for LINUX.                                                */
{
  struct tm * time_info;
  time_t timep;
  time(&timep);
  time_info=localtime(&timep);
  jd[0]=time_info->tm_mday;
  jd[1]=time_info->tm_mon;
  jd[2]=time_info->tm_year;
}
void itime_(int jd[3])
/* Get time for LINUX.                                                */
{
  struct tm * time_info;
  time_t timep;
  time(&timep);
  time_info=localtime(&timep);
  jd[0]=time_info->tm_hour;
  jd[1]=time_info->tm_min;
  jd[2]=time_info->tm_sec;
}
#include<signal.h>
lsignal_(int * snum, void (*handler)(int))
{
      signal(*snum,handler);
}
#endif
#ifdef XLIB
/*                                                                    */
/*   Provide routines that follow the gks structure.                  */
/*                                                                    */
F_XGOPWK (error_flag)
/* Connect to the server display and get its dimensions.              */
/* The display can be set using "setenv DISPLAY host_name:0".         */
int   *error_flag;
{
int   i;
/* Allocate needed memory and connect to the display.                 */
if(!(size_hints=XAllocSizeHints()))
 { printf("xlib:  unable to allocate needed memory.\n");
   return *error_flag=-1;
 }
if(!(wm_hints=XAllocWMHints()))
 { printf("xlib:  unable to allocate needed memory.\n");
   return *error_flag=-1;
 }
if((display=XOpenDisplay(display_name))==NULL)
 { printf("xlib:  unable to connect to server display.\n");
   return *error_flag=-2;
 }
/* Store the window and icon names.                                   */
if(XStringListToTextProperty(&window_name,1,&windowName)==0)
 { XCloseDisplay(display);
   printf("xlib: unable to perform structure allocation.\n");
   return *error_flag=-3;
 }
if(XStringListToTextProperty(&icon_name,1,&iconName)==0)
 { XCloseDisplay(display);
   printf("xlib: unable to perform structure allocation.\n");
   return *error_flag=-3;
 }
/* Load the basic font - search several representations for a match.  */
for(i=0; *font_bas[i] != '\0'; i++)
   if((font_info=XLoadQueryFont(display,font_bas[i]))!=NULL) break;
if(font_info==NULL)
 { XCloseDisplay(display);
   printf("xlib: unable to open a font file.\n");
   return *error_flag=-4;
 }
/* Get the screen number and display dimensions.                      */
scrnnm=DefaultScreen(display);
display_width=DisplayWidth(display,scrnnm);
display_width_mm=DisplayWidthMM(display,scrnnm);
display_height=DisplayHeight(display,scrnnm);
display_height_mm=DisplayHeightMM(display,scrnnm);
res_x=25.4*display_width/display_width_mm;
res_y=25.4*display_height/display_height_mm;
}
F_XGCLWK ()
/* Release font memory and disconnect from display and server.        */
{
XUnloadFont(display,font_info->fid);
XCloseDisplay(display);
}
F_XGACWK ()
/* Initialize all window parameters.                                  */
{
/* Note xo,yo is the window origin relative to upper lefthand corner. */
/* Set window height to min(80% display_height, display_height-yo).   */
height=cnint(0.8*display_height);
if(height>display_height-yo)height=display_height-yo;
/* Start with a rectangle viewport (scale factor in sub. tekdvr).     */
win_shape=0;
width=cnint(height*4./3.);
if(width>display_width-xo)
 { width=display_width-xo;
   height=cnint(width*3./4.);
 }
/* Create an unmapped subwindow of the root.                          */
win_flag=0;
win=XCreateSimpleWindow(display,RootWindow(display,scrnnm),xo,yo,width,
                      height,border_width,BlackPixel(display,scrnnm),
                      WhitePixel(display,scrnnm));
/* Create pixel map for icon of plot window.                          */
icon_pixmap=XCreateBitmapFromData(display,win,icon_bitmap_bits,
                                  icon_bitmap_width,icon_bitmap_height);
/* Initialize various window manager properties.                      */
size_hints->flags=PPosition|PSize|PMinSize;
size_hints->min_width=cnint(width/2.0);
size_hints->min_height=cnint(height/2.0);
wm_hints->initial_state=NormalState;
wm_hints->input=True;
wm_hints->icon_pixmap=icon_pixmap;
wm_hints->flags=StateHint|IconPixmapHint|InputHint;
XSetWMProperties(display,win,&windowName,&iconName,NULL,0,size_hints,
                 wm_hints,NULL);
/* Select the event types - most are not currently used.              */
XSelectInput(display,win,event_mask);
/* Create the graphics context.                                       */
gc=XCreateGC(display,win,0,NULL);
XSetForeground(display,gc,BlackPixel(display,scrnnm));
XSetBackground(display,gc,WhitePixel(display,scrnnm));
if(XDoesBackingStore(DefaultScreenOfDisplay(display)) != NotUseful)
 { attributes.backing_store=Always;
   XChangeWindowAttributes(display,win,CWBackingStore,&attributes);
 }
/* Initialize the fonts.                                              */
xgstxfp ();
ch_flag=0;
ch_old=0;
}
F_XGDAWK ()
/* Release window memory and remove plot window.                      */
{
if(font_scl_info!=NULL)XUnloadFont(display,font_scl_info->fid);
XFreeGC(display,gc);
XDestroyWindow(display,win);
XFlush(display);
}
F_XGCLRW ()
/* Clear the window.                                                  */
{
XClearWindow(display,win);
}
F_XGUWK ()
/* Sound the bell and process any queued events.                      */
{
XBell(display,100);
XFlush(display);
}
F_XGPL (x1,y1,x2,y2)
/* Draw a line from point x1,y1 to x2,y2.                             */
float *x1,*y1,*x2,*y2;
{
int   xa,ya,xb,yb;
xa=cnint(width*(*x1-xmin)/(xmax-xmin));
xb=cnint(width*(*x2-xmin)/(xmax-xmin));
ya=cnint(height*(ymax-*y1)/(ymax-ymin));
yb=cnint(height*(ymax-*y2)/(ymax-ymin));
XDrawLine(display,win,gc,xa,ya,xb,yb);
}
F_XGTX (str,length,x,y)
/* Draw a character string starting at point x,y.                     */
#if defined(UNICOS)
_fcd  str;
#elif defined(PCDOS)
Lahey_Char *str;
#else
char  *str;
#endif
int   *length;
float *x,*y;
{
char  *ch_str;
int   xa,ya,i;
#if defined(UNICOS)
ch_str=_fcdtocp(str);
#elif defined(PCDOS)
ch_str=str->text;
#else
ch_str=str;
#endif
if(ch_pixel!=ch_old)xscfont();
xa=cnint(width*(*x-xmin)/(xmax-xmin));
ya=cnint(height*(ymax-*y)/(ymax-ymin));
if(ch_flag)for(i=*length-1; i>=0; i--)
         {  XDrawImageString(display,win,gc,xa,ya,ch_str+i,1);
              ya=ya-font_scl_info->ascent-font_scl_info->descent;
           }
else XDrawImageString(display,win,gc,xa,ya,ch_str,*length);
}
xgstxfp ()
/* Initialize the scalable, standard, and basic fonts.                */
/* This is called by xgacwk() rather than sub. gstxfp in MCNP which   */
/* is positioned poorly in sub. gxon.                                 */
{
int   i,j,field,end;
/* Setup the scalable font - insert the display resolutions.          */
for(i=j=field=end=0; field<=14&&font_scl[i]!='\0'; i++)
 { if(end)font_scl_end[j++]=font_scl[i];
   else font_scl_beg[j++]=font_scl[i];
   if(font_scl[i]=='-')
   {  field++;
      switch (field)
      {  case 8:
            font_scl_beg[j]='\0';
            while(font_scl[i+1]!='-')i++;
            end=1;
            j=0;
            break;
         case 9:
            sprintf(&font_scl_end[j],"%d",res_x);
            while(font_scl_end[j]!='\0')j++;
            while(font_scl[i+1]!='-')i++;
            break;
         case 10:
            sprintf(&font_scl_end[j],"%d",res_y);
            while(font_scl_end[j]!='\0')j++;
            while(font_scl[i+1]!='-')i++;
            break;
      }
   }
 }
font_scl_end[j]='\0';
/* Setup the standard font for scaling.                               */
for(i=j=field=end=0; field<=14&&font_std[i]!='\0'; i++)
 { if(end)font_std_end[j++]=font_std[i];
   else font_std_beg[j++]=font_std[i];
   if(font_std[i]=='-')
   {  field++;
      switch (field)
      {  case 8:
            font_std_beg[j]='\0';
            while(font_std[i+1]!='-')i++;
            end=1;
            j=0;
            break;
      }
   }
 }
font_std_end[j]='\0';
/* Load the basic font.                                               */
XSetFont(display,gc,font_info->fid);
}
F_XGSCHH (ch)
/* Set the character height in pixels.                                */
float *ch;
{
ch_cm=*ch;
if(ch_flag)ch_pixel=ch_cm*width/(xmax-xmin);
else ch_pixel=ch_cm*height/(ymax-ymin);
}
F_XGSCHU (x,y)
/* Set the character up vector - ch_flag=1 for rotated text.          */
float *x,*y;
{
float xa,ya;
xa=*x;
ya=*y;
if(xa < 0.)xa=-1.*xa;
if(ya < 0.)ya=-1.*ya;
if(xa > ya)
ch_flag=1;
else
ch_flag=0;
}
F_XGSWN (x1,x2,y1,y2)
/* Save the window size.                                              */
float *x1,*x2,*y1,*y2;
{
xmin=*x1;
xmax=*x2;
ymin=*y1;
ymax=*y2;
}
F_XGSWK (x1,x2,y1,y2)
/* Draw the window on the display.                                    */
float *x1,*x2,*y1,*y2;
{
float scale;
int   pre_width;
long int i;
/* Check if the window has been resized.                              */
xevent(0);
pre_width=width;
/* Scale = 1.25 for a rectangular window and 1.00 for for square.     */
scale=(*x2-*x1)/(*y2-*y1);
win_shape=0;
if(scale <= 1.01)win_shape=1;
width=cnint(height*scale);
if(width>display_width-xo)
 { width=display_width-xo;
   height=cnint(width/scale);
 }
if(width != pre_width)
 { XResizeWindow(display,win,width,height);
   xevent(2);
 }
/* Raise the window on the display.                                   */
XMapRaised(display,win);
/* First time wait for an expose event - process all other events.    */
if(!win_flag)xevent(3);
win_flag=1;
xevent(0);
for(i=0; i<1000000; i++);
}
F_XGRQLC (flag,x,y)
/* Get pointer position.                                              */
int   *flag;
float *x,*y;
{
long int i;
int   x1,x2,y1,y2;
/* Set cursor, flush previous ButtonPress events, and sound the bell. */
cursor=XCreateFontCursor(display, cursor_shape[*flag-1]);
XDefineCursor(display,win,cursor);
if(color_flag)XRecolorCursor(display,cursor,&color_fore,&color_back);
xevent(0);
XBell(display,100);
/* Wait for user input via any mouse button.                          */
xevent(1);
/* Place the appropriate cross-hair at the chosen pixel.              */
if(*flag==1)
 { x1=pnt_x-height/80;
   x2=pnt_x+height/80;
   y1=pnt_y-height/80;
   y2=pnt_y+height/80;
 }
else
 { x1=xo+width*(1-win_shape)/4;
   x2=width;
   y1=yo;
   y2=height;
 }
XDrawLine(display,win,gc,pnt_x,y1,pnt_x,y2);
XDrawLine(display,win,gc,x1,pnt_y,x2,pnt_y);
XFlush(display);
for(i=0; i<1000000; i++);
/* Return the pixel location.                                         */
*x=xmin+pnt_x*(xmax-xmin)/width;
*y=ymax-pnt_y*(ymax-ymin)/height;
if(*flag!=2)XUndefineCursor(display,win);
}
F_XGFA (x1,y1,x2,y2)
/* Fill the rectangular area with the initialized color.              */
float *x1,*y1,*x2,*y2;
{
int   xa,ya,xb,yb;
xa=cnint(width*(*x1-xmin)/(xmax-xmin));
xb=cnint(width*(*x2-xmin)/(xmax-xmin));
ya=cnint(height*(ymax-*y1)/(ymax-ymin));
yb=cnint(height*(ymax-*y2)/(ymax-ymin));
XFillRectangle(display,win,gc,xa,ya,xb-xa,yb-ya);
}
F_XGSPLC (ic)
/* Set the line color.                                                */
int   *ic;
{
if(XAllocNamedColor(display,cmap,cname[*ic-1],&color_fore,&exact))
  XSetForeground(display,gc,color_fore.pixel);
}
F_XGQCF (type,nc)
/* Set the color flag if a color display is used.                     */
int   *type,*nc;
{
int   i,j;
color_flag=0;
if((depth=DefaultDepth(display,scrnnm)) == 1)return;
cmap=DefaultColormap(display,scrnnm);
for(i=5; i>=StaticColor; i--)
   if(XMatchVisualInfo(display,scrnnm,depth,i,&visual_info))
   {  color_flag=1;
      XAllocNamedColor(display,cmap,"white",&color_back,&exact);
      for(j=0; *cname[j] != '\0'; j++);
      return *type=1, *nc=j;
   }
}
/*                                                                    */
/*   Provide auxiliary c routines.                                    */
/*                                                                    */
F_XLNWID (scale)
/* Set the line attributes, scale the line width.                     */
float *scale;
{
int   np;
np=cnint(*scale);
XSetLineAttributes(display,gc,np,LineSolid,CapRound,JoinRound);
}
xevent (wait)
/* Process the various events - wait specifies an event to wait for.  */
int   wait;
{
int   x,y;
unsigned int mask;
switch (wait)
 { case 0:
/* Don't wait for any specific event - process all events.            */
      while (1)
      {  if(!XCheckWindowEvent(display,win,event_mask,&report))return;
         switch (report.type)
         {  case ConfigureNotify:
               width=report.xconfigure.width;
               height=report.xconfigure.height;
               break;
            case Expose:
            case ButtonPress:
               break;
         }
      }
      break;
   case 1:
/* Wait to process a ButtonPress event and get pointer location.      */
      XWindowEvent(display,win,ButtonPressMask,&report);
      XQueryPointer(display,win,&root,&child,&x,&y,&pnt_x,&pnt_y,&mask);
      break;
   case 2:
/* Wait to process a ConfigureNotify event and get the window size.   */
      XWindowEvent(display,win,StructureNotifyMask,&report);
      width=report.xconfigure.width;
      height=report.xconfigure.height;
      break;
   case 3:
/* Wait to process an Expose event.                                   */
      XWindowEvent(display,win,ExposureMask,&report);
      break;
 }
}
xscfont ()
/* Load a font that matches the character height.                     */
{
char  ch_point[4],font[300];
int   i;
ch_old=ch_pixel;
/* First, attempt to load a scalable font.                            */
font[0]='\0';
sprintf(&ch_point[0],"%d",10*ch_pixel);
strcat(font,font_scl_beg);
strcat(font,ch_point);
strcat(font,font_scl_end);
if((font_scl_info=XLoadQueryFont(display,font))!=NULL)
 { XSetFont(display,gc,font_scl_info->fid);
   return;
 }
/* Next, attempt to load a scaled standard font (8,10,12,or 14 point).*/
font[0]='\0';
for(i=8; ch_pixel>=i+2&&i<=14; i=i+2);
sprintf(&ch_point[0],"%d",10*i);
strcat(font,font_std_beg);
strcat(font,ch_point);
strcat(font,font_std_end);
if((font_scl_info=XLoadQueryFont(display,font))!=NULL)
 { XSetFont(display,gc,font_scl_info->fid);
   return;
 }
/* Otherwise use the basic font.                                      */
XSetFont(display,gc,font_info->fid);
}
cnint (x)
/* Provide nearest integer function.                                  */
float x;
{
return (int) (x+.5);
}
#endif
#ifdef POINTERS
F_MALLOF (p,bytes)
/* Allocate memory to pointer p.                                      */
long int  *p; int  *bytes;
{
*p=malloc(*bytes);
}
F_REALLF (p,bytes)
/* Adjust memory allocated to pointer p.                              */
long int  *p; int  *bytes;
{
*p=realloc(*p,*bytes);
}
#endif
#ifdef UNICOS
CGTENV (_fcd name,_fcd string)
{
extern char *getenv __((const char *_Name));
int   j;
char  *value;
if((value=getenv(_fcdtocp(name))) == '\0')return;
j=strlen(value);
if(j > 80)j=80;
strncpy(_fcdtocp(string),value,j);
}
#endif
