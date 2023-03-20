#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

struct Point2D { int mx, my; };
struct Point3D { double mx,my,mz; unsigned char b,g,r; };

struct Randm{ int res, result; };

const double cx = 0.998629534755, sy = 0.052335956243;
int x_c = 1020, y_c = 740, lncol = 0, lncoV = 0, d_x = 100, d_y = 100, x_m = 100, y_m = 1;

char rand0( struct Randm* a )
{
    a->res = rand();
    a->result = (unsigned char)(a->res);
    return a->result;
}

double scale = 900.00;
void trnsCrd( struct Point3D *c1, struct Point2D *a1 )
{
    double x = ( 510.55 + ( ( ( c1->mx / ( -1.0 * ( c1->mz - scale ) ) ) ) * 300.0 ) ),
           y = ( 370.55 + ( ( ( c1->my / ( -1.0 * ( c1->mz - scale ) ) ) ) * 300.0 ) );

    a1->mx = (int)(x); a1->my = (int)(y);
}

void Rotate3Dy(struct Point3D *c, double const cosa, double const sina, int drctn)
{
    double ry = 0, rz = 0;
    if (drctn) { ry=(c->my*cosa)-(c->mz*sina); rz=(c->mz*cosa)+(c->my*sina); }
    else { ry=(c->my*cosa)+(c->mz*sina); rz=(c->mz*cosa)-(c->my*sina); }
    c->mz=rz; c->my=ry;
}

void Rotate3Dz(struct Point3D *c, double const cosa, double const sina)
{
    double rx = (c->mx*cosa)-(c->my*sina), ry = (c->my*cosa)+(c->mx*sina);
    c->mx=rx; c->my=ry;
}
void Rotate3Dx(struct Point3D *c, double const cosa, double const sina, int drctn)
{
    double rx = 0, rz = 0;
    if ( drctn ) { rx=(c->mx*cosa)+(c->mz*sina); rz=(c->mz*cosa)-(c->mx*sina); }
    else { rx=(c->mx*cosa)-(c->mz*sina); rz=(c->mz*cosa)+(c->mx*sina); }
    c->mz=rz; c->mx=rx;
}

void cBackgr(char *dt, double rad, int x, int y, unsigned char dens, unsigned char mask)
{	unsigned char tr, bp, gp, rp;
    double xc = 0.0, xs, yc = 1.0, rc = rad * rad;
    int pos = 0, k1 = 1, k2 = 1, cc = 1, cr = 1;

    while (yc>=0) { k2=1; if (cr) { xs = xc*xc; yc = rc-xs; yc = sqrt(yc); }
     else { xs = yc*yc; xc = rc-xs; xc = sqrt(xc); }
     if ((cr==1)&&((int)(xc)>=(int)(yc))) { cr = 0; yc = xc;}
     for (int b=1; b<=4; b++) { if ((xc>x)||(xc<0)) goto sk1;
      pos=(((x_c*(y+(k1*(int)(yc))))+(x+(k2*(int)(xc))))*4);
      if ((pos<=0)||(pos>=x_c*y_c*4)) goto sk1; if ((dt[pos+3]==mask)) goto sk1;

       if (x_c==2000) { unsigned char ma=dt[pos+3];

        tr=dt[pos+2]; if (dens>tr||(int)(tr-dens)<0)tr=dens; else tr &= dens;

        if ((ma&0b10000000)&&!(ma&0b01000000)) {
        if (lncol==0) { bp=tr-tr/4; gp=tr-tr/2; rp=tr-tr/3; }
        if (lncol==1) { bp=tr-tr/4; gp=tr; rp=tr-tr/2; }
        if (lncol==2) { bp=tr-tr/2; gp=tr-tr/3; rp=tr; } }

        if ((ma&0b01000000)&&!(ma&0b10000000)) {
        if (lncoV==0) { bp=tr-tr/2; gp=tr-tr/3; rp=tr-tr; }
        if (lncoV==1) { bp=1; gp=tr-tr/5; rp=tr+tr/5; }
        if (lncoV==2) { bp=tr; gp=tr-tr; rp=tr; } }

        tr=dt[pos+0]; if (dens>tr||(int)(tr+dens)<0) tr=dens; else tr &= dens;
        dt[pos+0]=bp;
        tr=dt[pos+1]; if (dens>tr||(int)(tr+dens)<0) tr=dens; else tr &= dens;
        dt[pos+1]=gp;
        tr=dt[pos+2]; if (dens>tr||(int)(tr-dens)<0)tr=dens; else tr &= dens;
        dt[pos+2]=rp;  dt[pos+3]=mask; goto sk1; } // end

    if (mask==99) { tr=dt[pos]; if (dens+tr>255) dt[pos]=255; else dt[pos]=dens+tr;
        tr=dt[pos+1];  if (dens+tr>255) dt[pos]=255; else dt[pos+1]=dens+tr;
        tr=dt[pos+2];  if (dens+tr>255) dt[pos]=255; else dt[pos+2]=dens+tr;
        dt[pos+3]=mask; goto sk1;}

      tr=dt[pos]; if (dens>tr) tr=0; else tr -= dens;
      dt[pos]=tr; dt[pos+1]=tr; dt[pos+2]=tr; dt[pos+3]=mask;

sk1: 	  if ((b%2!=0)) k2=-1; else k2=1;
      switch (b) { case 2: k1=-1; break; case 4: k1=1; break; }
     } if (cr) xc++; else yc--; }
}

void putStr(char *dtM, struct Point2D *dt2D, unsigned char pix)
{	unsigned char b = pix, g = pix, r = pix;
    for ( int y = -2; y <= 2; y++ ) { for ( int x = -2; x <= 2; x++ ) {
    int pos = ( ( 1020 * ( dt2D->my + y ) ) + ( dt2D->mx + x ) ) * 4;
    if ( ( pos > 1020 * 740 * 4 ) || ( pos < 0 ) ) goto no_;

    if (pix<((abs(x)+abs(y))*(pix/3))) b = 0; else b = pix-((abs(x)+abs(y))*(pix/3));
    if (pix<((abs(x)+abs(y))*(pix/3))) g = 0; else g = pix-((abs(x)+abs(y))*(pix/3));
    if (pix<((abs(x)+abs(y))*(pix/3))) r = 0; else r = pix-((abs(x)+abs(y))*(pix/3));

    if ((unsigned char)(dtM[pos+0]) + b > 236) b = 236; else b+=dtM[pos+0];
    if ((unsigned char)(dtM[pos+1]) + g > 236) g = 236; else g+=dtM[pos+1];
    if ((unsigned char)(dtM[pos+2]) + r > 236) r = 236; else r+=dtM[pos+2];

    dtM[pos+0]=b; dtM[pos+1]=g; dtM[pos+2]=r; dtM[pos+3]=99;
    no_: ; } }
}

void putDts2D(char *dtM, struct Point2D *dt2D, struct Point3D *dt3D)
{
    if ( dt3D->mz <= -255 ) { putStr(dtM, dt2D, 69);  return; }
    if ( dt3D->mz >= 255  ) { putStr(dtM, dt2D, 236); return; } //111
    if ( dt3D->mz <= 0 && dt3D->mz > -255 ) { putStr(dtM, dt2D, 101); return; } //154
    if ( dt3D->mz <= 255 && dt3D->mz >= 0 ) { putStr(dtM, dt2D, 169); return; }//236

}

int main()
{
    srand(13423); rand(); printf( "%i : randmax\n", RAND_MAX);

    j0_: ; char *dtI = malloc( sizeof( char[1501*1500*3] ) );
    if ( !dtI ) { printf( "Bad alloc: dtI\n" ); goto j0_; }

    const char* fname = "./img.ppm";
    FILE* iF = fopen( fname, "r+" ); if ( !iF ) printf( "Open file: img.ppm error...\n" );

    fseek( iF, 0x40, SEEK_SET ); fread( dtI, sizeof(*dtI), 1500*1500*3-1, iF );
    if ( feof(iF) || ferror(iF) ) printf("Error reading: unexpected end of file || something else.\n");

    fclose(iF);

    for (int n=0; n<=1500*1500*3; n+=3) {
    if ( (unsigned char)(dtI[n+0]) - 89 < 0 ) dtI[n+0]=0; else dtI[n+0]-=89;
    if ( (unsigned char)(dtI[n+1]) - 89 < 0 ) dtI[n+1]=0; else dtI[n+1]-=89;
    if ( (unsigned char)(dtI[n+2]) - 89 < 0 ) dtI[n+2]=0; else dtI[n+2]-=89; }

    for ( int y = 0; y <= 1500; y++ ) { for ( int a = 69, b = 0, x = 0; x <= 100; x++) {
    int p1 = ( ( y * 1500 ) + x ) * 3; unsigned char tr = 0;
    if ((unsigned char)(dtI[p1+0])-a<0) dtI[p1+0]=0; else dtI[p1+0]-=a;
    if ((unsigned char)(dtI[p1+1])-a<0) dtI[p1+1]=0; else dtI[p1+1]-=a;
    if ((unsigned char)(dtI[p1+2])-a<0) dtI[p1+2]=0; else dtI[p1+2]-=a;
    int p2 = ( ( y * 1500 ) + x + 1400 ) * 3;
    if ((unsigned char)(dtI[p2+0])-b<0) dtI[p2+0]=0; else dtI[p2+0]-=b;
    if ((unsigned char)(dtI[p2+1])-b<0) dtI[p2+1]=0; else dtI[p2+1]-=b;
    if ((unsigned char)(dtI[p2+2])-b<0) dtI[p2+2]=0; else dtI[p2+2]-=b;

    if ((unsigned char)(dtI[p1+0])+(unsigned char)(dtI[p2+0])>255)
    {dtI[p1+0]=(unsigned char)255;dtI[p2+0]=(unsigned char)255;} else {tr=dtI[p1+0];dtI[p1+0]+=dtI[p2+0];dtI[p2+0]+=tr;}
    if ((unsigned char)(dtI[p1+1])+(unsigned char)(dtI[p2+1])>255)
    {dtI[p1+1]=(unsigned char)255;dtI[p2+1]=(unsigned char)255;} else {tr=dtI[p1+1];dtI[p1+1]+=dtI[p2+1];dtI[p2+1]+=tr;}
    if ((unsigned char)(dtI[p1+2])+(unsigned char)(dtI[p2+2])>255)
    {dtI[p1+2]=(unsigned char)255;dtI[p2+2]=(unsigned char)255;} else {tr=dtI[p1+2];dtI[p1+2]+=dtI[p2+2];dtI[p2+2]+=tr;}
    if (a!=0) {a--; b++;} } }

    for ( int x = 0; x <= 1500; x++) { for ( int a = 69, b = 0, y = 0; y <= 100; y++ ) {
    int p1 = ( ( y * 1500 ) + x ) * 3; unsigned char tr = 0;
    if ((unsigned char)(dtI[p1+0])-a<0) dtI[p1+0]=0; else dtI[p1+0]-=a;
    if ((unsigned char)(dtI[p1+1])-a<0) dtI[p1+1]=0; else dtI[p1+1]-=a;
    if ((unsigned char)(dtI[p1+2])-a<0) dtI[p1+2]=0; else dtI[p1+2]-=a;
    int p2 = ( ( ( y + 1400 ) * 1500 ) + x ) * 3;
    if ((unsigned char)(dtI[p2+0])-b<0) dtI[p2+0]=0; else dtI[p2+0]-=b;
    if ((unsigned char)(dtI[p2+1])-b<0) dtI[p2+1]=0; else dtI[p2+1]-=b;
    if ((unsigned char)(dtI[p2+2])-b<0) dtI[p2+2]=0; else dtI[p2+2]-=b;

    if ((unsigned char)(dtI[p1+0])+(unsigned char)(dtI[p2+0])>255)
    {dtI[p1+0]=(unsigned char)255;dtI[p2+0]=(unsigned char)255;} else {tr=dtI[p1+0];dtI[p1+0]+=dtI[p2+0];dtI[p2+0]+=tr;}
    if ((unsigned char)(dtI[p1+1])+(unsigned char)(dtI[p2+1])>255)
    {dtI[p1+1]=(unsigned char)255;dtI[p2+1]=(unsigned char)255;} else {tr=dtI[p1+1];dtI[p1+1]+=dtI[p2+1];dtI[p2+1]+=tr;}
    if ((unsigned char)(dtI[p1+2])+(unsigned char)(dtI[p2+2])>255)
    {dtI[p1+2]=(unsigned char)255;dtI[p2+2]=(unsigned char)255;} else {tr=dtI[p1+2];dtI[p1+2]+=dtI[p2+2];dtI[p2+2]+=tr;}
    if (a!=0) {a--; b++;} } }

    struct Randm r; const double lncrc = 505.0; const int slen = 936;
    double *vx = malloc ( sizeof ( double[slen] ) );
    double *vy = malloc ( sizeof ( double[slen] ) );
    double *vz = malloc ( sizeof ( double[slen] ) );
    unsigned char crc9 = 103; double rad9 = 250.0;

    r.res = 1111;
    for ( int i = 0; i < slen; i++ ) { do_:
                 while ( r.res > lncrc+lncrc + 1 ) { rand0(&r); r.res &= 0b0000011111111111; } vx[i] = r.res - lncrc; r.res = 1111;
                 while ( r.res > lncrc+lncrc + 1 ) { rand0(&r); r.res &= 0b0000011111111111; } vy[i] = r.res - lncrc; r.res = 1111;
                 while ( r.res > lncrc+lncrc + 1 ) { rand0(&r); r.res &= 0b0000011111111111; } vz[i] = r.res - lncrc; r.res = 1111;
            if ( ( ( ( vx[i] * vx[i] ) + ( vy[i] * vy[i] ) ) > lncrc * lncrc ) ||
                 ( ( ( vx[i] * vx[i] ) + ( vz[i] * vz[i] ) ) > lncrc * lncrc ) ||
                 ( ( ( vz[i] * vz[i] ) + ( vy[i] * vy[i] ) ) > lncrc * lncrc ) ) goto do_; }

    Display *d = XOpenDisplay(0);
    XSetWindowAttributes swa;
    swa.event_mask = KeyPressMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask;

    if ( d ) { XEvent e0, e1;

    Window w = XCreateWindow( d, XDefaultRootWindow(d), 0, 0, 1018, 740, 0, CopyFromParent, CopyFromParent, CopyFromParent, 0b00100000000000, &swa);
    XStoreName( d, w, "'_-\0" );

    XMapWindow( d, w ); XFlush( d );

    XSizeHints hnt; hnt.flags = PMaxSize | PMinSize;
    hnt.min_width = 1020; hnt.min_height = 740; hnt.max_width = 1020; hnt.max_height = 740;
    XSetWMNormalHints( d, w, &hnt );
    Window root_r = XDefaultRootWindow(d);

    GC gc = XDefaultGC( d, 0 );
    XImage *Img = XGetImage( d, root_r, 1, 1, 1020, 740, AllPlanes, ZPixmap );

    char *dt = Img->data;

    struct Point3D dot3D; struct Point2D dot2D;
    int ximg = 0, yimg = 1400, bpress = 0;

    double cc1 = 10.0 * clock() / CLOCKS_PER_SEC, cc2 = cc1 + 1.1;


   do { double ccc = cc2 - cc1; if ( ccc > 0.78 ) {

    cc1 = 10.0 * clock() / CLOCKS_PER_SEC;

    int xx = 0, yy = 0;

    for ( int x = 0; x < 1020; x++ ) { for ( int y = 0; y < 740; y++ ) {
    xx = ximg + x; if ( xx >= 1500 ) xx -= 1400;
    int pA = ( ( ( y ) * 1020 ) + x ) * 4, pB = ( ( yimg * 1500 ) + xx ) * 3;
    dt[pA+0] = dtI[pB+0]; dt[pA+1] = dtI[pB+1]; dt[pA+2] = dtI[pB+2]; dt[pA+3] = 0; } }

    ximg += 8; if ( ximg >= 1400 ) ximg = 0;
    yimg -= 2; if ( yimg <= 0 ) yimg = 1400;

    unsigned char c9col = crc9; for ( double rd = 0.0; rd <= rad9; rd += 0.5 ) {
      cBackgr(dt, rd, 510, 370, c9col, 99); if ( (int)(rd) % 5 == 0 ) c9col--; }

    for ( int i = 0; i < slen; i++ ) { dot3D.mx = vx[i]; dot3D.my = vy[i]; dot3D.mz = vz[i];

        trnsCrd(&dot3D, &dot2D); putDts2D(dt, &dot2D, &dot3D);

        if ( !bpress ) { if ( abs(d_y) > 69 && abs(d_x) > 69 ) Rotate3Dz(&dot3D, cx, sy);

        if ( d_x > 69 ) Rotate3Dx(&dot3D, cx, sy, 0);  if ( d_x < -69 ) Rotate3Dx(&dot3D, cx, sy, 1);
        if ( d_y > 69 ) Rotate3Dy(&dot3D, cx, sy, 1);  if ( d_y < -69 ) Rotate3Dy(&dot3D, cx, sy, 0);

        } vx[i] = dot3D.mx; vy[i] = dot3D.my; vz[i] = dot3D.mz; }

    XPutImage( d, w, gc, Img, 0, 0, 0, 0, 1020, 740 );

   if( XCheckWindowEvent(d, w, ButtonReleaseMask|ButtonPressMask|ButtonMotionMask, &e0) ) {

       if ( e0.xbutton.type == ButtonRelease && bpress == 1 ) { bpress = 0;

       } else if ( e0.xbutton.type == ButtonPress ) {

           d_x = 510 - e0.xbutton.x; d_y = 370 - e0.xbutton.y;

           if ( abs(d_x) < 69 && abs(d_y) < 69 && !bpress ) bpress = 1;

           if ( ( e0.xbutton.button == 5 ) && ( scale < 2400.0 ) ) { scale += 25.0; crc9--; rad9 -= 3; }
           if ( ( e0.xbutton.button == 4 ) && ( scale > 900.00 ) ) { scale -= 25.0; crc9++; rad9 += 3; }

           int evnts=XPending(d); if ( ( e0.xbutton.button != 5 ) && ( e0.xbutton.button != 4 ) )
           while ( evnts > 0 )
           { XCheckWindowEvent(d, w, ButtonReleaseMask|ButtonPressMask|ButtonMotionMask, &e0); e0.type=0; evnts--; }
        }

    }

    XCheckWindowEvent( d, w, KeyPressMask, &e1 );

    if ( e1.type == KeyPress ) { if ( e1.xkey.keycode == 65 ) {
                XNextEvent(d, &e1); e1.xkey.keycode = 0; e1.type = 0; }
                                printf( "keycode: %i \n", e1.xkey.keycode ); }

    } else cc2 = 10.0 * clock() / CLOCKS_PER_SEC; }

    while ( e1.type != KeyRelease && e1.xkey.keycode != 9 );

    } XCloseDisplay(d);

    free ( dtI ); free ( vx ); free ( vy ); free ( vz );

    return 0;
}
