#include <math.h>
#include <stdlib.h>
#include <GL/freeglut.h>
#include<stdio.h>

#define GL_SILENCE_DEPRECATION 1

#ifndef M_PI
#define M_PI 3.14159265
#endif

#define MAXMO 13
#define MAXMOSQ 200
#define FILM no
#define RCUT 4.0
#define RCUT2 3.3


#define COS(X)   cos( (X) * 3.14159/180.0 )
#define SIN(X)   sin( (X) * 3.14159/180.0 )

#define RED 1
#define WHITE 2
#define CYAN 3
static GLUquadricObj *quadObj;
static void myreshape();
static void resh();
static void init();
void mainljdt_(float *, float *, float *, float *, long *,long *,long *);
void mainljdtm_(float *, float *, float *,float *, float*, float *, float *, long *,long *,long *);

#define QUAD_OBJ_INIT() { if(!quadObj) initQuadObj(); }

static void
initQuadObj(void)
{
  quadObj = gluNewQuadric();
}



static GLfloat view_rotx = 0.0, view_roty = 0.0, view_rotz = 0.0,trans=-40.0;
static GLfloat persp1=5.0,perspa=45.0;
static GLint atom[MAXMO];
static GLint lien[MAXMOSQ];
static float x[MAXMO];
static float y[MAXMO];
static float z[MAXMO];
static float px[MAXMO];
static float py[MAXMO];
static float pz[MAXMO];
static float tin;
static long idum=0;
static long nst=0;
static long pstep=10;
static long mc=1;
static GLfloat angle = 0.0;

static GLuint limit;
static GLuint count = 1;
static GLuint countlien = 0;
static GLuint niter=MAXMO;

static GLfloat red[4] =
  {0.8, 0.1, 0.0, 1.0};
static GLfloat green[4] =
  {0.0, 0.8, 0.2, 1.0};
static GLfloat blue[4] =
  {0.2, 0.2, 1.0, 1.0};


typedef struct {
  GLfloat x;
  GLfloat  y;
  GLfloat z;
  GLuint np;
} point;
	

static point l[MAXMO]; 

static point lmo[MAXMO];
static int arrows=0; 
static int time=0; 
static int iniatom=0; 
static FILE *fglo; 


unsigned char rgb_colors[MAXMO][3];
unsigned char rgba_colors[MAXMO][4];

void addlien()
{
  countlien++;
  if(countlien>MAXMOSQ)
    {fprintf(stderr,"MAXMOSQ too small \n");
    exit(-1);
    }
}

void 
glutSolidCylinder(GLdouble base, GLdouble height,
		  GLint slices, GLint stacks)
{
  QUAD_OBJ_INIT();
  gluQuadricDrawStyle(quadObj, GLU_FILL);
  gluQuadricNormals(quadObj, GLU_SMOOTH);
  gluQuadricTexture(quadObj, GL_TRUE);
  /* If we ever changed/used the texture or orientation state
     of quadObj, we'd need to change it to the defaults here
     with gluQuadricTexture and/or gluQuadricOrientation. */
  gluCylinder(quadObj, base, base, height, slices, stacks);
}


void 
glutSolidSpher(GLdouble radius, GLint slices, GLint stacks)
{
  QUAD_OBJ_INIT();
  gluQuadricDrawStyle(quadObj, GLU_FILL);
  gluQuadricNormals(quadObj, GLU_SMOOTH);
  /* If we ever changed/used the texture or orientation state
     of quadObj, we'd need to change it to the defaults here
     with gluQuadricTexture and/or gluQuadricOrientation. */
  gluQuadricTexture(quadObj, GL_TRUE);
  gluSphere(quadObj, radius, slices, stacks);
}            




float dist(int i,int j)
{
  float d,dx,dy,dz;
  if(j!=0) {
    dx=(l+j)->x-(l+i)->x;
    dy=(l+j)->y-(l+i)->y;
    dz=(l+j)->z-(l+i)->z; 
  }
  else    
    {
      dx=(lmo+i)->x;
      dy=(lmo+i)->y;
      dz=(lmo+i)->z; 
    };
  
  d=dx*dx+dy*dy+dz*dz;
  d=sqrt(d); 
  return(d);
}

void dangle(int i, int j, float *theta, float *phi)
{
    
  float d,dx,dy,dz,ax,ay,az,s,x,y,z;
  if(j!=0) {
    dx=(l+j)->x-(l+i)->x;
    dy=(l+j)->y-(l+i)->y;
    dz=(l+j)->z-(l+i)->z; 
  }
  else     
    {
      dx=(lmo+i)->x;
      dy=(lmo+i)->y;
      dz=(lmo+i)->z; 
    };
  d=dx*dx+dy*dy+dz*dz;
  d=sqrt(d);
  dx=dx/d;
  dy=dy/d;
  dz=dz/d;
  *theta=180/M_PI*acos(dz);
  d=dx*dx+dy*dy;
  d=sqrt(d);
  d=d+0.0000001;
  x=dx/d;
  y=dy/d;
  *phi=180/M_PI*acos(x);
  if(y<0.0) *phi=-*phi;
  x=cos(*phi/180*M_PI)*sin(*theta/180*M_PI);
  y=sin(*phi/180*M_PI)*sin(*theta/180*M_PI);
  z=cos(*theta/180*M_PI);
  /*  fprintf(stderr,"%f %f %f %f %f %f\n",dx,dy,dz,x,y,z); */
}
  


  

static void
draw(void)
{ int i,j; float xx,yy,zz,x,y,z,theta,phi;
  
 glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

 glPushMatrix();
 glRotatef(view_rotx, 1.0, 0.0, 0.0);
 glRotatef(view_roty, 0.0, 1.0, 0.0);
 glRotatef(view_rotz, 0.0, 0.0, 1.0);

 /* draw the axes */
 /*    glIndexi(CYAN);
       glBegin(GL_LINES);
  
    
       glVertex3f(-100.0,0.0,0.0);
       glVertex3f(100.0,0.0,0.0);
  
       glVertex3f(00.0,-100.0,0.0);
       glVertex3f(00.0,100.0,0.0);
  
       glVertex3f(00.0,0.0,-100.0);
       glVertex3f(00.0,0.0,100.0);
  
  
       glEnd(); 
 */
 glPushMatrix();
 glCallList(atom[1]);   
 glPopMatrix(); 

 glutSwapBuffers();

 count++;
 if (count == -1) {
   exit(0);
 }
}

static void
idle(void)
{
  if(mc==1) {
    mainljdt_(&x,&y,&z,&tin,&idum,&nst,&pstep);}
  else {
    mainljdtm_(&x,&y,&z,&px,&py,&pz,&tin,&idum,&nst,&pstep);}

  init();
  glutPostRedisplay(); 
}

/* change view angle, exit upon ESC */
/* ARGSUSED1 */
static void
key(unsigned char k, int x, int y)
{
  char c;
  int i; FILE *f;
  switch (k) {
  case 't':
    tin *=1.01;
    printf("temp = %f\n",tin);
    break;
  case 'T':
    tin /= 1.01;
    if(tin<0.0) tin=0.0;
    printf("temp = %f\n",tin);
    break;
  case 'p':
    pstep+=1;
    printf("pstep = %l\n",pstep);
    break;
  case 'P':
    pstep -= 1;
    if(pstep<0) pstep=0;
    printf("pstep = %l\n",pstep);
    break;
  case 'm':
    if(mc==1)
      { mc=0; nst=0; tin*=1.0001;
      for(i=0;i<niter;i++) 
	{ 
	  px[i]=0.0;
	  py[i]=0.0;
	  pz[i]=0.0;
	} 
      }
    else 
      {
	mc=1; nst=0; tin*=1.0001;
      }
    printf("switch mc/md\n");
    break;
  case 'z':
    view_rotz +=1.0;
    break;
  case 'Z':
    view_rotz -= 1.0;
    break;
  case 'f' :
    trans+=1.0;
    break;
  case 'F' :
    trans-=1.0;
    break;
  case '+':
    perspa +=1.0;
    break;
  case '-':
    perspa -=1.0;
    break;
  case 'r' :
    fprintf(stderr,"relu\n");      
    break;
  case 27:  /* Escape */
    exit(0);
    break;
  default:
    return;
  }
  resh();
  glutPostRedisplay();
}

/* change view angle */
/* ARGSUSED1 */
static void
special(int k, int x, int y)
{
  switch (k) {
  case GLUT_KEY_UP:
    view_rotx += 1.0;
    break;
  case GLUT_KEY_DOWN:
    view_rotx -= 1.0;
    break;
  case GLUT_KEY_LEFT:
    view_roty += 1.0;
    break;
  case GLUT_KEY_RIGHT:
    view_roty -= 1.0;
    break;
  default:
    return;
  }
  resh();
  glutPostRedisplay();
}

/* new window size or exposure */
static void
myreshape(int width, int height)
{
  GLfloat h = (GLfloat) height / (GLfloat) width;

  glViewport(0, 0, (GLint) width, (GLint) height);
  glLoadIdentity(); 
  glMatrixMode(GL_PROJECTION); 
  glFrustum(-1.0, 1.0, -h, h, 5.0, 400.0); 
  /*  glOrtho(-6.0, 6.0, -6.0, 6.0, -6.0, 6.0); */
  /*  gluPerspective(perspa, 1.33, 0.1, 100.0);  */
  glMatrixMode(GL_MODELVIEW);  
  glLoadIdentity();
  glTranslatef(0.0, 0.0, trans);
}

void resh()
{
  
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(0.0, 0.0, trans);
}

void
crossprodm(GLfloat v1[3], GLfloat v2[3], GLfloat prod[3])
{
  GLfloat p[3];         /* in case prod == v1 or v2 */

  p[0] = v1[1] * v2[2] - v2[1] * v1[2];
  p[1] = v1[2] * v2[0] - v2[2] * v1[0];
  p[2] = v1[0] * v2[1] - v2[0] * v1[1];
  prod[0] = p[0];
  prod[1] = p[1];
  prod[2] = p[2];
}        
void
normalizem(GLfloat v[3])
{
  GLfloat d;

  d = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  if (d == 0.0) {
    
    v[0] = d = 1.0;
  }
  d = 1 / d;
  v[0] *= d;
  v[1] *= d;
  v[2] *= d;
}      
void triangle(int c, int i,int j,int k)
{
  GLfloat c0[3],n0[3], d1[3], d2[3],d3[3],ps,gr[3]; 
  printf(" triangle centre %i %i %i %i \n",c,i,j,k); 
  c0[0]=(l+c)->x;
  c0[1]=(l+c)->y;
  c0[2]=(l+c)->z;
  d1[0]=(l+i)->x;
  d1[1]=(l+i)->y;
  d1[2]=(l+i)->z;
  d2[0]=(l+j)->x;
  d2[1]=(l+j)->y;
  d2[2]=(l+j)->z;
  d3[0]=(l+k)->x;
  d3[1]=(l+k)->y;
  d3[2]=(l+k)->z;
  gr[0]=1.0/3.0*(d1[0]+d2[0]+d3[0]);
  gr[1]=1.0/3.0*(d1[1]+d2[1]+d3[1]);
  gr[2]=1.0/3.0*(d1[2]+d2[2]+d3[2]);
  gr[0]-=c0[0];
  gr[1]-=c0[1];
  gr[2]-=c0[2];
  crossprodm(d1, d2, n0); 
  ps=n0[0]*gr[0]+n0[1]*gr[1]+n0[2]*gr[2]; 
  if(ps<0.0) {
    n0[0]=-n0[0];
    n0[1]=-n0[1];
    n0[2]=-n0[2];
  }

  normalizem(n0);     
  glBegin(GL_TRIANGLES);
  glNormal3fv(n0); 
  glVertex3fv(d1);
  glVertex3fv(d2);
  glVertex3fv(d3);
  glEnd();  
}
	   

static void
init()
{ int i,j,k,ll,koct,oct[7]; float theta, phi; 
 static GLfloat pos[4] =
   {5.0, 5.0, 10.0, 0.0};
  
 static float lmodel_ambient[] = {0.5, 0.5, 0.5, 1.0};
 static float lmodel_twoside[] = {GL_TRUE};
 static float lmodel_local[] = {GL_TRUE};
	


 static float light0_ambient[] = {0.00, 0.00, 0.00, 1.0};
 static float light0_diffuse[] = {0.8, 0.8, 0.8, 1.0};
 static float light0_position[] = {1.0, 1.0, 1.0, 0.0};
 static float light0_specular[] = {0.8, 0.8, 0.8, 1.0};

 static float light1_ambient[] = {0.05, 0.05, 0.05, 1.0};
 static float light1_diffuse[] = {0.1, 0.1, 0.1, 1.0};
 static float light1_position[] = {-20.8660254, -20.8, -0.0, 0.0};
 static float light1_specular[] = {0.1, 0.1, 0.1, 1.0};

         

 GLfloat mat_ambient0[] = { 0.0, 0.3, 0.0, 1.0 };
 GLfloat mat_diffuse0[] = { 0.0, 0.8, 0.0, 1.0 };
 GLfloat mat_specular0[] = { 0.0, 0.8, 0.0, 1.0 };
 GLfloat low_shininess0[] = { 175.0 };
 GLfloat mat_emission0[] = {0.00, 0.05, 0.00, 1.0};

 GLfloat mat_ambient1[] = { 0.3, 0.0, 0.0, 1.0 };
 GLfloat mat_diffuse1[] = { 0.8, 0.0, 0.0, 1.0 };
 GLfloat mat_specular1[] = { 0.8, 0.0, 0.0, 1.0 };
 GLfloat low_shininess1[] = { 175.0 };
 GLfloat mat_emission1[] = {0.05, 0.00, 0.00, 1.0};

 GLfloat mat_ambient2[] = { 0.3, 0.3, 0.3, 0.5 };
 GLfloat mat_diffuse2[] = { 0.8, 0.8, 0.8, 0.5 };
 GLfloat mat_specular2[] = { 0.8, 0.8, 0.8, 0.5 };
 GLfloat low_shininess2[] = { 175.0 };
 GLfloat mat_emission2[] = {0.05, 0.05, 0.05, 0.5};

   	 
 glShadeModel(GL_SMOOTH);
 glClearColor(1.0f, 1.0f, 1.0f, 1.5f);
 glClearDepth(1.0f);
 glEnable(GL_DEPTH_TEST);
 glDepthFunc(GL_LEQUAL);
 glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);    

     

    
          
 glLightfv(GL_LIGHT0, GL_AMBIENT, light0_ambient);
 glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_diffuse);
 glLightfv(GL_LIGHT0, GL_SPECULAR, light0_specular);
 glLightfv(GL_LIGHT0, GL_POSITION, light0_position);
 glEnable(GL_LIGHT0);
 glLightfv(GL_LIGHT1, GL_AMBIENT, light0_ambient);
 glLightfv(GL_LIGHT1, GL_DIFFUSE, light0_diffuse);
 glLightfv(GL_LIGHT1, GL_SPECULAR, light0_specular);
 glLightfv(GL_LIGHT1, GL_POSITION, light0_position);
 glEnable(GL_LIGHT1);
 glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lmodel_local);
 glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside);
 glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
 glEnable(GL_LIGHTING);
	



 glShadeModel(GL_SMOOTH);
	
	
 glEnable(GL_BLEND);
 glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
 glEnable(GL_LINE_SMOOTH);
 glHint(GL_LINE_SMOOTH_HINT, GL_FASTEST);        




 glutSetColor(RED, 1.0, 0.0, 0.0);
 glutSetColor(WHITE, 1.0, 1.0, 1.0);
 glutSetColor(CYAN, 0.0, 1.0, 1.0);

 /* glBindTexture(GL_TEXTURE_2D, texture[1]); */ 




 /* make the atoms */


 for(i=0;i<niter;i++) {
   (l+i)->x=(GLfloat) x[i];
   (l+i)->y=(GLfloat) y[i];
   (l+i)->z=(GLfloat) z[i];
   (l+i)->np=9;}
        
        
 if(iniatom==1) 
   glDeleteLists(atom[1],1);

 if(iniatom==0) {
   iniatom=1;
 } 
 atom[1] = glGenLists(1);
 glNewList(atom[1], GL_COMPILE);


 for(i=0;i<niter;i++) {
   glPushMatrix();
    j=((l+i)->np)%3;
   if(j==0) {
     glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient0);
     glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission0);
     glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mat_diffuse0);
     glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular0);
     glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, low_shininess0);
   }
   else {
     glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient1);
     glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission1);
     glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mat_diffuse1);
     glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular1);
     glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, low_shininess1);
   }
   glTranslatef((l+i)->x,(l+i)->y,(l+i)->z);
   glutSolidSpher(0.15+0.4*(((l+i)->np)%3),16, 16); 
  
   /*  glutSolidSpher(0.15+0.4*(((l+i)->np)%3),4+j*4, 4+j*4); */ 
   /* glutSolidSpher(3.15+0.2*(((l+i)->np)%3),128, 128); */
   glPopMatrix();
 }

 glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient2);
 glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission2);
 glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mat_diffuse2);
 glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular2);
 glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, low_shininess2);

 
 for(i=0;i<niter;i++) {
   if(((l+i)->np)==9) {
     for(j=i+1;j<niter;j++) {
       if(((l+j)->np)==9) {
	 if(i!=j) {
	   if(dist(i,j)<1.5) {
	     glPushMatrix();   
	     dangle(i,j,&theta,&phi);
	     glTranslatef((l+i)->x,(l+i)->y,(l+i)->z);
	     glRotatef(phi, 0.0, 0.0, 1.0);
	     glRotatef(theta, 0.0, 1.0, 0.0);
	     glutSolidCylinder(0.03,dist(i,j),8,8);

	     glPopMatrix();
	   }}}}}}

 glEndList();


 glEnable(GL_NORMALIZE);

}
void 
visible(int vis)
{
  if (vis == GLUT_VISIBLE)
    glutIdleFunc(idle);
  else
    glutIdleFunc(NULL);
}

int main(int argc, char *argv[])
{ 
  glutInit(&argc, argv);




   
  glutInitDisplayMode( GLUT_RGBA | GLUT_DOUBLE |GLUT_DEPTH );


  glutInitWindowPosition(0, 0);
  glutInitWindowSize(600, 600);
  glutCreateWindow("mc lj");
  mainljdt_(&x,&y,&z,&tin,&idum,&nst,&pstep);
  init();

  glutDisplayFunc(draw);
  glutReshapeFunc(myreshape);
  glutKeyboardFunc(key);
  glutSpecialFunc(special);
  glutVisibilityFunc(visible);

  glutMainLoop();
  return 0;             /* ANSI C requires main to return int. */
}




















