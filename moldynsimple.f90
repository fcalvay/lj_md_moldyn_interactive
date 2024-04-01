!.....................................................................
!                   module  operateur
!
! this module creates the type vecteur and functions using this type     

! partir d'un systeme plus ordonne
! tracer de moins en moins frequemment

module operateur

  implicit none

  type vecteur
     real  :: x,y,z
  end type vecteur

  interface operator (+)
     module procedure somme
  end interface

  interface operator (-)
     module procedure soustraction
  end interface

  interface operator (*)
     module procedure multiplication
  end interface

contains

  function somme (p1,p2)
    type (vecteur),intent (in) :: p1,p2
    type (vecteur) :: somme
    somme=vecteur(p1%x+p2%x,p1%y+p2%y,p1%z+p2%z)
  end function somme

  function soustraction (p1,p2)
    type (vecteur),intent (in) :: p1,p2
    type (vecteur) :: soustraction
    soustraction=vecteur(p1%x-p2%x,p1%y-p2%y,p1%z-p2%z)
  end function soustraction

  function multiplication (a,p)
    type (vecteur),intent (in) :: p
    real ,intent(in) :: a
    type (vecteur) :: multiplication
    multiplication=vecteur(a*p%x,a*p%y,a*p%z)
  end function multiplication

  function norm (p)
    type (vecteur),intent (in) :: p
    real :: norm
    norm=p%x*p%x+p%y*p%y+p%z*p%z
  end function norm

  function prod (p1,p2)
    type (vecteur),intent(in) :: p1,p2
    type (vecteur) :: prod
    prod=vecteur(p1%y*p2%z-p1%z*p2%y, &
         p1%z*p2%x-p1%x*p2%z, &
         p1%x*p2%y-p1%y*p2%x)
  end function prod

  function init()
    type (vecteur) :: init
    init=vecteur(0,0,0)
  end function init
  function aint_v(p)
    type (vecteur), intent (in) :: p
    type (vecteur) :: aint_v
    aint_v=vecteur(0.0,0.0,0.0)
  end function aint_v


end module operateur


!...............................................................................
!                     main program


subroutine mainljdtm(x,y,z,px,py,pz,temp,idum,ist,pstep)

  use operateur
  include 'var.inc'


  type (vecteur) :: dij
  type (vecteur), dimension (n) :: cl,cpl,fl,cli
  integer pstep
  dimension x(n),y(n),z(n),px(n),py(n),pz(n)   ! coordinates     
  real :: v, e,ek
  integer :: i

  itflag=0
  if(temp.ne.tin) then
     tin=temp
     itflag=1
     ist=0
     r=0.0
     r2=0.0
     eav=0.0
     eav2=0.0
  endif


  do i=1,n
     cl(i)%x  = x(i)
     cl(i)%y  = y(i)
     cl(i)%z  = z(i)
     cpl(i)%x = px(i) ! add temp
     cpl(i)%y = py(i)
     cpl(i)%z = pz(i)
  enddo



  do it=1,pstep
     ist=ist+1
     call prec(cl,cpl,fl,v)
     call en(cl,cpl,v,e,ek)
     write(6,*) 'moldyn : pstep, temp,etot,ek,tk',pstep,tin,e,ek,2.0/3.0*ek/n
     do i=1,n
        x(i)= cl(i)%x
        y(i)= cl(i)%y 
        z(i)= cl(i)%z 
        px(i)= cpl(i)%x
        py(i)= cpl(i)%y 
        pz(i)= cpl(i)%z 
     enddo
     do i=1,n
        do j=1,i-1
           r2i=(x(i)-x(j))**2+ (y(i)-y(j))**2+(z(i)-z(j))**2
           r(i,j)=r(i,j)+sqrt(r2i)
           r2(i,j)=r2(i,j)+r2i
        enddo
     enddo


     write(6,*) 'mc',ist,tin,pstep,e
     tin=temp

     eav=eav+e 
     eav2=eav2+e*e 
  enddo
  delta=0.0

  do i=1,n
     do j=1,i-1
        rij=r(i,j)/ist
        r2ij=r2(i,j)/ist
        delta=delta+sqrt(-rij*rij+r2ij)/rij 
     enddo
  enddo

  cv=(eav2/ist-eav*eav/ist/ist)/temp/temp
  write(16,*)temp,delta/n/(n-1),cv,eav/ist
  write(6,*) 'mct, delta, cv, eav',temp,delta/n/(n-1),cv,eav/ist
  call flush(16)
  call flush(6)







!!$
!!$! test of evolution to a Boltzmann distribution
!!$
!!$  
!!$  ! main loop
!!$  
!!$  !cl=cli
!!$ ! cpl=cpli
!!$ ! v=0.0
!!$  !iobs=10
!!$  do it=0,nmax
!!$     if(mod(it,iobs).eq.0) then
!!$           call calchisto(cl,cpl,it)
!!$     endif
!!$     write(6,*) it,iobs
!!$     call prec(cl,cpl,fl,v)
!!$     call en(cl,cpl,v,e)
!!$     write(ifenergy,'(4(g15.7))') it*dt1,e,e-v,v 
!!$  end do
!!$
!!$! end of main loop
!!$ 
!!$ ! call jimd_finalize_
!!$  



end subroutine mainljdtm


function distance(a,b)
  use operateur
  include 'var.inc'
  type (vecteur), dimension (ng) , intent(in) :: a,b
  d=0.0
  do i=1,ng
     d=d+norm(a(i)-b(i))
  enddo
  distance=sqrt(d)
  return
end function distance


!................................................................................
! this subroutine calculates the energy

subroutine en(cl,cpl,v,e,ek)

  use operateur
  include 'var.inc'

  type (vecteur), dimension (ng) , intent(in) :: cl,  cpl
  real,intent (inout) :: v, e,ek



  ! we accumulate locally the p**2/(2m)

  ek=0.0

  do i=1,ng

     ek=ek+norm(cpl(i))

  enddo


  ek=ek/2.0



  e=ek+v

  return

end subroutine en


!.................................................................................
! this subroutine calculate the forces and the potential (F=-grad(V))

subroutine calcforce(cl,fl,v)

  use operateur
  include 'var.inc'

  type (vecteur), dimension (ng),intent(in) :: cl
  type (vecteur), dimension (ng),intent(out) :: fl

  type (vecteur) :: fij,clc
  real,intent (inout) :: v
  type (vecteur), dimension (ng) :: fs, fr

  ! compute local force

  do i=1,ng

     fl(i)=init()

  enddo

  v=0.0


  do i=1,ng
     do j=1,ng

        !conditions for periodic box

        clc=cl(i)-cl(j)
        !        clc=clc-box*aint_v(box2inv*clc)

        rij=norm(clc)

        rij2 = rij  * rij
        rij3 = rij2 * rij
        rij4 = rij2 * rij2
        rij6 = rij4 * rij2
        rij7 = rij6 * rij


        if ((rij>=0.00001)) then 

           fij   = (-4.0/rij7+2.0/rij4)*clc
           fl(i) = fl(i)+fij
           v     = v+4.0/rij6-4.0/rij3

        endif

     enddo
  enddo

  v=v/2.0

  do i=1,ng

     fl(i)=(-12.)*fl(i)

  enddo


  return

end subroutine calcforce


!...................................................................................
! this subroutine uses the predictor-corrector method

subroutine prec(cl,cpl,fl,v)

  use operateur
  include 'var.inc'

  type (vecteur),dimension (ng),intent(inout) :: cl, cpl,  fl
  type (vecteur), dimension (ng) :: clt, cplt, flt
  real,intent (inout) :: v



  ! predictor step

  call calcforce(cl,fl,v) 

  do i=1,ng

     clt(i)  = cl(i)
     flt(i)  = fl(i)
     cplt(i) = cpl(i)

     cl(i)  = cl(i)  + dt1*cpl(i)
     cpl(i) = cpl(i) + dt1*fl(i)

  enddo

  ! corrector step

  call calcforce(cl,fl,v)

  do i=1,ng

     fl(i)  = 0.5*(flt(i)+fl(i))
     cpl(i) = 0.5*(cplt(i)+cpl(i))
     cl(i)  = clt(i)  + dt1*cpl(i)
     cpl(i) = cplt(i) + dt1*fl(i)

     ! conditions for periodic box

     !     cl(i)=cl(i)-box*aint_v(box2inv*cl(i))

  enddo

  return

end subroutine prec
subroutine precb(cl,cpl,fl,v)

  use operateur
  include 'var.inc'

  type (vecteur),dimension (ng),intent(inout) :: cl, cpl,  fl
  type (vecteur), dimension (ng) :: clt, cplt, flt
  real,intent (inout) :: v



  ! predictor step with the last force from the forward step



  do i=1,ng

     clt(i)  = cl(i)
     flt(i)  = fl(i)
     cplt(i) = cpl(i)

     cl(i)  = cl(i)  - dt1*cpl(i)
     cpl(i) = cpl(i) - dt1*fl(i)

  enddo

  ! corrector step

  call calcforce(cl,fl,v)

  do i=1,ng

     fl(i)  = 0.5*(flt(i)+fl(i))
     cpl(i) = 0.5*(cplt(i)+cpl(i))
     cl(i)  = clt(i)  - dt1*cpl(i)
     cpl(i) = cplt(i) - dt1*fl(i)

     ! conditions for periodic box

     !    cl(i)=cl(i)-box*aint_v(box2inv*cl(i))

  enddo
  call calcforce(cl,fl,v) 
  return

end subroutine precb




!.......................................................................................
! this subroutine calculate data for the program histo

subroutine calchisto(cl,cp,it)

  use operateur
  include 'var.inc'
  character *20 chaine

  type (vecteur), dimension(ng), intent (in) :: cl,cp

  integer,dimension(0:nbin) :: histo

  do i=0,nbin
     histo(i)=0
  enddo


  write(chaine,*) 'histo',it 
  ifhisto=60
  !open(unit=ifhisto,file=chaine)
  open(unit=ifhisto+1,file='histo')

  do i=1,ng
     p=norm(cp(i)) 
     !      if(sqrt(norm(cl(i))).lt.rradius) then
     if(p.gt.pmax) pmax=p
     !      endif
  enddo
  pmax=3.0
  pmax=pmax+1e-9
  sbin=pmax/nbin
  do i=1,ng
     p=norm(cp(i)) 
     !      if(sqrt(norm(cl(i))).lt.rradius) then
     if(p/sbin.le.nbin) then
        ih=p/sbin
        histo(ih)=histo(ih)+1
     endif
  enddo
  do i=0,nbin
     !    write(ifhisto,*) i*sbin,histo(i)
     write(ifhisto+1,*) it*dt1,i*sbin,histo(i)
  enddo
  !close(unit=ifhisto)
  write(ifhisto+1,*) 
  write(ifhisto+1,*) 

  return
end subroutine calchisto

