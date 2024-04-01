c     
c     Metropolis evolution of a Lennard Jones 13 atom cluster
c     
c     
c     
      subroutine mainljdt(x,y,z,temp,idum,ist,pstep)
      integer pstep
!     computes one step of lj
      include 'lj.inc'
     
      itflag=0
      if(temp.ne.tin) then
         tin=temp
         itflag=1
         ist=0
      endif
      if(idum.eq.0) then
         call init(x,y,z,h,temp)
         idum=2005
         write(6,*) 'init***'
         do i=1,n
            do j=1,n
               r(i,j)=0.0
               r2(i,j)=0.0
            enddo 
         enddo 
         eav=0.0
         eav2=0.0
      endif

      do i=1,n
         v(i)=0.
      enddo
      
      do i=1,n
         do j=i+1,n
            w=vlj(x,y,z,i,j)
            v(i)=v(i)+w
            v(j)=v(j)+w
         enddo
      enddo
      
      if(temp.lt.0.95) then 
         h=(16./(1.-sqrt(temp)))**(1./6.)
         h=h-(16./(1.+sqrt(Temp)))**(1./6.)
         h=0.045*h              ! optimal step size
      else
         h=0.05*sqrt(temp)
      endif
      if(itflag.eq.1) then
         write(6,*) 'temp changed'
         do i=1,n
            do j=1,n
               r(i,j)=0.0
               r2(i,j)=0.0
            enddo 
         enddo 
         eav=0.0
         eav2=0.0
         ist=0
      endif
      do m=1,pstep
         ist=ist+1              ! to compute cv
         call propagate(x,y,z,h,idum,temp)
         do i=1,n
            do j=1,i-1
               r2i=(x(i)-x(j))**2+ (y(i)-y(j))**2+(z(i)-z(j))**2
               r(i,j)=r(i,j)+sqrt(r2i)
               r2(i,j)=r2(i,j)+r2i
            enddo 
         enddo 
         e=0.0
         do k=1,n
            e=e+v(k)
           ! write(6,*) v(k)
         enddo
         e=e/2.0
 
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


     
      end
C     
C------------------init----------------------------------
C     c     
C     ************************************************************

      subroutine init(x,y,z,h,temp)
c     
C     ************************************************************

      include 'lj.inc'
      open(10,file='xyz.in',status='old')
      do i=1,n
         read(10,*) x(i),y(i),z(i)
      enddo
      close(10)
      open(10,file='temp.in',status='old')
      read(10,*) temp
      close(10)

      open(unit=41,file='cv.out',status='UNKNOWN')
      
      
      do i=1,n
         v(i)=0.
      enddo
      
      do i=1,n
         do j=i+1,n
            w=vlj(x,y,z,i,j)
            v(i)=v(i)+w
            v(j)=v(j)+w
         enddo
      enddo
      
      return
      end
c     
c--------------LJ potential-------------------
c     
      function vlj(x,y,z,i,j)
      include 'lj.inc'
      r22=(x(i)-x(j))**2+(y(i)-y(j))**2+(z(i)-z(j))**2
      r6=1./r22**3
      vlj=4.0*r6*(r6-1.0)
      return
      end
c     
c------------------propagate--------------------
c     

      subroutine propagate(x,y,z,h,idum,temp)
      include 'lj.inc'
      
      do k=1,n
         xo(k)=x(k)
         yo(k)=y(k)
         zo(k)=z(k)
         vo(k)=v(k)
      enddo

      i=1+n*ran0(idum)

      do k=1,n
         if (i.ne.k) v(k)=v(k)-vlj(x,y,z,i,k)
      enddo
      
      ddx=h*(ran0(idum)-0.5)    !random step
      ddy=h*(ran0(idum)-0.5)
      ddz=h*(ran0(idum)-0.5)
      ddxn=ddx/n 
      ddyn=ddy/n
      ddzn=ddz/n

      rmax=0.
      do k=1,n
         if (k.ne.i) then
            x(k)=x(k)-ddxn      !to conserve center of mass
            y(k)=y(k)-ddyn
            z(k)=z(k)-ddzn
         else
            x(k)=x(k)-ddxn+ddx
            y(k)=y(k)-ddyn+ddy
            z(k)=z(k)-ddzn+ddz
         endif
         r22=x(k)**2+y(k)**2+z(k)**2
         rmax=max(r22,rmax)      !to ensure atoms stay around
      enddo

      v(i)=0.
      do k=1,n
         if (k.ne.i) then
            w=vlj(x,y,z,k,i)
            v(k)=v(k)+w
            v(i)=v(i)+w
         endif
      enddo

      de=v(i)-vo(i)
      w=min(1.,exp(-de/temp))
      if (ran0(idum).gt.w) then ! reject move
         do k=1,n
            x(k)=xo(k)
            y(k)=yo(k)
            z(k)=zo(k)
            v(k)=vo(k)
         enddo
      endif

      return
      end
      
c     
c----------ran0-------------
c     
c     completely basic (and wrong) random number generator
c     replace with your own !
c     
      function ran0(idum)
      idum=123456789*idum+987654321
      idum=mod(idum,2**30)
      ran0=idum/(2**30*1.0)
      if(ran0.lt.0.0) ran0=ran0+1.0
      return
      end

