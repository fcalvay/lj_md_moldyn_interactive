      parameter(n=13)            ! number of atoms
      parameter(nstep=100000000) ! monte carlo steps
      parameter(radius2=5.0)     ! radius to keep atoms
      parameter(radius=0.5)      ! display radius   
      parameter(ipasinf=1000)    ! when to send coordinates
      dimension x(n),y(n),z(n)   ! coordinates
      dimension xo(n),yo(n),zo(n)
      common /tempe/ tin,itflag,r(n,n),r2(n,n),eav,eav2,v(n)
      dimension vo(n)       !potential
