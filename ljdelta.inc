      parameter(n=13)            ! number of atoms
      parameter(nstep=9000000) ! monte carlo steps
      common /temp/ temp         ! temperature, 
      parameter(radius2=5.0)
      dimension x(n),y(n),z(n)   ! coordinates
      dimension xo(n),yo(n),zo(n)
      dimension v(n),vo(n)       !potential
