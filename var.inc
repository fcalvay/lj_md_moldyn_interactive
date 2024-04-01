! parametre pour programme principal

	integer, parameter :: n       = 13
	integer, parameter :: ng=n
	real, parameter :: kb  = 1.0
	real, parameter :: dt1 = 0.002
	common /tempe/ tin,itflag,r(n,n),r2(n,n),eav,eav2
	integer, parameter :: iobserv=10
	integer, parameter :: irenorm=10
	integer, parameter :: nbin=n/25
	real, parameter :: box     = 11
	real, parameter :: box2    = box/2.0
	real, parameter :: box2inv = 1.0/box2
	real, parameter :: boxinv  = 1.0/box 

        real,parameter :: rradius=20.0	
        real,parameter :: fact=1.1

	

	integer, parameter :: nbarre=150
	real, parameter ::    limit=1.1

        double precision, parameter :: radius=0.4
        integer, parameter :: ibox=box     
