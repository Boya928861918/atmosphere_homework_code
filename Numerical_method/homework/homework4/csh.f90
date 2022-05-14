     program main
      real*8,allocatable::b(:),x(:),tol(:),x0(:)
      real*8,allocatable::h(:,:),D(:,:),L(:,:),U(:,:),DL(:,:),ib(:,:),e(:,:)
      integer i,j,k,m,n,q
      real*8 check
      

      
      write(*,*)"n="
      read(*,*)n
      
      allocate(h(n,n),b(n),D(n,n),L(n,n),U(n,n),x(n),DL(n,n),ib(n,n),e(n,n),x0(n),tol(n))
      h=0.0

      do i=1,n
         do j=1,n
            h(i,j)=1.00/(i+j-1)
         end do
      end do
      
      do i=1,n
         b(i)=sum(h(i,:))
      end do

      D=0.0
      do i=1,n
         D(i,i)=h(i,i)
      end do
       
      L=0.0
      do i=n,2,-1
         do j=1,i-1
            L(i,j)=h(i,j)
         end do
      end do
      
      U=0.0
      do j=n,2,-1
         do i=1,j-1
            U(i,j)=h(i,j)
         end do 
      end do
      
      e=0.0
      do i=1,n
         e(i,i)=1.0
      end do

       DL=D+L
       do i=2,n
         do j=1,i-1
            r=DL(i,j)/DL(j,j)
            do k=1,n
               DL(i,k)=DL(i,k)-r*DL(j,k)
            end do
            e(i,:)=e(i,:)-r*e(j,:)
         end do
       end do
       do i=1,n
         e(i,:)=e(i,:)/DL(i,i)
       end do
     
       x=0.0
       ib=-matmul(e,U)
       x0=1.0
       m=0
       check = 1
      do while(check>1E-3)
         m=m+1
         x=matmul(ib,x)+matmul(e,b)
         tol=(abs(x-x0))
         check=maxval(tol)
         print*, check
     
      end do
     write(*,*)"m",m
      
      
      write(*,*)"x="
      do i=1,n
      write(*,"(10000F12.6)") x(i)
      end do
     

      end program
