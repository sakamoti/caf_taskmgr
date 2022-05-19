program testast
  use iso_fortran_env
  use m_coarray_task_manager
  implicit none
  integer,parameter :: num=8
  class(*),allocatable :: obj
  integer :: i,j
  type test
    integer :: n=2
  end type
  if(this_image()>1) sync images (this_image()-1)
  print*, "this_image=",this_image(),"num_images=",num_images()
  if(this_image()<num_images())  sync images (this_image()+1)
  call set_msg(.true.)
  call alloc_task(num)
  call put_task(1)
  call put_task(1.0)
  call put_task(1d0)
  call put_task(1.0_real128)
  call put_task((1.0,0.0))
  call put_task((1d0,0d0))
  call put_task("char")
  call get_task(obj,this_image())
  call put_task(.false.)
  call put_task(test(n=3))
  !call retrieve_task_from(2)
  !---
  !if(this_image()>1) sync images (this_image()-1)
  do i=1,num_images()
    !call retrieve_task_from(i)
    call get_task(obj,i) !;call showobj(obj)
  end do
  call clear_unused_task()
  !if(this_image()<num_images())  sync images (this_image()+1)
  !---
  call gc_task()
  !------
  if(this_image()==1) print*, " --- task_broadcast ---"
  call task_broadcast([1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6],1)
  print *,this_image(),task_size
  stop
  sync all
  if(this_image()==1) print*, " --- task_broadcast ---"
  call task_broadcast([1d0,6d0],3)
  sync all
  if(this_image()==1) print*, " --- task_broadcast ---"
  call task_broadcast([.true.,.false.],4)
  sync all
  if(this_image()==1) print*, " --- task_broadcast ---"
  block
    real(kind=real64) :: x(10000)
    call random_number(x)
    call task_broadcast(x,1)
    !call task_broadcast(x,2)
  end block
  sync all
  !---
  if(this_image()==2)then
    block
      real(kind=real64) :: x(100000)
      integer :: i
      do i=1,1000
        call random_number(x)
      enddo
    end block
  end if
  if(this_image()==1) then
    print*, "!-----------------"
    !print*, compiled_by
    !print*, compiled_with
    call task_maxloc(i,j)
    print*, "maxloc",i,j
    call task_minloc(i,j)
    print*, "minloc",i,j
    !--
    call task_maxloc(i,obj=obj)
    call task_minloc(i,obj=obj)
    print*,"--showobj--"
    call showobj(obj)
    print*,"--end showobj--"
    !--
    call task_maxloc(i)
    print*, "maxloc",i
    call task_minloc(i)
    print*, "minloc",i
    print*, "maxloc",i,j
    call task_minloc(i,j)
    print*, "minloc",i,j
  end if
  print*, "done image=",this_image()
end program
