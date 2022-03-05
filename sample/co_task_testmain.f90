program co_task_test
  use m_coarray_task_manager
  implicit none
  type(t_co_task) :: task(100)
  integer:: i,me,ne

  call set_msg(.true.)
  call alloc_task(size(task))
  me=this_image()
  ne=num_images()
  if(me==1)then
    do i=1,size(task)
       task(i)=t_co_task([i,i/ne,-i*me],&
         &[dble(i),dble(i)*ne,0d0],&
         &[.true.,.false.,.false.])
    end do
    call task_broadcast(task)
    print*, compiled_by
    print*, compiled_with
  end if
  sync all
  !----------------
  ! show task data in each images
  if(me==1)then
    call showtask()
  else
    sync images(me-1)
    call showtask()
  end if
  if(me<num_images()) sync images(me+1)
  !----------------
  ! run tasks in each images
  ! ....
  do i=1,task_size
    call sleep(me/5)
    print'(a,i4,a,i3,a,i3,a,i4)',"done:image=",me,",task=(",i,"/",task_size,"), total=",co_task(i)%idata(1)
  end do
  contains
    subroutine showtask()
      do i=1,task_size
        call showobj(co_task(i))
      end do
    end subroutine
end program
