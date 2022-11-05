program mainProg
    implicit none
    character(len=15) :: firstName ! esto tiene que ser de longitud variable
    character(len=15) :: secondName
    character(len=15) :: transaction
    character(len=15) :: preposition
    real :: amount
    integer :: ios

    do
        read (*, *, iostat=ios) firstName, transaction, amount, preposition, secondName
        if (ios < 0) exit 
        print *, firstName, transaction, amount, preposition, secondName
       
    end do

end program mainProg
