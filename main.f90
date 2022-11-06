program mainProg
    use :: bst_mod 
    implicit none
    ! Initial length for reading data, will be trimed for storage
    integer,        parameter :: upperBoundLength = 20
    integer :: ios
    ! Variables needed for processing input lines
    character(len=upperBoundLength) :: firstName 
    character(len=upperBoundLength) :: secondName
    character(len=upperBoundLength) :: transaction
    character(len=upperBoundLength) :: preposition
    real :: amount
    ! Binary search tree declaration
    type(a_tree_node), pointer :: root 
    root => null()

    do
        read (*, *, iostat=ios) firstName, transaction, amount, preposition, secondName
        if (ios < 0) exit 
        ! IMPORTANT - These variables need to be trimmed (trim(firstName)...)
        ! when passing them to the tree
        
        if ((transaction == "lent") .or. (transaction == "borrowed")) then
            block
                character(len=:), allocatable :: firstGodName 
                character(len=:), allocatable :: secondGodName 
                firstGodName = trim(firstName)
                secondGodName = trim(secondName)
                call bstInsert(root, firstGodName)
                call bstInsert(root, secondGodName)
            end block
        else
            print *, "Wrong transaction '", trim(transaction), "'"
        end if
    end do
end program mainProg
