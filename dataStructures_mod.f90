! File which implements the data structures (Binary Search Tree and single
! linked list) that will be used in Project 1 - TdP subject.
! Sergio Guerra Arencibia - ULL - 08-11-2022

module dataStructures_mod
        implicit none

        type :: a_tree_node
            character(len=:),  allocatable :: deityName 
            type(a_tree_node), pointer     :: left
            type(a_tree_node), pointer     :: right
            type(a_list_item), pointer     :: credit 
            type(a_list_item), pointer     :: debit
        end type a_tree_node

        type :: a_list_item
            type(a_tree_node), pointer :: deityNode
            real                       :: amount
            type(a_list_item), pointer :: next
        end type a_list_item

        private :: getDeityNode
        private :: sumListValues
        
contains

    ! LIST ---------------------------------------
    ! Subroutine which inserts a value in the list
    recursive subroutine listInsert(head, newDeityNode, amount)
        type(a_list_item), pointer, intent(in out) :: head
        type(a_tree_node), pointer, intent(in)     :: newDeityNode 
        real,                          intent(in)  :: amount

        type(a_list_item), pointer :: newNode 

        if (.not.associated(head)) then
            allocate(newNode)
            newNode = a_list_item(deityNode=newDeityNode, amount=amount, next=null())
            head => newNode
        else if (llt(newDeityNode%deityName, head%deityNode%deityName)) then
            allocate(newNode)
            newNode = a_list_item(deityNode=newDeityNode, amount=amount, next=null())
            newNode%next => head
            head => newNode
        else if (newDeityNode%deityName == head%deityNode%deityName) then
            head%amount = head%amount + amount
        else 
            call listInsert(head%next, newDeityNode, amount)
        end if
    end subroutine listInsert

    ! Subroutine which prints the list provided
    recursive subroutine printList(head)
        type(a_list_item), pointer, intent(in) :: head

        if(associated(head)) then
            print '(8x, a, a, f0.2)', head%deityNode%deityName, ' ', head%amount
            call printList(head%next)
        end if 
    end subroutine printList

    ! Subroutine which destroys the list provided
    recursive subroutine destroyList(head)
        type(a_list_item), pointer, intent(in out) :: head

        if (associated(head)) then
            call destroyList(head%next)
            deallocate(head)
        end if
    end subroutine destroyList

    ! Function which returns the sum of values of all de list items
    recursive function sumListValues(head) result(totalValue)
        type(a_list_item), pointer, intent(in) :: head

        real :: totalValue

        if(associated(head)) then
            totalValue = head%amount
            totalValue = totalValue + sumListValues(head%next)
        else
            totalValue = 0.0
        end if
    end function sumListValues

    ! TREE ---------------------------------------
    ! Subroutine which prints the value of the node 
    recursive subroutine printBst(node)
        type(a_tree_node), pointer, intent(in) :: node

        if (associated(node)) then
            call printBst(node%left)
            print '(a, a)', node%deityName, ':'
            if (associated(node%debit)) then
                print '(4x, a)', "debit" 
                call printList(node%debit)
            end if
            if (associated(node%credit)) then
                print '(4x, a)', "credit" 
                call printList(node%credit)
            end if
            call printBst(node%right)
        end if
    end subroutine printBst 

    ! Subroutine which inserts a new name in the tree (starting from provided node)
    recursive subroutine bstInsert(node, newName)
        type(a_tree_node), pointer, intent(in out) :: node
        character(len=:),  allocatable, intent(in) :: newName 

        if (.not.associated(node)) then
            allocate(node)
            node = a_tree_node(deityName=newName, left=null(), right=null(), credit=null(), debit=null())
            print '(a, a)', newName, " has been added."
        else if (lgt(node%deityName, newName)) then
            call bstInsert(node%left, newName)
        else if (llt(node%deityName, newName)) then
            call bstInsert(node%right, newName)
        else
            continue
        end if
    end subroutine bstInsert

    ! Private function which returns the node of the deityName provided
    recursive function getDeityNode(root, deityName) result(deityNode)
        type(a_tree_node), pointer, intent(in)     :: root
        character(len=:),  allocatable, intent(in) :: deityName
        type(a_tree_node), pointer                 :: deityNode

        if(.not.associated(root)) then
            deityNode => null()
            print *, "Looking for a deity that is not in the tree"
        else if (llt(deityName, root%deityName)) then
            deityNode => getDeityNode(root%left, deityName)
        else if (lgt(deityname, root%deityName)) then
            deityNode => getDeityNode(root%right, deityName)
        else
            deityNode => root
        end if
    end function getDeityNode


    ! Subroutine which inserts a new credit into a deity list
    subroutine insertCredit(root, deityName, secondDeityName, amount)
        type(a_tree_node), pointer, intent(in)    :: root
        character(len=:), allocatable, intent(in) :: deityName
        character(len=:), allocatable, intent(in) :: secondDeityName
        real,                          intent(in) :: amount 

        type(a_tree_node), pointer :: firstDeityNode 
        type(a_tree_node), pointer :: secondDeityNode 
        firstDeityNode => getDeityNode(root, deityName)
        secondDeityNode => getDeityNode(root, secondDeityName)

        call listInsert(firstDeityNode%credit, secondDeityNode, amount)       
    end subroutine insertCredit


    ! Subroutine which inserts a new debit into a deity list
    subroutine insertDebit(root, deityName, secondDeityName, amount)
        type(a_tree_node), pointer, intent(in)    :: root
        character(len=:), allocatable, intent(in) :: deityName
        character(len=:), allocatable, intent(in) :: secondDeityName
        real,                          intent(in) :: amount 

        type(a_tree_node), pointer :: firstDeityNode 
        type(a_tree_node), pointer :: secondDeityNode 
        firstDeityNode => getDeityNode(root, deityName)
        secondDeityNode => getDeityNode(root, secondDeityName)

        call listInsert(firstDeityNode%debit, secondDeityNode, amount)
    end subroutine insertDebit

    ! Function which returns the total credit of the tree
    recursive function sumTreeValuesCredit(root) result(totalValue)
        type(a_tree_node), pointer, intent(in) :: root

        real :: totalValue
        totalValue = 0.0
        if (associated(root)) then
            totalValue = sumListValues(root%credit)
            totalValue = totalValue + sumTreeValuesCredit(root%left)
            totalValue = totalValue + sumTreeValuesCredit(root%right)
        end if
    end function sumTreeValuesCredit

    ! Function which returns the total dbeit of the tree
    recursive function sumTreeValuesDebit(root) result(totalValue)
        type(a_tree_node), pointer, intent(in) :: root

        real :: totalValue
        totalValue = 0.0
        if (associated(root)) then
            totalValue = sumListValues(root%debit)
            totalValue = totalValue + sumTreeValuesDebit(root%left)
            totalValue = totalValue + sumTreevaluesDebit(root%right)
        end if
    end function sumTreeValuesDebit

    ! Subroutine which prints the total credit and debit of the tree
    subroutine totalDebitAndCredit(root)
        type(a_tree_node), pointer, intent(in) :: root
        
        print '(a)'
        print '(a, f0.2)', "Net debit:  ", sumTreeValuesCredit(root)
        print '(a, f0.2)', "Net credit: ",  sumTreeValuesDebit(root)
    end subroutine

    ! Subroutine which destroys the tree
    recursive subroutine bstDestroy(node)
        type(a_tree_node), pointer, intent(in out) :: node

        if (associated(node)) then
            call bstDestroy(node%left)
            call bstDestroy(node%right)

            call destroyList(node%credit)
            call destroyList(node%debit)
            deallocate(node)
        end if
    end subroutine
end module dataStructures_mod
