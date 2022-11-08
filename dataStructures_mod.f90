module dataStructures_mod
        implicit none
        ! Contains no lists form the moment
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

!        private :: getDeityNode
        
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
        else 
            call listInsert(head%next, newDeityNode, amount)
        end if
    end subroutine listInsert

    ! Subroutine which prints the list provided
    recursive subroutine printList(head)
        type(a_list_item), pointer, intent(in) :: head

        if(associated(head)) then
            print *, "    ", head%deityNode%deityName, head%amount
            call printList(head%next)
        end if 
    end subroutine printList


    ! TREE ---------------------------------------
    ! Subroutine which prints the value of the node 
    recursive subroutine printBst(node)
        type(a_tree_node), pointer, intent(in) :: node

        if (associated(node)) then
            call printBst(node%left)
            print *, node%deityName
            if (associated(node%debit)) then
                print *, "  debit" 
                call printList(node%debit)
            end if
            if (associated(node%credit)) then
                print *, "  credit" 
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
            print *, newName, " has been added"
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

    ! Subroutine which destroys the tree
    recursive subroutine bstDestroy(node)
        type(a_tree_node), pointer, intent(in out) :: node

        if (associated(node)) then
            call bstDestroy(node%left)
            call bstDestroy(node%right)
            deallocate(node)
        end if
    end subroutine

end module dataStructures_mod
