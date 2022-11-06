module bst_mod
        implicit none
        ! Contains no lists form the moment
        type :: a_tree_node
            character(len=:),  allocatable :: godName 
            type(a_tree_node), pointer     :: left
            type(a_tree_node), pointer     :: right
        end type a_tree_node

contains

    ! Subroutine which prints the value of the node 
    recursive subroutine printBst(node)
        type(a_tree_node), pointer, intent(in) :: node

        if (associated(node)) then
            call printBst(node%left)
            print *, node%godName
            call printBst(node%right)
        end if
    end subroutine printBst 

    ! Subroutine which inserts a new name in the tree (starting from provided node)
    recursive subroutine bstInsert(node, newName)
        type(a_tree_node), pointer, intent(in out) :: node
        character(len=:),  allocatable, intent(in) :: newName 

        if (.not.associated(node)) then
            allocate(node)
            node = a_tree_node(godName=newName, left=null(), right=null())
            print *, newName, " has been added"
        else if (lgt(node%godName, newName)) then
            call bstInsert(node%left, newName)
        else if (llt(node%godName, newName)) then
            call bstInsert(node%right, newName)
        else
            continue
        end if
    end subroutine bstInsert
end module bst_mod
