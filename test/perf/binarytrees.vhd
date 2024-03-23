package binarytrees is
    procedure test_binarytrees;
end package;

package body binarytrees is
    type treeNode;
    type treeNodePtr is access treeNode;

    type treeNode is record
        left, right : treeNodePtr;
    end record;

    procedure NewTreeNode (variable left, right : in  treeNodePtr;
                           result               : out treeNodePtr) is
    begin
        result := new TreeNode'(left, right);
    end procedure;

    procedure ItemCheck (variable tree : in  treeNodePtr;
                         result        : out integer) is
        variable left_sum, right_sum   :     integer;
    begin
        if tree.left = null then
            result := 1;
        else
            ItemCheck(tree.left, left_sum);
            ItemCheck(tree.right, right_sum);
            result := 1 + left_sum + right_sum;
        end if;
    end procedure;

    procedure BottomUpTree (depth  : in  integer;
                            result : out treeNodePtr) is
        variable left, right       :     treeNodePtr;
    begin
        if depth > 0 then
            BottomUpTree(depth - 1, left);
            BottomUpTree(depth - 1, right);
        end if;
        NewTreeNode(left, right, result);
    end procedure;

    procedure DeleteTree (tree : inout treeNodePtr) is
    begin
        if tree.left /= null then
            DeleteTree(tree.left);
            DeleteTree(tree.right);
        end if;

        deallocate(tree);
    end procedure;

    procedure test_binarytrees is
        constant VERBOSE       : boolean := false;
        constant N             : integer := 10;
        variable minDepth      : integer;
        variable maxDepth      : integer;
        variable stretchDepth  : integer;
        variable check         : integer;
        variable depth         : integer;
        variable iterations    : integer;
        variable sum           : integer;
        variable stretchTree   : treeNodePtr;
        variable longLivedTree : treeNodePtr;
        variable tempTree      : treeNodePtr;
    begin
        minDepth := 4;

        if minDepth + 2 > N then
            maxDepth := minDepth + 2;
        else
            maxDepth := N;
        end if;

        stretchDepth := maxDepth + 1;

        BottomUpTree(stretchDepth, stretchTree);
        ItemCheck(stretchTree, check);

        assert not VERBOSE report "stretch tree of depth "
            & integer'image(stretchDepth)
            & HT & " check: " & integer'image(check)
            severity note;

        DeleteTree(stretchTree);

        BottomUpTree(maxDepth, longLivedTree);

        depth          := minDepth;
        while depth    <= maxDepth loop
            iterations := 2 ** (maxDepth - depth + minDepth);
            check      := 0;

            for i in 1 to iterations loop
                BottomUpTree(depth, temptree);
                ItemCheck(tempTree, sum);
                check := check + sum;
                DeleteTree(tempTree);
            end loop;

            assert not VERBOSE report integer'image(iterations) & HT
                & " trees of depth " & integer'image(depth)
                & HT & " check: " & integer'image(check)
                severity note;

            depth := depth + 2;
        end loop;

        ItemCheck(longLivedTree, check);
        assert not VERBOSE report "long lived tree of depth "
            & integer'image(maxDepth)
            & HT & " check: " & integer'image(check)
            severity note;

    end procedure;
end package body;
