PACKAGE reproducer_pkg IS
    TYPE t_reproducer IS PROTECTED

        FUNCTION  func_a(delim : character) RETURN boolean;
        FUNCTION  func_a RETURN boolean;

    END PROTECTED t_reproducer;
END PACKAGE reproducer_pkg;

PACKAGE BODY reproducer_pkg IS

    TYPE t_reproducer IS PROTECTED BODY

        FUNCTION func_a RETURN boolean IS
        BEGIN
            RETURN TRUE;
        END FUNCTION func_a;

        FUNCTION func_a(delim : character) RETURN boolean IS
        BEGIN
            RETURN func_a;
        END FUNCTION func_a;

    END PROTECTED BODY t_reproducer;
END PACKAGE BODY;
