entity cover1 is
end entity;

architecture test of cover1 is

    signal my_signal : integer;

    procedure my_global_procedure is
    begin
        rpt1: report "Report within GLOBAL procedure";
    end procedure;

    function my_global_function(i : integer) return boolean is
    begin
        rpt2: report "Report within GLOBAL function";

        if (i mod 2 = 0) then
            return true;
        else
            return false;
        end if;
    end function;

begin

    test_proc : process
        variable cnt : integer := 0;
        variable rv : boolean;

        procedure my_local_procedure is
        begin
            rpt3: report "Report within LOCAL procedure";
        end procedure;

        function my_local_function(i : integer) return boolean is
        begin
            rpt4: report "Report within LOCAL function";
            if (i mod 2 = 0) then
                return true;
            else
                return false;
            end if;
        end function;

    begin

        -- T_IF, T_FOR, T_CASE
        loop1: for i in 0 to 4 loop
            if_cond_1: if (i = 0) then
                report "T_IF, i = 0";
            elsif (i = 1) then
                report "T_IF, i = 1";
            elsif (i = 2) then
                report "T_IF, i = 2";
            elsif (i = 3) then
                report "T_IF, i = 3";
            elsif (i = 4) then
                report "T_IF, i = 4";
            elsif (i = 10) then
                -- Some unreachable statement to get also some uncovered statements.
                assert (False);

            else
                -- coverage off
                assert (False) report "PRAGMA COVER OFF statement";
                -- coverage on
            end if;

            case_cond_1: case i is
            when 0 =>
                report "T_CASE, i = 0";
            when 1 =>
                report "T_CASE, i = 1";
            when others =>
                report "T_CASE, i = others";
            end case;
        end loop;

        -- T_VAR_ASSIGN, T_WHILE, T_NEXT, T_EXIT
        cnt := 0;
        while_loop: while (true) loop
            cnt := cnt + 1;

            if (cnt = 1) then
                next;
            end if;

            if (cnt = 10) then
                exit;
            end if;
            report "T_WHILE, i = " & integer'image(cnt);
        end loop;

        -- T_SIGNAL_ASSIGN, T_ASSERT, T_WAIT
        sign_assign_1: my_signal <= 1;
        wait_1: wait for 0 ns;
        sign_assign_2: my_signal <= 0;
        wait_2: wait for 0 ns;
        assert_1: assert(my_signal = 0);

        -- T_PROC_CALL, T_FUNC_CALL, T_RETURN
        proc_call_global: my_global_procedure;
        rv := my_global_function(cnt);
        rv := my_global_function(cnt + 3);
        proc_call_local: my_local_procedure;
        rv := my_local_function(cnt + 2);
        rv := my_local_function(cnt + 5);

        wait for 1 ns;

        wait;
    end process;

end architecture;
