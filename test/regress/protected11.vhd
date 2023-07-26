entity protected11 is
end entity;

architecture test of protected11 is

    procedure do_something;
    procedure do_wait;

    type pt is protected
        procedure proc;
    end protected;

    type pt is protected body
        variable count : natural;

        procedure proc(arg : integer);

        procedure proc is
        begin
            count := count + 1;
            assert count = 1;
            proc(5);
            assert count = 2;
            proc(-1);
        end procedure;

        procedure proc(arg : integer) is
        begin
            count := count + 1;
            do_something;
            if arg < 0 then
                do_wait;                -- Error
            end if;
        end procedure;

    end protected body;

    procedure do_something is
    begin
    end procedure;

    procedure do_wait is
    begin
        wait for 1 ns;
    end procedure;

    shared variable v : pt;
begin

    tb: process is
    begin
        v.proc;
        wait;
    end process;

end architecture;
