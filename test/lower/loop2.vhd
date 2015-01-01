entity loop2 is
end entity;

architecture test of loop2 is

    procedure p3(t : in time) is
    begin
        loop
            if t >= 1 ps then
                return;
            end if;
        end loop;
    end procedure;

begin
end architecture;
