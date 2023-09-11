entity shuffle1 is
end entity;

architecture test of shuffle1 is
    type t_order is protected
        impure function get return natural;
    end protected;

    type t_order is protected body
        variable last  : time := time'high;
        variable count : natural;

        impure function get return natural is
            variable r : natural;
        begin
            if now /= last then
                last := now;
                count := 0;
            end if;
            r := count;
            count := count + 1;
            return r;
        end function;
    end protected body;

    shared variable global : t_order;

    type order_vector is array (natural range <>) of natural;

    constant ITERS : natural := 100;
begin

    g: for i in 1 to 5 generate

        p: process is
            variable history : order_vector(1 to ITERS);
            variable order : natural;
            variable shuffled : boolean := false;
        begin
            for j in 1 to ITERS loop
                order := global.get;
                --report p'instance_name & " ==> " & integer'image(order);
                history(j) := order;
                wait for 1 ns;
            end loop;

            for j in 2 to ITERS loop
                shuffled := shuffled or (history(j) /= history(j - 1));
            end loop;

            assert shuffled;
            wait;
        end process;

    end generate;

end architecture;
