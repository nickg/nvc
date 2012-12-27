architecture a of e is
begin

    process is
    begin
        x.all := 1;
        v := x.all + 5;
        p := new t;
    end process;

end architecture;
