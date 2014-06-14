architecture a of e is
begin

    process is
    begin
        x.all := 1;
        v := x.all + 5;
        p := new t;
        p := a.all(1 to 3);
        q := a.all(3);
    end process;

end architecture;
