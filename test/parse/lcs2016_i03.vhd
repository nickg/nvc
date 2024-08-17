entity lcs2016_i03 is
end entity;

architecture test of lcs2016_i03 is
    function get return integer is
    begin
        return 42;
    end function;

    function get return real is
    begin
        return 42.0;
    end function;
begin

    b1: block is
        generic ( function f1 return integer;
                  function f1 return real );
        generic map (
            f1 [return integer] => get,  -- OK
            f1 [return real] => get );  -- OK
    begin
    end block;

    b2: block is
        port ( x : integer );
        port map ( x [return integer] => 1 );  -- Error
    begin
    end block;

    b3: block is
        generic ( function f1 return integer );
        generic map ( f1 [return boolean] => get );  -- Error
    begin
    end block;

    b4: block is
        generic ( g : integer );
        generic map ( g [return integer] => 4 );  -- Error
    begin
    end block;

    b5: block is
        generic ( function f1 return integer;
                  function f2 return real );
        generic map (
            f1 => get,                  -- OK
            f2 => get );                -- OK
    begin
    end block;

end architecture;
