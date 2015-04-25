entity issue135 is
end entity;

architecture test of issue135 is
  function bad_performance (
    constant a : string;
    constant b : natural   := 0;
    constant c : character := ',';
    constant d : time      := 0 ns;
    constant e : string    := "i" )
    return string is
  begin
    return (natural'image(b) &
            time'image(d) &  -- Uncomment this line to increase runtime further
            c &
            e &
            c );
  end function;
begin

end architecture;
