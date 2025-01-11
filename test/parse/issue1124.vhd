entity issue1124 is
end entity;

architecture test of issue1124 is
    type t_logic is ('U', 'X', '1', '0');
    type t_logic_vector is array (natural range <>) of t_logic;

    function or_reduce(arg: t_logic_vector) return t_logic is
    begin
        return '1';
    end function;

    function "??" (l : t_logic) return boolean is
    begin
        return false;
    end function;

    signal s : t_logic_vector(7 downto 0);
begin

    process is
    begin
        if or_reduce(s(3 downto 0)) then  -- OK
        end if;
        wait;
    end process;

end architecture;
