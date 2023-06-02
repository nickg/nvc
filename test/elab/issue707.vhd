entity test_module is
    generic (
        CONDITION : string := "true"
    );
    port (
        output : out bit_vector
    );
end entity test_module;

architecture rtl of test_module is

begin
    bad_cond : if CONDITION = "true" generate
        output <= X"12";
    else generate
        output <= X"1234";              -- Should be no error here
    end generate;
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    signal s : bit_vector(7 downto 0);
begin

    u: entity work.test_module
        port map ( s );

end architecture;
