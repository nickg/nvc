entity Foo is
    generic (
        type DATA_TYPE;
        type DATA_ARRAY_TYPE is array (natural range <>) of data_type;
        INPUTS : positive);
    port (
        selector : in bit_vector(INPUTS-1 downto 0);
        input : in data_array_type(0 to INPUTS-1);
        output : out data_type);
end entity;

architecture rtl of Foo is

begin

end architecture;

-------------------------------------------------------------------------------

entity issue1201 is
end entity;

architecture rtl of issue1201 is

    constant INPUTS : positive := 3;

    subtype data_type_1 is bit_vector(7 downto 0);
    type data_array_type_1 is array (0 to INPUTS - 1) of data_type_1;

    signal selector : bit_vector(INPUTS - 1 downto 0) := (others => '0');
    signal input : data_array_type_1 := (others => (others => '0'));
    signal output : data_type_1;

begin

    foo_inst : entity work.Foo
        generic map (
            DATA_TYPE => data_type_1,
            DATA_ARRAY_TYPE => data_array_type_1,
            INPUTS => INPUTS)
        port map (
            selector => selector,
            input(0) => input(0),
            input(1 to 2) => input(1 to 2),
            output => output);

end architecture;
