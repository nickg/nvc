package TEST_TYPES is
    type WIDTH_TYPE is record
        DATA : integer;
    end record;
end package;

use work.test_types.all;

entity  TEST_NG is
    generic (
        WIDTH  : WIDTH_TYPE
    );
    port (
        DATA_I : in  bit_vector(WIDTH.DATA-1 downto 0);
        DATA_O : out bit_vector(WIDTH.DATA-1 downto 0)
    );
end TEST_NG;

architecture MODEL of TEST_NG is
    procedure  output_data is
    begin
      DATA_O <= DATA_I;
    end procedure;
begin
end MODEL;
