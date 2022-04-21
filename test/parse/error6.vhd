package axil_generic_pkg is

    generic(
        ADDR_WIDTH : natural
    );

    type mosi_t is record
        valid : bit;
        addr  : bit_vector(ADDR_WIDTH-1 downto 0);
    end record;

    type miso_t is record
        ready : bit;
    end record;
