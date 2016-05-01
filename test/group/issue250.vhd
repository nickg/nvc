entity field is
end entity;

architecture fum of field is

    constant cols:  natural := 2;
    constant rows:  natural := 2;

    type inter_cell is record
        alive_in:   bit_vector (0 to 1);
        alive_out:  bit;
    end record;

    type field_cell_signals is array
                (integer range <>, integer range <>) of inter_cell;

    signal cellio : field_cell_signals(0 to rows - 1, 0 to cols - 1);  -- 0..11

     signal row:     integer range 0 to rows - 1 := 1;  -- 12
     signal col:     integer range 0 to rows - 1 := 1;  -- 13

begin
    cellio(col, row).alive_in <= cellio(col - 1, row - 1).alive_out & '0'
                        when (col > 0 and row > 0) else "00";
end architecture;
