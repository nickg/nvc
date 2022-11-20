library ieee;
use ieee.std_logic_1164.all;

entity sub_module is
    port (
        a       : in std_logic;
        b       : in std_logic;
        c       : out std_logic
    );
end entity;

architecture test of sub_module is

    type t_record_in_submodule is record
        elem       : std_logic;
    end record;

    signal record_in_submodule : t_record_in_submodule;

begin

    tgl_proc : process (a,b)
    begin
        c <= a xor b;
    end process;

end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity cover2 is
end entity;

architecture test of cover2 is

    -- Full toggle
    signal t1_a  : std_logic := '0';
    signal t1_b  : std_logic := '0';
    signal t1_c  : std_logic;

    -- Toggle only 0 -> 1
    signal t2_a  : std_logic := '0';
    signal t2_b  : std_logic := '0';
    signal t2_c  : std_logic := '0';

    -- Toggle only 1 -> 0
    signal t3_a  : std_logic := '1';
    signal t3_b  : std_logic := '0';
    signal t3_c  : std_logic;

    -- Toggle on multi-dimensional array
    type t_tensor is array (natural range <>, natural range <>, natural range <>) of std_logic;
    signal tensor : t_tensor(0 to 2, 2 downto 0, 2 downto 0) := (others => (others => ('1','0','1')));

    -- Toggle on nested array
    type t_memory_2d is array (2 downto 0) of std_logic_vector(0 to 3);
    signal memory_2d : t_memory_2d := (others => x"A");

    type t_memory_3d is array (2 downto 0) of t_memory_2d;
    signal memory_3d : t_memory_3d := (others => (others => x"5"));

    -- Toggle on signals within record and nested record
    type t_nested_record is record
        a       : natural;
        b       : std_logic;
        c       : std_logic_vector(2 downto 0);
    end record;

    type t_record is record
        x       : std_logic;
        y       : std_logic_vector(3 downto 0);
        z       : t_nested_record;
    end record;

    signal my_rec : t_record := ('0', "0000", (0, '0', "000"));

    -- coverage off
    signal my_ignored_signal : std_logic := '0';
    -- coverage on

begin

    -- Toggle on ports - Full
    sub_module_inst : entity work.sub_module
    port map (
        a   => t1_a,
        b   => t1_b,
        c   => t1_c
    );

    -- Toggle on ports - 0 -> 1 only
    sub_module_inst_2 : entity work.sub_module
    port map (
        a   => t2_a,
        b   => t2_b,
        c   => t2_c
    );

    -- Toggle on ports - 1 -> 0 only
    sub_module_inst_3 : entity work.sub_module
    port map (
        a   => t3_a,
        b   => t3_b,
        c   => t3_c
    );


    test_proc : process
    begin
        -- Toggle 0 -> 1 and 1 -> 0 on both inputs and outputs
        wait for 1 ns;
        t1_a <= '1';
        wait for 1 ns;
        t1_b <= '1';
        wait for 1 ns;
        t1_a <= '0';
        wait for 1 ns;
        t1_b <= '0';

        -- Toggle 0 -> 1 only
        wait for 1 ns;
        t2_a <= '1';
        wait for 1 ns;

        -- Toggle 1 -> 0 only
        wait for 1 ns;
        t3_a <= '0';
        wait for 1 ns;

        -- Mixed toggle on multi-dimensional array
        for i in 0 to 2 loop
            for j in 0 to 2 loop
                for k in 0 to 2 loop
                    tensor(i,j,k) <= not tensor(i,j,k);
                    wait for 1 ns;
                end loop;
            end loop;
        end loop;

        -- Mixed toggle on nested array
        for i in 0 to 2 loop
            for j in 0 to 3 loop
                if (i = j) then
                    memory_2d(i)(j) <= not memory_2d(i)(j);
                    wait for 1 ns;
                end if;
            end loop;
        end loop;

        for i in 0 to 2 loop
            for j in 0 to 3 loop
                for k in 0 to 3 loop
                    if (i = j and j = k and k = i) then
                        memory_3d(i)(j)(k) <= not memory_3d(i)(j)(k);
                        wait for 1 ns;
                    end if;
                end loop;
            end loop;
        end loop;

        -- Toggle 0 -> 1 on signals within record
        my_rec.x <= '1';
        my_rec.y <= "0101";
        wait for 1 ns;

        my_rec.z.b <= '1';
        my_rec.z.c <= "101";
        wait for 1 ns;

        wait;
    end process;

    -- Toggle on the process just to make sure that toggle callback
    -- is truly not called and does not mess something up.
    process
    begin
        wait for 1 ns;
        my_ignored_signal <= '1';
        wait for 1 ns;
        my_ignored_signal <= '0';
        wait for 1 ns;
        wait;
    end process;

end architecture;
