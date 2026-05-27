library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

package vhpi21_pkg is
    type leaf_status_t is record
        payload : std_logic_vector;
    end record;

    type leaf_status_array_t is array (natural range <>) of leaf_status_t;

    type inner_status_t is record
        item : leaf_status_array_t;
    end record;

    type status_t is record
        inner : inner_status_t;
    end record;

    type sample_record_t is record
        data  : std_logic_vector;
        tag   : std_logic_vector(1 downto 0);
        addr  : std_logic_vector;
        valid : std_logic_vector(0 downto 0);
    end record;

    type sample_array_t is array (natural range <>) of sample_record_t;

    function init_sample(
        data_width : positive;
        addr_width : positive) return sample_record_t;

    function derived_data_width(width : positive) return positive;
    function derived_addr_width(width : positive) return positive;
end package;

package body vhpi21_pkg is
    function loop_identity(width : positive) return positive is
    begin
        return positive(integer(ceil(real(width))));
    end function;

    function derived_data_width(width : positive) return positive is
    begin
        return loop_identity(width);
    end function;

    function derived_addr_width(width : positive) return positive is
    begin
        return loop_identity(width);
    end function;

    function init_sample(
        data_width : positive;
        addr_width : positive) return sample_record_t is
        variable r : sample_record_t(
            data(data_width - 1 downto 0),
            addr(addr_width - 1 downto 0));
    begin
        r.data  := (others => '0');
        r.tag   := (others => '0');
        r.addr  := (others => '0');
        r.valid := (others => '0');
        return r;
    end function;
end package body;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity vhpi21_leaf_unit is
    generic (
        width : positive := 8);
    port (
        clk : in  std_logic;
        y   : out std_logic_vector(width - 1 downto 0));
end entity;

architecture test of vhpi21_leaf_unit is
begin
    y <= (others => '0');
end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.vhpi21_pkg.all;

entity vhpi21_nested_unit is
    generic (
        width : positive := 8;
        count : positive := 2);
    port (
        clk    : in  std_logic;
        status : out status_t(
            inner(
                item(0 to count - 1)(
                    payload(width - 1 downto 0)))));
end entity;

architecture test of vhpi21_nested_unit is
    signal leaf_y : std_logic_vector(width - 1 downto 0);
begin
    u_leaf : entity work.vhpi21_leaf_unit
        generic map (
            width => width)
        port map (
            clk => clk,
            y   => leaf_y);

    status.inner.item(0).payload <= leaf_y;
    status.inner.item(1).payload <= (others => '0');
end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.vhpi21_pkg.all;

entity vhpi21_sample_unit is
    generic (
        data_width : positive := 8;
        addr_width : positive := 4);
    port (
        clk        : in  std_logic;
        sample_in  : in  sample_record_t(
            data(data_width - 1 downto 0),
            addr(addr_width - 1 downto 0));
        sample_out : out sample_record_t(
            data(data_width - 1 downto 0),
            addr(addr_width - 1 downto 0)));
end entity;

architecture test of vhpi21_sample_unit is
begin
    sample_out <= sample_in;
end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.vhpi21_pkg.all;

entity vhpi21_sample_array_unit is
    generic (
        data_width : positive := 8;
        addr_width : positive := 4;
        count      : positive := 2);
    port (
        clk : in std_logic);
end entity;

architecture test of vhpi21_sample_array_unit is
    constant c_data_width : positive := derived_data_width(data_width);
    constant c_addr_width : positive := derived_addr_width(addr_width);

    signal sample : sample_array_t(0 to count - 1)(
        data(c_data_width - 1 downto 0),
        addr(c_addr_width - 1 downto 0)) := (
        others => init_sample(c_data_width, c_addr_width));
    signal sample_copy : sample_array_t(0 to count - 1)(
        data(c_data_width - 1 downto 0),
        addr(c_addr_width - 1 downto 0)) := (
        others => init_sample(c_data_width, c_addr_width));
begin
    gen_sample : for i in 0 to count - 1 generate
        u_sample : entity work.vhpi21_sample_unit
            generic map (
                data_width => c_data_width,
                addr_width => c_addr_width)
            port map (
                clk        => clk,
                sample_in  => sample((i - 1 + count) mod count),
                sample_out => sample_copy(i));

        sample(i).data <= (others => '0');
        sample(i).tag <= (others => '0');
        sample(i).addr <= (others => '0');
        sample(i).valid <= (others => '0');
    end generate;
end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.vhpi21_pkg.all;

entity vhpi21 is
end entity;

architecture test of vhpi21 is
    constant width      : positive := 8;
    constant addr_width : positive := 4;
    constant count      : positive := 2;

    signal clk : std_logic := '0';
    signal status : status_t(
        inner(
            item(0 to count - 1)(
                payload(width - 1 downto 0)))) := (
        inner => (
            item => (
                others => (
                    payload => (others => '0')))));
    signal cache : leaf_status_array_t(0 to count - 1)(
        payload(width - 1 downto 0)) := (
        others => (
            payload => (others => '0')));
    signal sample : sample_array_t(0 to count - 1)(
        data(width - 1 downto 0),
        addr(addr_width - 1 downto 0)) := (
        others => init_sample(width, addr_width));
begin
    u_nested : entity work.vhpi21_nested_unit
        generic map (
            width => width,
            count => count)
        port map (
            clk    => clk,
            status => status);

    u_samples : entity work.vhpi21_sample_array_unit
        generic map (
            data_width => width,
            addr_width => addr_width,
            count      => count)
        port map (
            clk => clk);

    gen_top_cache : for i in 0 to count - 1 generate
        cache(i).payload <= (others => '0');
        sample(i).data <= (others => '0');
        sample(i).tag <= (others => '0');
        sample(i).addr <= (others => '0');
        sample(i).valid <= (others => '0');
    end generate;
end architecture;
