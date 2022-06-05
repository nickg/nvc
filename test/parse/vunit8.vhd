package my_logic is

  type STD_ULOGIC is ( 'U',             -- Uninitialized
                       'X',             -- Forcing  Unknown
                       '0',             -- Forcing  0
                       '1',             -- Forcing  1
                       'Z',             -- High Impedance
                       'W',             -- Weak     Unknown
                       'L',             -- Weak     0
                       'H',             -- Weak     1
                       '-'              -- Don't care
                       );

  subtype std_logic is std_ulogic;
end package;

use work.my_logic.all;

package check_pkg is
  type log_level_t is (
    null_log_level,

    trace,
    debug,
    pass,
    info,
    warning,
    error,
    failure,

    custom_level1,
    custom_level2,
    custom_level3,
    custom_level4,
    custom_level5,
    custom_level6,
    custom_level7,
    custom_level8);

  constant check_result_tag : string    := "<+/->";
  function result (msg : string := "") return string;

  type edge_t is (rising_edge, falling_edge, both_edges);
  type trigger_event_t is (first_pipe, first_no_pipe, penultimate);

  -----------------------------------------------------------------------------
  -- check_equal
  -----------------------------------------------------------------------------

  procedure check_equal(
    constant got         : in integer;
    constant expected    : in integer;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    variable pass        : out boolean;
    constant got         : in integer;
    constant expected    : in integer;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    constant got         : in std_logic;
    constant expected    : in std_logic;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    variable pass        : out boolean;
    constant got         : in std_logic;
    constant expected    : in std_logic;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    constant got         : in std_logic;
    constant expected    : in boolean;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    variable pass        : out boolean;
    constant got         : in std_logic;
    constant expected    : in boolean;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    constant got         : in boolean;
    constant expected    : in std_logic;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    variable pass        : out boolean;
    constant got         : in boolean;
    constant expected    : in std_logic;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    constant got         : in boolean;
    constant expected    : in boolean;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    variable pass        : out boolean;
    constant got         : in boolean;
    constant expected    : in boolean;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    variable pass        : out boolean;
    constant got         : in string;
    constant expected    : in string;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    constant got         : in character;
    constant expected    : in character;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    variable pass        : out boolean;
    constant got         : in character;
    constant expected    : in character;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  procedure check_equal(
    variable pass        : out boolean;
    constant got         : in time;
    constant expected    : in time;
    constant msg         : in string      := check_result_tag;
    constant level       : in log_level_t := null_log_level;
    constant path_offset : in natural     := 0;
    constant line_num    : in natural     := 0;
    constant file_name   : in string      := "");

  -----------------------------------------------------------------------------

end package;

entity vunit8 is
end entity;

use work.check_pkg.all;
use work.my_logic.all;

architecture test of vunit8 is
begin

    p1: process is
        variable b : boolean;
    begin
        check_equal(true, '1', "");         -- OK
        check_equal(b, '1', "");            -- OK
        wait;
    end process;

end architecture;
