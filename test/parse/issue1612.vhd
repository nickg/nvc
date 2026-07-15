package env is
  type line is access string;

  impure function FILE_NAME return LINE;
  impure function FILE_NAME return STRING;
  impure function FILE_PATH return LINE;
  impure function FILE_PATH return STRING;
end package;

package FileLinePathPkg is
  -- Alias's to std.env are not ambiguous just like for ieee.std_logic_textio
  alias FILE_NAME is work.env.FILE_NAME [return string] ;
  alias FILE_PATH is work.env.FILE_PATH [return string] ;
end package;

use work.FileLinePathPkg ;

entity FilePathAlias is
end FilePathAlias ;
architecture test of FilePathAlias is
  constant RawTestFilePath : string  := work.FileLinePathPkg.FILE_PATH ;  -- OK
begin
end test ;
