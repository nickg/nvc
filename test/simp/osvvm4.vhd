package ScoreboardGenericPkg is
  generic (
    type ExpectedType ;
    type ActualType ;
    function Match(Actual : ActualType ;                           -- defaults
                   Expected : ExpectedType) return boolean ;       -- is "=" ;
    function expected_to_string(A : ExpectedType) return string ;  -- is to_string ;
    function actual_to_string  (A : ActualType) return string      -- is to_string ;
  ) ;
end package;

-------------------------------------------------------------------------------

package AlertLogPkg is
    function MetaMatch (l, r : bit_vector) return boolean ;
    function MetaMatch (l, r : integer) return boolean ;
end package;

-------------------------------------------------------------------------------

package ScoreBoardPkg_slv is new work.ScoreboardGenericPkg
  generic map (
    ExpectedType        => bit_vector,
    ActualType          => bit_vector,
    Match               => work.AlertLogPkg.MetaMatch,  -- "=", [std_logic_vector, std_logic_vector return boolean]
    expected_to_string  => to_hstring, --      [std_logic_vector return string]
    actual_to_string    => to_hstring  --      [std_logic_vector return string]
  ) ;
