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
