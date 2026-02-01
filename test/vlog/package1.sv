package dummy_pkg;
  wire a;
endpackage : different_name

package constants;
  parameter a = 1;
  parameter b = 2;
  localparam c = 3;
endpackage // constants

import constants::a;

package scope1;
  localparam p1 = a;  // OK
  localparam p2 = b;  // Error
  localparam a = 42;  // OK
endpackage // scope1

import constants::*;

package scope2;
  localparam p1 = c;
endpackage // scope2
