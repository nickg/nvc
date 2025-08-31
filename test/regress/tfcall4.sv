// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: expr_short_circuit
:description: expression short circuiting test
:type: simulation elaboration parsing
:tags: 11.3.5
*/
module tfcall4();

logic a = 1;
logic b = 1;
logic c = 0;
logic d;

function int fun(logic a);
  $display("FAILED (called fun)");
  $finish;
	return a;
endfunction

initial begin
    d = a && (b || fun(c));
    if (d == 1)
      $display("PASSED");
    else
      $display("FAILED");
end

endmodule
