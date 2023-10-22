`timescale 1ps/1ps        // OK
`timescale 1ps/1hello     // Error
`timescale 5s/1ps         // Error

module foo;
endmodule // foo
