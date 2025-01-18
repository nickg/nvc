proc proc2 args {
    error "boo"
}

proc proc1 args {
    proc2
}

proc1
