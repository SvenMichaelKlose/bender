rm -r obj
mkdir obj
tre src/main.lisp
echo "(unless (string== (fetch-file \"tests/all_instructions.bin\") (fetch-file \"out.prg\")) (error \"Invalid test assembly!\"))" | tre
