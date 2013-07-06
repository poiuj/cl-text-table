cl-text-table
=============

Library for printing text tables.


Example
=======

For instance, we have input file test-table.tbl:
```
first;second;third
1;2;3
1;1;2
3;2;3
```

Parse input file:
```lisp
(setf my-table (txt-tbl:parse-file "test-table.tdl"))
```

After that, we can dump it to file:
```lisp
(with-open-file (s "result-table.txt" :direction :output)
    (txt-tbl:print-table my-table :stream s))
```

The result is:
```
 first | second | third |
=========================
   1   |    2   |   3   |
   1   |    1   |   2   |
   3   |    2   |   3   |
```
