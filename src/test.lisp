
(let* ((ir (parser:parse-file "../../../../../students/lab2/code_check_1/cc5.i"))
       (result (renamer:rename-registers ir 1000)))
  (ir::output-virtual ir))
