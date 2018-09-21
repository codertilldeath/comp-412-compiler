
(let* ((ir (parser:parse-file "../../../../../students/lab2/code_check_1/cc1.i"))
       (result (renamer:rename-registers ir 10)))
  (ir::output-virtual ir))
