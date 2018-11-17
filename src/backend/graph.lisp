(in-package :scheduler)

;; Graph stuff
(defparameter *node-table* nil)
(defparameter *edge-table* nil)
(defparameter *VR-definst* nil)
(defparameter *VR-value* nil)
(defparameter *edge-count* nil)

;; Utilities
(defparameter *loads* nil)
(defparameter *last-store* nil)
(defparameter *last-output* nil)
(defparameter *memory* nil)
(defparameter *memory-activity* nil)

(defstruct node
  (inst (ir::make-IR) :type ir::ir)
  (node (ll::make-ll-node) :type ll::ll-node)
  (succ -1 :type fixnum)
  (pred -1 :type fixnum)
  (priority 0 :type fixnum)
  (visited nil :type boolean)
  (dep-left 0 :type fixnum)
  (pred-left 0 :type fixnum)
  (pred-total 0 :type fixnum)
  (exec-time 0 :type fixnum))

(defstruct edge
  (source -1 :type fixnum)
  (sink -1 :type fixnum)
  (next-succ -1 :type fixnum)
  (next-pred -1 :type fixnum)
  (weight 0 :type fixnum)
  (active t :type boolean))

(defun add-succ (n index)
  (let* ((node (aref *node-table* n))
         (succ (node-succ node)))
    (unless (= succ -1)
      (setf (edge-next-succ
                 (aref *edge-table* index))
                succ))
    (setf (node-succ node) index)
    (incf (node-dep-left node))))

(defun add-pred (n index)
  (let* ((node (aref *node-table* n))
         (pred (node-pred node)))
    (unless (= pred -1)
      (setf (edge-next-pred
                 (aref *edge-table* index))
                pred))
    (setf (node-pred node) index)
    (incf (node-pred-left node))
    (incf (node-pred-total node))))

(defun add-edge (source sink)
  (vector-push-extend (make-edge :source source
                                 :sink sink)
                      *edge-table*)
  (add-succ source *edge-count*)
  (add-pred sink *edge-count*)
  (incf *edge-count*))

(defun get-edge-index (source sink)
  (loop for succ in (get-successors source)
     for index = (when (= sink (edge-sink (aref *edge-table* succ)))
                   succ)
     while (null index)
     finally (return index)))

(defun remove-edge (source sink)
  (let* ((index (get-edge-index source sink))
         (edge (aref *edge-table* index)))
    (format t "~a~%" edge)
    (setf (edge-active edge) nil)))

(defun edge-exists? (source sink)
  (let ((exists nil))
    (loop for i = (node-succ (aref *node-table* source))
       then (edge-next-succ (aref *edge-table* i))
       while (and (not (= i -1))
                  (not exists))
       do (when (= sink
                   (edge-sink (aref *edge-table* i)))
            (setf exists t)))
    exists))

(defun add-edge-check (source sink)
  (unless (edge-exists? source sink)
    (vector-push-extend (make-edge :source source
                                   :sink sink)
                        *edge-table*)
    (add-succ source *edge-count*)
    (add-pred sink *edge-count*)
    (incf *edge-count*)))

(defun add-edge-check-small-store (source sink)
  (unless (edge-exists? source sink)
    (vector-push-extend (make-edge :source source
                                   :sink sink
                                   :weight 4)
                        *edge-table*)
    (add-succ source *edge-count*)
    (add-pred sink *edge-count*)
    (incf *edge-count*)))

(defun add-use-edge-check (linum register)
  (let ((v (ir::virtual register)))
    (unless (or (= v -1) (edge-exists? linum (aref *VR-definst* v)))
      (add-edge linum
                (aref *VR-definst* v)))))

(defun add-use-edge (linum register)
  (let ((v (ir::virtual register)))
    (unless (= v -1)
      (add-edge linum
                (aref *VR-definst* v)))))

(defun constructor (ir)
  (let ((size (ll::size ir)))
    (setf *node-table* (make-array size :element-type 'node :initial-element (make-node)
                                   :adjustable nil :displaced-to nil :fill-pointer nil)
          *edge-table* (make-array (* size 4) :fill-pointer 0)
          *VR-definst* (make-array renamer:*VR-name* :element-type 'fixnum :initial-element -1)
          *VR-value* (make-array renamer:*VR-name* :element-type 'fixnum :initial-element -1)
          *edge-count* 0
          *last-output* nil
          *last-store* nil
          *loads* '()
          *memory* (make-array 32768 :element-type 'fixnum :initial-element -1)
          *memory-activity* (make-array 32768 :element-type 'fixnum :initial-element -1))))

(defun shl (x width bits)
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (x width bits)
  (logand (ash x (- bits))
          (1- (ash 1 width))))

(defun get-edge-with-value (n list)
  (loop for i in list
     for num = (when (= n (aref *VR-value* (ir::virtual (ir::r2 (node-inst (aref *node-table* i))))))
                 i)
     while (null num)
     do (format t "~a~%" (ir::r2 (node-inst (aref *node-table* i))))
     finally (return num)))

(defun unknown-value-or-equal (value inst)
  (let* ((node (aref *node-table* inst))
         (instruction (node-inst node))
         (val (aref *VR-value* (ir::virtual (ir::r2 instruction)))))
    (or (= val -1)
        (= val value))))

(defun get-best-store (val)
  (loop for i in *last-store*
     for best = (when (unknown-value-or-equal val i)
                  i)
     while (null best)
     finally (return best)))

(defparameter *debug-inst* nil)

(defun handle-instruction (node linum)
  (let ((instruction (ll::data node)))
    ;; Make a node
    (setf (aref *node-table* linum)
          (make-node :inst instruction
                     :node node))

    ;; Defining instruction for r3
    (let ((def (ir::virtual (ir::r3 instruction))))
      (when (eq :memop (ir::category instruction))
        (setf *debug-inst* (list node linum instruction))
        (let* ((r1 (ir::virtual (ir::r1 instruction)))
               (r1v (aref *VR-value* r1))
               (r2 (ir::virtual (ir::r2 instruction)))
               (r2v (or (= -1 r2) (aref *VR-value* r2)))
               (r3 (ir::virtual (ir::r3 instruction))))
          (cond ((ir::store instruction)
                 ;; For stores, set the memory to the value
                 (unless (= r2v -1)
                   (setf (aref *memory* r2v)
                         r1v)))
                ((not (ir::store instruction))
                 (unless (= r1v -1)
                   (setf (aref *VR-value* r3)
                         (aref *memory* r1v))))
                )))
      (unless (= def -1)
        (setf (aref *VR-definst* def)
              linum)
        (case (ir::category instruction)
          (:loadi (setf (aref *VR-value* def)
                        (ir::constant instruction)))
          (:arithop
           (let ((r1 (ir::virtual (ir::r1 instruction)))
                 (r2 (ir::virtual (ir::r2 instruction))))
             (when (and (eq (ir::category instruction) :arithop)
                        (/= -1 (aref *VR-value* r1))
                        (/= -1 (aref *VR-value* r2)))
               (setf (aref *VR-value* def)
                     (case (ir::opcode instruction)
                       (:|add| (+ (aref *VR-value* r1)
                                  (aref *VR-value* r2)))
                       (:|mult| (* (aref *VR-value* r1)
                                   (aref *VR-value* r2)))
                       (:|sub| (- (aref *VR-value* r1)
                                  (aref *VR-value* r2)))
                       (:|lshift| (ash (aref *VR-value* r1)
                                       (aref *VR-value* r2)))
                       (:|rshift| (ash (aref *VR-value* r1)
                                       (- (aref *VR-value* r2))))
                       (t -1)))))))))

    ;; Slight bug, maybe. If r1 and r2 are the same, then there are 2 edges :/
    (add-use-edge linum (ir::r2 instruction))
    ;; Fixed for now
    (add-use-edge-check linum (ir::r1 instruction))

    ;; IO Edges
    (let ((cat (ir::category instruction)))
      (cond ((eq cat :output)
             ;; For outputs
             ;; Rely on the last store, but only if it stores to the address
             (when *last-store*
               (when-let ((v (get-best-store (ir::constant instruction))))
                 (add-edge-check linum v)))
             (setf (aref *memory-activity* (ir::constant instruction)) linum)
             ;; Required edge for serialized output
             (when *last-output*
               (add-edge-check linum (car *last-output*)))
             (push linum *last-output*))
            ((and (eq cat :memop)
                  (not (ir::store instruction)))
             ;; For loads
             ;; Should only need an edge to the latest store that stores to the value of vr
             (when *last-store*
               (let ((value (aref *VR-value* (ir::virtual (ir::r1 instruction)))))
                 ;; If we don't know the value of vr1 of the load 
                 (if (= value -1)
                     ;; Just grab the last store
                     ;; This however causes a bug to where some stores don't have dependencies
                     ;; To fix this, if a load is from an unknown address, link to all previous stores
                     (progn
                       (loop for i from 0 to (1- (array-dimension *memory-activity* 0))
                          do (setf (aref *memory-activity* i) -1))
                       (add-edge-check linum (car *last-store*)))
                     ;; (mapcar (lambda (x) (add-edge-check linum x))
                     ;;         *last-store*)
                     ;; Find the store that uses the value of vr1
                     (progn
                       (setf (aref *memory-activity* value) linum)
                       (when-let ((v (get-best-store value)))
                         (add-edge-check linum v))))
                 ))
             (push linum *loads*))
            ((eq cat :memop)
             ;; For stores
             (when *last-output*
               (add-edge-check linum (car *last-output*)))
             (when *last-store*
               (let ((value (aref *VR-value* (ir::virtual (ir::r2 instruction)))))
                 ;; If we don't know the value of vr2 of the store
                 (if (= value -1)
                     ;; Just grab the last store
                     (progn
                       (loop for i from 0 to (1- (array-dimension *memory-activity* 0))
                          do (setf (aref *memory-activity* i) -1))
                       (add-edge-check linum (car *last-store*)))
                     ;; Find the store that uses the value of vr2
                     (when-let ((v (get-best-store value)))
                       ;; Also, if there has been no activity (load/output) with the given address
                       (if (and (/= (aref *memory-activity* value) -1)
                                (<= (aref *memory-activity* value) v))
                           (add-edge-check-small-store linum v)
                           (add-edge-check linum v))
                       (setf (aref *memory-activity* value) linum)))
                 )
               ;; (add-edge-check linum (car *last-store*))
             )
             (when *loads*
               (mapcar (lambda (x)
                         (add-edge-check linum x))
                       *loads*))
             (push linum *last-store*))))))

(defun get-leaves ()
  (loop for i from 0 to (1- (array-dimension *node-table* 0))
     when (= -1 (node-succ (aref *node-table* i)))
     collect i))

(defun make-graph (ir)
  (constructor ir)
  (loop for node = (ll::head ir) then (ll::next node)
     while node
     for i from 0
     do (handle-instruction node i))
  (fill-priorities)
  ;; (loop for i from 0 to (1- (array-dimension *node-table* 0))
  ;;    for node = (aref *node-table* i)
  ;;    do
  ;;      (setf (node-dep-left node) (node-dep-total node)))
       )

(defun get-pred-or-succ (n node-fun edge-fun edge-getter)
  (loop for i = (funcall node-fun (aref *node-table* n)) then (funcall edge-fun (aref *edge-table* i))
     while (/= i -1)
     when (edge-active (aref *edge-table* i))
     collect (funcall edge-getter (aref *edge-table* i))))

(defun get-predecessors (n)
  (get-pred-or-succ n #'node-pred #'edge-next-pred #'edge-source))

(defun get-pred-or-succ-struct (n node-fun edge-fun)
  (loop for i = (funcall node-fun (aref *node-table* n)) then (funcall edge-fun (aref *edge-table* i))
     while (/= i -1)
     when (edge-active (aref *edge-table* i))
     collect (aref *edge-table* i)))

(defun get-predecessor-edges (n)
  (get-pred-or-succ-struct n #'node-pred #'edge-next-pred))

(defun get-successors (n)
  (get-pred-or-succ n #'node-succ #'edge-next-succ #'edge-sink))

;; (defun recursively-add-cost (i diff)
;;   (let ((succs (get-successors i)))
;;     (mapcar (lambda (i) (let* ((node (aref *node-table* i))
;;                                (p (+ (node-priority node) diff)))
;;                           (setf (node-priority node) p)
;;                           (recursively-add-cost i diff)))
;;             succs)))

(defun dec-preds (n cost)
  (let ((new-ready '()))
    (loop for i in (get-successors n)
       for node = (aref *node-table* i)
       do (decf (node-pred-left node))
         ;; Maybe if we only take the max
       ;; (setf (node-priority node) (+ (node-priority node) cost))
         (when (< (node-priority node) cost)
           (setf (node-priority node) cost))
         (when (zerop (node-pred-left node))
           (push i new-ready)))
    new-ready))

(defun fill-priorities ()
  (let ((worklist (loop for i from 0 to (1- (array-dimension *node-table* 0))
                     when (= -1 (node-pred (aref *node-table* i)))
                     collect i)))
    (loop for i = (pop worklist)
       while i
       do
         (let ((node (aref *node-table* i)))
           ;; Don't visit nodes twice
           (let* ((prio (node-priority node))
                  (cat (ir::category (node-inst node)))
                  (op (ir::opcode (node-inst node)))
                  (cost (case cat
                          (:memop 5)
                          (:arithop (if (eq op :|mult|) 3 1))
                          (t 1)))
                  (new-prio (+ prio cost)))
             
             ;; Update node with own cost plus previous costs
             (setf (node-visited node) t
                   (node-priority node) new-prio
                   (node-exec-time node) cost)
             
             ;; Add new cost of this node to all nodes of successors
             ;; Add successors to worklist
             ;; Only add the ones who have their predecessors expanded
         (appendf worklist (dec-preds i new-prio)))))))

(defun output-graph-nodes (stream)
  (loop for i from 0 to (1- (array-dimension *node-table* 0))
     do (format stream "	~a [label=\"~a:  ~a~%priority: ~a\"];~%" i (1+ i)
                (ir::string-instruction (node-inst (aref *node-table* i))
                                        #'ir::virtual)
                (node-priority (aref *node-table* i)))))

(defun output-graph-edges (stream)
  (loop for i from 0 to (1- (fill-pointer *edge-table*))
     do (when-let (edge (aref *edge-table* i))
          (when (edge-active edge)
              (format stream "	~a -> ~a [ label=\" ~a\"];~%"
                      (edge-source edge)
                      (edge-sink edge)
                      (if (= 0 (edge-weight edge))
                          ""
                          (edge-weight edge)))))))

(defun output-graph (filename)
  (if (null filename)
      (let ((stream t))
        (format stream "digraph DG {~%")
        (output-graph-nodes stream)
        (output-graph-edges stream)
        (format stream "}~%"))
      (with-open-file (stream (concatenate 'string filename ".before.dot") :direction :output :if-exists :supersede)
        (format stream "digraph DG {~%")
        (output-graph-nodes stream)
        (output-graph-edges stream)
        (format stream "}~%"))))
