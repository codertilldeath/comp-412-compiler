(in-package :scheduler)

;; Graph stuff
(defparameter *node-table* nil)
(defparameter *edge-table* nil)
(defparameter *VR-definst* nil)
(defparameter *edge-count* nil)

;; Utilities
(defparameter *loads* nil)
(defparameter *last-store* nil)
(defparameter *last-output* nil)

(defstruct node
  (inst (ir::make-IR) :type ir::ir)
  (node (ll::make-ll-node) :type ll::ll-node)
  (succ -1 :type fixnum)
  (pred -1 :type fixnum))

(defstruct edge
  (source -1 :type fixnum)
  (sink -1 :type fixnum)
  (next-succ -1 :type fixnum)
  (next-pred -1 :type fixnum))



(defun add-succ (n index)
  (let* ((node (aref *node-table* n))
         (succ (node-succ node)))
    (unless (= succ -1)
      (setf (edge-next-succ
                 (aref *edge-table* index))
                succ))
    (setf (node-succ node) index)))

(defun add-pred (n index)
  (let* ((node (aref *node-table* n))
         (pred (node-pred node)))
    (unless (= pred -1)
      (setf (edge-next-pred
                 (aref *edge-table* index))
                pred))
    (setf (node-pred node) index)))

(defun add-edge (source sink)
  (vector-push-extend (make-edge :source source
                                 :sink sink)
                      *edge-table*)
  (add-succ source *edge-count*)
  (add-pred sink *edge-count*)
  (incf *edge-count*))

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
          *edge-count* 0
          *last-output* nil
          *last-store* nil
          *loads* '())))


(defun handle-instruction (node linum)
  (let ((instruction (ll::data node)))
    (setf (aref *node-table* linum)
          (make-node :inst instruction
                     :node node))
    (let ((def (ir::virtual (ir::r3 instruction))))
      (unless (= def -1)
        (setf (aref *VR-definst* def)
              linum)))
    (add-use-edge linum (ir::r2 instruction))
    (add-use-edge linum (ir::r1 instruction))
    (let ((cat (ir::category instruction)))
      (cond ((eq cat :output)
             (when *last-store*
               (add-edge-check linum *last-store*))
             (when *last-output*
               (add-edge-check linum *last-output*))
             (setf *last-output* linum))
            ((and (eq cat :memop)
                  (not (ir::store instruction)))
             (when *last-store*
               (add-edge-check linum *last-store*))
             (push linum *loads*))
            ((eq cat :memop)
             (when *last-output*
               (add-edge-check linum *last-output*))
             (when *last-store*
               (add-edge-check linum *last-store*))
             (when *loads*
               (mapcar (lambda (x)
                         (add-edge-check linum x))
                       *loads*))
             (setf *last-store* linum))))))

(defun make-graph (ir)
  (constructor ir)
  (loop for node = (ll::head ir) then (ll::next node)
     while node
     for i from 0
     do (handle-instruction node i)))

(defun output-graph-nodes (stream)
  (loop for i from 0 to (1- (array-dimension *node-table* 0))
     do (format stream "	~a [label=\"~a:  ~a\"];~%" i i
                (ir::string-instruction (node-inst (aref *node-table* i))
                                        #'ir::virtual))))

(defun output-graph-edges (stream)
  (loop for i from 0 to (1- (fill-pointer *edge-table*))
     do (let ((edge (aref *edge-table* i)))
          (format stream "	~a -> ~a;~%"
                  (edge-source edge)
                  (edge-sink edge)))))

(defun output-graph (filename)
  (with-open-file (stream (concatenate 'string filename ".before.dot") :direction :output)
    (format stream "digraph DG {~%")
    (output-graph-nodes stream)
    (output-graph-edges stream)
    (format stream "}~%")))
