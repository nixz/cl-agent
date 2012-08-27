(defparameter *circle* (list 1 4 7 12 15 20 27))

(defun successor (key)
;; ;; This is the distance between two nodes. 
;; (defun distance(a b)
;;   "Gives the distance between two nodes in any direction (clockwise or anticlockwise)"
;;   (xor a b))

;; the successor is basically replaced by the distance function (i think)
(defun distance (a b)
  (if (= a b)
      nil
      (if (< a b)
          (- b a)
          (+ 31 (- b a)))))

;; # From the start node, find the node responsible
;; # for the target key
;; def findNode(start, key):
;;     current=start
;;     while distance(current.id, key) > \
;;           distance(current.next.id, key):
;;         current=current.next
;;     return current

(defun find-node (start key)
  (let ((current start))
    (loop while (> (distance (id current) key)
                   (distance (next (id (current))) key))
       do (setf current (next current)))
    current))


;; # Find the responsible node and get the value for
;; # the key
;; def lookup(start, key):
;;     node=findNode(start, key)
;;     return node.data[key]

(defun lookup(start key)
  (let ((node (find-node start key))))
    node))

;; # Find the responsible node and store the value
;; # with the key
;; def store(start, key, value):
;;     node=findNode(start, key)
;;     node.data[key]=value

(defun store(&key start key value)
  (let* ((node (find-node start key))
         (setf (data node :key key)) value))))

