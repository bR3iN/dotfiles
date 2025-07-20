;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Utilities for indexed lists implemented as a ring buffer.

(cl-defstruct il
  "Indexed list implemented as a ring buffer.

List with push/pop interface and fixed capacity that silently drops oldest elements
when full. Also has an internal cursor that is reset on modifications."
  ;; Data vector of ring buffer, implicitly encoding the max capacitiy of the indexed list.
  data
  ;; Offset of list head in `DATA'.
  first
  ;; Logical length of list, bounded by `DATA's capacity.
  size
  ;; Offset of cursor in list.
  cursor)

(defun il-create (capacity)
  "Create new indexed list with given CAPACITY."
  (make-il
   :data (make-vector capacity nil)
   :first 0
   :size 0
   :cursor 0))

(defun il-reset (il)
  "Reset internal cursor to point to list tail"
  (setf (il-cursor il) 0))

(defun il-incr (il)
  "Increment ILs internal cursor by 1."
  (let* ((new (1+ (il-cursor il))))
    (if (< new (il-size il))
        (setf (il-cursor il) new)
      (error "no next element"))))

(defun il-decr (il)
  "Decrement ILs internal cursor by 1."
  (if (> (il-cursor il) 0)
      (setf (il-cursor il) (1- (il-cursor il)))
    (error "no previous element")))

(defun il-move (il offset)
  "Move ILs internal cursor by OFFSET."
  ;; FIXME: Could be O(1) instead.
  (while (not (= offset 0))
    (if (< 0 offset)
        (progn
          (il-incr il)
          (setq offset (1- offset)))
      (il-decr il)
      (setq offset (1+ offset)))))

(defun il-capacity (il)
  "Get capacity of IL."
  (length (il-data il)))

(defun il-get (il)
  "Get element of IL referenced by its internal cursor."
  (il-at (- (1+ (il-cursor il))) il)
  ;; (when (and (<= 0 (il-cursor il)) (< (il-cursor il) (il-size il)))
  ;;   (aref (il-data il)
  ;;         (mod (- (+ (il-first il)
  ;;                    (il-size il))
  ;;                 (1+ (il-cursor il)))
  ;;              (il-capacity il))))
  )

(defun il-last (il)
  (il-at -1 il))

(defun il-full-p (il)
  "Indicates if IL is full and old elements will be silently discarded when new elements are added."
  (= (il-size il) (il-capacity il)))

(defun il-push (el il)
  "Push new element onto list."
  (if (il-full-p il)
      (progn
        (aset (il-data il) (il-first il)
              el)
        (setf (il-first il) (mod (1+ (il-first il))
                                 (il-capacity il))))
    (aset (il-data il) (mod (+ (il-first il) (il-size il))
                            (il-capacity il))
          el)
    (setf (il-size il) (1+ (il-size il))))
  (il-reset il))

(defun il-pop (il)
  "Pop list tail."
  (when (= (il-size il) 0)
    (error "list is empty"))
  (il-reset il)
  (prog1
      (il-get il)
    (setf (il-size il) (1- (il-size il)))))

(defun il-at (n il)
  "Get Nth element, starting at 0, bypassing internal cursor. Negative N will wrap around to back of list.

Starts from the front, so il-pop returns `(il-at -1 IL)'"
  ;; Wrap around if negative
  (when (< n 0)
    (setq n (+ (il-size il) n)))
  ;; Bound check so we don't overflow silently
  (when (and (<= 0 n) (< n (il-size il)))
    (aref (il-data il)
          (mod (+ (il-first il) n)
               (il-capacity il)))))

(provide 'il)
