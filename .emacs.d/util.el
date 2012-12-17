;;; time utility
(defun get-mtime (file-attr)
  (caddr (cdddr file-attr)))

(defun get-diff-days (t1 t2)
  (- (time-to-days t1) (time-to-days t2)))


(provide 'util)
