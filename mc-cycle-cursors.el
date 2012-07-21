(eval-when-compile (require 'cl))

(defun mc/next-cursor-after-point ()
  (let ((pos (point))
        (next-pos (point-max))
        next)
    (mc/for-each-fake-cursor
     (let ((cursor-pos (overlay-get cursor 'point)))
       (when (and (< pos cursor-pos)
                  (< cursor-pos next-pos))
         (setq next-pos cursor-pos)
         (setq next cursor))))
    next))

(defun mc/prev-cursor-before-point ()
  (let ((pos (point))
        (prev-pos (point-min))
        prev)
    (mc/for-each-fake-cursor
     (let ((cursor-pos (overlay-get cursor 'point)))
       (when (and (> pos cursor-pos)
                  (> cursor-pos prev-pos))
         (setq prev-pos cursor-pos)
         (setq prev cursor))))
    prev))

(defun mc/cycle-forward ()
  (interactive)
  (let ((next-cursor (mc/next-cursor-after-point)))
    (unless next-cursor
      (error "We're already at the last cursor"))
    (mc/create-fake-cursor-at-point)
    (mc/pop-state-from-overlay next-cursor)
    (recenter)))

(defun mc/cycle-backward ()
  (interactive)
  (let ((prev-cursor (mc/prev-cursor-before-point)))
    (unless prev-cursor
      (error "We're already at the first cursor"))
    (mc/create-fake-cursor-at-point)
    (mc/pop-state-from-overlay prev-cursor)
    (recenter)))

(define-key mc/keymap (kbd "C-v") 'mc/cycle-forward)
(define-key mc/keymap (kbd "M-v") 'mc/cycle-backward)

(provide 'mc-cycle-cursors)
