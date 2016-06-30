(defcustom vdiff-lock-scrolling t
  "Whether to lock scrolling by default when starting
  `vdiff-mode'.")

(defcustom vdiff-diff-program "diff"
  "diff program to use.")

(defcustom vdiff-diff-program-args ""
  "Extra arguments to pass to diff. If this is set wrong, you may
break vdiff.")

(defcustom vdiff-copied-commands '(next-line
                                   previous-line
                                   evil-next-line
                                   evil-previous-line
                                   beginning-of-buffer
                                   end-of-buffer)
  "Commands that should be executed in other vdiff buffer to keep
lines in sync.")

(defvar vdiff-buffers nil)
(defvar vdiff-temp-files (list (make-temp-file "vdiff-temp-a-")
                               (make-temp-file "vdiff-temp-b-")))
(defvar vdiff-process-buffer " *vdiff*")
(defvar vdiff-diff-data nil)
(defvar vdiff-diff-code-regexp
  "^\\([0-9]+\\),?\\([0-9]+\\)?\\([adc]\\)\\([0-9]+\\),?\\([0-9]+\\)?")
(defvar vdiff-inhibit-window-switch nil)
(defvar vdiff-scroll-command-cnt 0)
(defvar vdiff-inhibit-sync nil)
(defvar vdiff-line-map nil)

;; * Utilities

(defun vdiff-maybe-int (str)
  (when str (string-to-int str)))

(defun vdiff-buffer-a-p ()
  (eq (current-buffer) (car vdiff-buffers)))

(defun vdiff-buffer-b-p ()
  (eq (current-buffer) (cadr vdiff-buffers)))

(defun vdiff-buffer-p ()
  (memq (current-buffer) vdiff-buffers))

(defun vdiff-other-buffer ()
  (if (vdiff-buffer-a-p)
      (cadr vdiff-buffers)
    (car vdiff-buffers)))

(defun vdiff-other-window ()
  (get-buffer-window (vdiff-other-buffer)))

(defun vdiff-move-to-line (n)
  (goto-char (point-min))
  (forward-line (1- n)))

(defun vdiff-overlay-at-point (&optional prior pos)
  (let ((pos (or pos (point))))
    (catch 'yes
      (dolist (ovr (overlays-at
                    (if prior
                        (max (point-min)
                             (1- pos))
                      pos)))
        (when (overlay-get ovr 'vdiff-type)
          (throw 'yes ovr))))))

(defun vdiff-overlays-in-region (beg end)
  (let (ovrs)
    (dolist (ovr (overlays-in beg end))
      (when (overlay-get ovr 'vdiff-type)
        (push ovr ovrs)))
    (nreverse ovrs)))

(defun vdiff-maybe-exit-overlay (&optional up)
  (let* ((ovr (vdiff-overlay-at-point up))
         (type (when ovr (overlay-get ovr 'vdiff-type))))
    (when (memq type '(addition change))
      (let ((range (overlay-get ovr 'vdiff-range)))
        (goto-char (if up (car range) (cdr range)))))))

(defmacro vdiff-with-other-window (&rest body)
  `(when (and (vdiff-buffer-p)
              (not vdiff-inhibit-window-switch)
              (vdiff-other-window))
     (setq vdiff-inhibit-window-switch t)
     (save-selected-window
       (unwind-protect
           (progn
             (select-window (vdiff-other-window))
             ,@body)
         (setq vdiff-inhibit-window-switch nil)))))

(defmacro vdiff-with-both-buffers (&rest body)
  `(when (and (buffer-live-p (car vdiff-buffers))
              (buffer-live-p (cadr vdiff-buffers)))
     (dolist (buf vdiff-buffers)
       (with-current-buffer buf
         ,@body))))

(defun vdiff-refresh ()
  "Asynchronously refresh diff information."
  (interactive)
  (let* ((cmd (mapconcat #'identity
                         (list
                          vdiff-diff-program
                          vdiff-diff-program-args
                          (car vdiff-temp-files)
                          (cadr vdiff-temp-files))
                         " "))
         proc)
    (with-current-buffer (car vdiff-buffers)
      (write-region nil nil (car vdiff-temp-files)))
    (with-current-buffer (cadr vdiff-buffers)
      (write-region nil nil (cadr vdiff-temp-files)))
    (with-current-buffer (get-buffer-create vdiff-process-buffer)
      (erase-buffer))
    (when proc
      (kill-process proc))
    (setq proc (start-process-shell-command
                vdiff-process-buffer
                vdiff-process-buffer
                cmd))
    (set-process-sentinel proc #'vdiff-diff-refresh-1)))

(defun vdiff-diff-refresh-1 (proc event)
  (cond ((string-match-p "exited abnormally with code 1" event)
         (setq vdiff-diff-data nil)
         (let (res)
           (with-current-buffer vdiff-process-buffer
             (goto-char (point-min))
             (while (re-search-forward vdiff-diff-code-regexp nil t)
               (push (list (match-string 3)
                           (cons (vdiff-maybe-int (match-string 1))
                                 (vdiff-maybe-int (match-string 2)))
                           (cons (vdiff-maybe-int (match-string 4))
                                 (vdiff-maybe-int (match-string 5))))
                     res)))
           (setq vdiff-diff-data (nreverse res)))
         (vdiff-refresh-overlays))
        ((string= event "finished\n"))
        ((string-match-p "exited abnormally with code" event)
         (message "vdiff process error: %s" event))))

(defun vdiff-remove-all-overlays ()
  (vdiff-with-both-buffers (remove-overlays)))

(defun vdiff-save-buffers ()
  (interactive)
  (vdiff-with-both-buffers (save-buffer)))

;; * Add overlays

(defun vdiff-add-subtraction-overlays
    (buffer start-line target-range amount)
  (with-current-buffer buffer
    (vdiff-move-to-line start-line)
    (end-of-line)
    (let ((position (1+ (point)))
          text)
      (dotimes (i amount)
        (push
         (make-string
          (1- (window-width (get-buffer-window buffer)))
          ?-)
         text))
      (let ((ovr (make-overlay position (1+ position))))
        (overlay-put ovr 'before-string 
                     (propertize
                      (concat
                       (mapconcat #'identity text "\n")
                       "\n")
                      'face '(:background "#440000")))
        (overlay-put ovr 'vdiff-type 'subtraction) 
        (overlay-put ovr 'vdiff-target-range target-range)))))

(defun vdiff-add-change-overlays
    (buffer start-line lines target-range &optional addition)
  (with-current-buffer buffer
    (vdiff-move-to-line start-line)
    (let ((beg (point))
          (end (progn (forward-line lines)
                      (point))))
      (let ((ovr (make-overlay beg end)))
        (overlay-put ovr 'face (if addition
                                   '(:background "#004422")
                                 '(:background "#353500")))
        (overlay-put ovr 'vdiff-type (if addition
                                            'addition
                                          'change))
        (overlay-put ovr 'vdiff-range (cons beg end))
        (overlay-put ovr 'vdiff-target-range target-range)))))

(defun vdiff-refresh-overlays ()
  (vdiff-remove-all-overlays)
  (vdiff-refresh-line-maps)
  (save-excursion
    (dolist (header vdiff-diff-data)
      (let* ((code (nth 0 header))
             (a-buffer (car vdiff-buffers))
             (b-buffer (cadr vdiff-buffers))
             (a-range (nth 1 header))
             (a-beg (car a-range))
             (a-end (if (cdr-safe a-range)
                        (cdr a-range)
                      (car a-range)))
             (a-norm-range (cons a-beg a-end))
             (a-length (1+ (- a-end a-beg)))
             (b-range (nth 2 header))
             (b-beg (car b-range))
             (b-end (if (cdr-safe b-range)
                        (cdr b-range)
                      (car b-range)))
             (b-norm-range (cons b-beg b-end))
             (b-length (1+ (- b-end b-beg))))
        (cond ((string= code "d")
               (vdiff-add-subtraction-overlays
                b-buffer b-beg a-norm-range a-length)
               (vdiff-add-change-overlays
                a-buffer a-beg a-length b-norm-range t))
              ((string= code "a")
               (vdiff-add-subtraction-overlays
                a-buffer a-beg b-norm-range b-length)
               (vdiff-add-change-overlays
                b-buffer b-beg b-length a-norm-range t))
              ((and (string= code "c") (> a-length b-length))
               (vdiff-add-change-overlays
                a-buffer a-beg a-length b-norm-range)
               (vdiff-add-change-overlays
                b-buffer b-beg b-length a-norm-range)
               (vdiff-add-subtraction-overlays
                b-buffer b-end nil (- a-length b-length)))
              ((and (string= code "c") (< a-length b-length))
               (vdiff-add-change-overlays
                a-buffer a-beg a-length b-norm-range)
               (vdiff-add-change-overlays
                b-buffer b-beg b-length a-norm-range)
               (vdiff-add-subtraction-overlays
                a-buffer a-end nil (- b-length a-length)))
              ((string= code "c")
               (vdiff-add-change-overlays
                a-buffer a-beg a-length b-norm-range)
               (vdiff-add-change-overlays
                b-buffer b-beg b-length a-norm-range)))))))

;; * Moving changes

(defun vdiff-send-changes (beg end &optional receive)
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (if (or (= (line-number-at-pos) 1)
                   (vdiff-overlay-at-point
                    nil (line-beginning-position)))
               (line-beginning-position)
             (save-excursion
               (forward-line -1)
               (line-beginning-position)))
           (line-end-position))))
  (let* ((ovrs (overlays-in beg end)))
    (dolist (ovr ovrs)
      (cond ((memq (overlay-get ovr 'vdiff-type)
                   '(change addition))
             (vdiff-transmit-change-overlay ovr receive))
            ((eq (overlay-get ovr 'vdiff-type) 'subtraction)
             (vdiff-transmit-subtraction-overlay ovr receive))))
    (vdiff-refresh)))

(defun vdiff-receive-changes (beg end)
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (if (or (= (line-number-at-pos) 1)
                   (vdiff-overlay-at-point
                    nil (line-beginning-position)))
               (line-beginning-position)
             (save-excursion
               (forward-line -1)
               (line-beginning-position)))
           (line-end-position))))
  (vdiff-send-changes beg end t))

(defun vdiff-transmit-change-overlay (chg-ovr &optional receive)
  (cond ((not (overlayp chg-ovr))
         (message "No change found"))
        (receive
         (let* ((target-rng (overlay-get chg-ovr 'vdiff-target-range))
                (pos (vdiff-pos-at-line-beginning
                      (car target-rng) (vdiff-other-buffer))))
           (vdiff-with-other-window
            (vdiff-send-changes pos (1+ pos)))))
        (t
         (let* ((addition (eq 'addition (overlay-get chg-ovr 'vdiff-type)))
                (target-rng (overlay-get chg-ovr 'vdiff-target-range))
                (text (buffer-substring-no-properties
                       (overlay-start chg-ovr)
                       (overlay-end chg-ovr))))
           (with-current-buffer (vdiff-other-buffer)
             (if addition
                 (vdiff-move-to-line (1+ (car target-rng)))
               (vdiff-move-to-line (car target-rng))
               (delete-region (point)
                              (save-excursion
                                (forward-line
                                 (1+ (- (cdr target-rng)
                                        (car target-rng))))
                                (point))))
             (insert text))))))

(defun vdiff-transmit-subtraction-overlay (sub-ovr &optional receive)
  (cond ((not (overlayp sub-ovr))
         (message "No change found"))
        (receive
         (let* ((target-rng (overlay-get sub-ovr 'vdiff-target-range))
                (pos (vdiff-pos-at-line-beginning
                      (car target-rng) (vdiff-other-buffer))))
           (vdiff-with-other-window
            (vdiff-send-changes pos (1+ pos)))))
        (t
         (let* ((target-rng
                 (overlay-get sub-ovr 'vdiff-target-range)))
           (when target-rng
             (with-current-buffer (vdiff-other-buffer)
               (vdiff-move-to-line (car target-rng))
               (delete-region (point)
                              (save-excursion
                                (vdiff-move-to-line
                                 (1+ (cdr target-rng)))
                                (point)))))))))

;; * Scrolling and line syncing

(defun vdiff-refresh-line-maps ()
  (let (new-map)
    (dolist (entry vdiff-diff-data)
      (let* ((code (car entry))
             (a-lines (nth 1 entry))
             (a-beg (car a-lines))
             (a-end (or (cdr-safe a-lines)
                        (car a-lines)))
             ;; (a-length (1+ (- a-end a-beg)) )
             (b-lines (nth 2 entry))
             (b-beg (car b-lines))
             (b-end (or (cdr-safe b-lines)
                        (car b-lines)))
             ;; (b-length (1+ (- b-end b-beg)))
             (delta 0))
        (cond ((string= code "d")
               (push (cons (1- a-beg) b-beg) new-map)
               (push (cons (1+ a-end) (1+ b-end)) new-map))
              ((string= code "a")
               (push (cons a-beg (1- b-beg)) new-map)
               (push (cons (1+ a-end) (1+ b-end)) new-map))
              ((and (string= code "c"))
               (push (cons (1- a-beg) (1- b-beg)) new-map)
               (push (cons (1+ a-end) (1+ b-end)) new-map)))))
    (setq vdiff-line-map (nreverse new-map))))

(defun vdiff-translate-line (line &optional B-to-A)
  (let ((nearest-line
         (catch 'closest
           (let (closest)
             (dolist (entry vdiff-line-map)
               (if (> (if B-to-A
                          (cdr entry)
                        (car entry)) line)
                   (throw 'closest closest)
                 (setq closest entry)))
             (throw 'closest closest)))))
    (cond ((and nearest-line B-to-A)
           (+ (- line (cdr nearest-line)) (car nearest-line)))
          (nearest-line
           (+ (- line (car nearest-line)) (cdr nearest-line)))
          (t line))))

(defun vdiff-goto-corresponding-line (line in-b)
  (interactive (list (line-number-at-pos)
                     (not (vdiff-buffer-a-p))))
  (vdiff-refresh)
  (let* ((new-line (vdiff-translate-line line in-b))
         (new-pos (vdiff-pos-at-line-beginning new-line)))
    (select-window (vdiff-other-window))
    (goto-char new-pos)))

(defun vdiff-sync-line (line in-a)
  (interactive (list (line-number-at-pos)
                     (not (vdiff-buffer-a-p))))
  (let ((new-line (vdiff-translate-line
                   line (not in-a)))
        (other-buffer (vdiff-other-buffer))
        (other-window (vdiff-other-window)))
    (set-window-point
     other-window
     (vdiff-pos-at-line-beginning new-line other-buffer))))

(defun vdiff-sync-and-center ()
  (interactive)
  (vdiff-sync-line (line-number-at-pos) (vdiff-buffer-a-p))
  (recenter)
  (vdiff-with-other-window
   (recenter)))

(defun vdiff-pos-at-line-beginning (line &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (vdiff-move-to-line line)
      (line-beginning-position))))

(defun vdiff-scroll-other (window window-start)
  (let ((win-a (get-buffer-window (car vdiff-buffers)))
        (win-b (get-buffer-window (nth 1 vdiff-buffers))))
    (when (and (eq window (selected-window))
               (window-live-p win-a)
               (window-live-p win-b)
               (memq window (list win-a win-b)))
      (let* ((in-b (eq window win-b))
             (this-window (if in-b win-b win-a))
             (other-window (if in-b win-a win-b))
             (other-buffer (if in-b (car vdiff-buffers)
                             (nth 1 vdiff-buffers)))
             (this-line (line-number-at-pos (point)))
             (other-line (vdiff-translate-line
                          this-line in-b))
             (this-start  (line-number-at-pos window-start))
             (other-start (vdiff-translate-line
                           this-start in-b))
             (other-start-pos (vdiff-pos-at-line-beginning
                               other-start other-buffer))
             other-pos)
        (set-window-start other-window other-start-pos)
        (vdiff-with-other-window
         (goto-char (vdiff-pos-at-line-beginning other-line)))))))

(defun vdiff-toggle-lock ()
  (interactive)
  (with-current-buffer (car vdiff-buffers)
    (vdiff-sync-line (line-number-at-pos) t)
    (scroll-all-mode)))


(defun vdiff-sync-scroll ()
  ;; Use real-this-command because evil-next-line and evil-previous-line pretend
  ;; they are next-line and previous-line
  (when (and (memq real-this-command vdiff-copied-commands)
             (not vdiff-inhibit-sync))
    (let ((this-line (line-number-at-pos))
          ;; This is necessary to not screw up the cursor column after calling
          ;; next-line or previous-line again from the other buffer
          temporary-goal-column)
      (vdiff-with-other-window
       (ignore-errors
         (let ((vdiff-inhibit-sync t))
           (call-interactively real-this-command))
         (if (< vdiff-scroll-command-cnt 40)
             (incf vdiff-scroll-command-cnt)
           ;; (message "syncing lines")
           (setq vdiff-scroll-command-cnt 0)
           (vdiff-move-to-line
            (vdiff-translate-line
             this-line (vdiff-buffer-a-p)))))))))

;; * Movement

(defun vdiff-next-change ()
  (interactive)
  (vdiff-maybe-exit-overlay)
  (let ((next (next-overlay-change (point))))
    (if (= next (point-max))
        (message "No more changes")
      (goto-char next)
      (while (and (not (eobp))
                  (not (vdiff-overlay-at-point)))
        (goto-char next)
        (setq next (next-overlay-change (point))))
      (vdiff-sync-and-center))))

(defun vdiff-previous-change ()
  (interactive)
  (vdiff-maybe-exit-overlay t)
  (let ((next (previous-overlay-change (point))))
    (if (= next (point-min))
        (message "No more changes")
      (goto-char next)
      (while (and (not (bobp))
                  (not (vdiff-overlay-at-point)))
        (goto-char next)
        (setq next (previous-overlay-change (point))))
      (vdiff-sync-and-center))))

;; * Entry points
 
(defun vdiff-files (A B &optional horizontal)
  (interactive (let ((file-a (read-file-name "File 1: ")))
                 (list
                  file-a
                  (read-file-name
                   (format "[File 1 %s] File 2: "
                           (file-name-nondirectory file-a)))
                  current-prefix-arg)))
  (let (a-window b-window a-buffer)
    (delete-other-windows)
    (find-file A)
    (goto-char (point-min))
    (setq a-buffer (current-buffer))
    (save-selected-window
      (setq b-window (if horizontal
                         (split-window-vertically)
                       (split-window-horizontally)))
      (find-file-other-window B)
      (setq vdiff-buffers (list a-buffer (window-buffer b-window)))
      (vdiff-with-both-buffers
       (vdiff-mode 1))
      (vdiff-refresh))))

(defvar vdiff-mode-map (make-sparse-keymap))
(define-key vdiff-mode-map "\C-l"  'vdiff-sync-and-center)
(define-key vdiff-mode-map "\C-cg" 'vdiff-goto-corresponding-line)
(define-key vdiff-mode-map "\C-cn" 'vdiff-next-change)
(define-key vdiff-mode-map "\C-cp" 'vdiff-previous-change)
(define-key vdiff-mode-map "\C-cs" 'vdiff-send-changes)
(define-key vdiff-mode-map "\C-cr" 'vdiff-receive-changes)

(define-minor-mode vdiff-mode
  " "
  nil " VDIFF" 'vdiff-mode-map
  (if vdiff-mode
      (progn
        (setq cursor-in-non-selected-windows nil)
        (add-hook 'after-save-hook #'vdiff-refresh nil t)
        (when vdiff-lock-scrolling
          (vdiff-scroll-lock-mode 1)))
    (setq cursor-in-non-selected-windows t)
    (setq vdiff-diff-data nil)
    (vdiff-remove-all-overlays)
    (remove-hook 'after-save-hook #'vdiff-refresh t)
    (when vdiff-scroll-lock-mode
      (vdiff-scroll-lock-mode -1))))

(define-minor-mode vdiff-scroll-lock-mode
  " "
  nil nil nil
  (if vdiff-scroll-lock-mode
      (progn
        (unless vdiff-mode
          (vdiff-mode 1))
        (message "Scrolling locked")
        (vdiff-with-both-buffers
         (add-hook 'window-scroll-functions #'vdiff-scroll-other nil t)
         (add-hook 'post-command-hook #'vdiff-sync-scroll nil t)))
    (message "Scrolling unlocked")
    (vdiff-with-both-buffers
     (remove-hook 'after-save-hook #'vdiff-refresh t)
     (remove-hook 'window-scroll-functions #'vdiff-scroll-other t))))
