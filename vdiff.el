(defvar vdiff-buffer-a nil)
(defvar vdiff-buffer-b nil)
(defvar vdiff-process-buffer " *vdiff*")
(defvar vdiff-diff-data nil)
(defvar vdiff-diff-data-w-text nil)
(defvar vdiff-diff-code-regexp
  "^\\([0-9]+\\),?\\([0-9]+\\)?\\([adc]\\)\\([0-9]+\\),?\\([0-9]+\\)?")
(defvar vdiff-diff-program (executable-find "diff"))

;; * Utilities

(defun vdiff-maybe-int (str)
  (when str (string-to-int str)))

(defun vdiff-buffer-a-p ()
  (eq (current-buffer) vdiff-buffer-a))

(defun vdiff-buffer-b-p ()
  (eq (current-buffer) vdiff-buffer-b))

(defun vdiff-buffer-p ()
  (memq (current-buffer) (list vdiff-buffer-a vdiff-buffer-b)))

(defun vdiff-other-buffer ()
  (if (vdiff-buffer-a-p) vdiff-buffer-b vdiff-buffer-a))

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

(defvar vdiff-inhibit-window-switch nil)

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

;; (defun vdiff-diff-sentinel (process event)
;;   (cond ((string= event "finished\n"))
;;         ((string-match "exited abnormally with code \\([0-9]+\\)\n" event)
;;          (user-error "%s" event))))


(defun vdiff-parse-diff-output ()
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
    (setq vdiff-diff-data (nreverse res))))

(defun vdiff-calc-diff ()
  (let ((file-a (make-temp-file "vdiff-a-"))
        (file-b (make-temp-file "vdiff-b-"))
        line-string hunk-header a-lines b-lines res
        message-log-max)
    (with-current-buffer vdiff-buffer-a
      (write-region nil nil file-a))
    (with-current-buffer vdiff-buffer-b
      (write-region nil nil file-b))
    (with-current-buffer (get-buffer-create vdiff-process-buffer)
      (erase-buffer))
    (call-process vdiff-diff-program
                  nil vdiff-process-buffer nil
                  file-a file-b)
    (vdiff-parse-diff-output)))

(defun vdiff-remove-all-overlays ()
  (with-current-buffer vdiff-buffer-a
    (remove-overlays))
  (with-current-buffer vdiff-buffer-b
    (remove-overlays)))

(defun vdiff-save-buffers ()
  (with-current-buffer vdiff-buffer-a
    (save-buffer))
  (with-current-buffer vdiff-buffer-b
    (save-buffer)))

;; * Add overlays

(defvar vdiff-subtraction-overlays '())
(defvar vdiff-change-overlays '())

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
          (1- (window-width (get-buffer-window vdiff-buffer-a))) ?-)
         text))
      (let ((ovr (make-overlay position (1+ position))))
        (overlay-put ovr 'before-string 
                     (propertize
                      (concat
                       (mapconcat #'identity text "\n")
                       "\n")
                      'face '(:background "#440000")))
        (overlay-put ovr 'vdiff-type 'subtraction) 
        (overlay-put ovr 'vdiff-target-range target-range)
        (push ovr vdiff-subtraction-overlays)))))

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
        (overlay-put ovr 'vdiff-target-range target-range)
        (push ovr vdiff-change-overlays)))))

(defun vdiff-refresh-diff-overlays ()
  (interactive)
  (vdiff-remove-all-overlays)
  (vdiff-calc-diff)
  (vdiff-refresh-line-maps)
  (save-excursion
    (dolist (header vdiff-diff-data)
      (let* ((code (nth 0 header))
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
                vdiff-buffer-b b-beg a-norm-range a-length)
               (vdiff-add-change-overlays
                vdiff-buffer-a a-beg a-length b-norm-range t))
              ((string= code "a")
               (vdiff-add-subtraction-overlays
                vdiff-buffer-a a-beg b-norm-range b-length)
               (vdiff-add-change-overlays
                vdiff-buffer-b b-beg b-length a-norm-range t))
              ((and (string= code "c") (> a-length b-length))
               (vdiff-add-change-overlays
                vdiff-buffer-a a-beg a-length b-norm-range)
               (vdiff-add-change-overlays
                vdiff-buffer-b b-beg b-length a-norm-range)
               (vdiff-add-subtraction-overlays
                vdiff-buffer-b b-end nil (- a-length b-length)))
              ((and (string= code "c") (< a-length b-length))
               (vdiff-add-change-overlays
                vdiff-buffer-a a-beg a-length b-norm-range)
               (vdiff-add-change-overlays
                vdiff-buffer-b b-beg b-length a-norm-range)
               (vdiff-add-subtraction-overlays
                vdiff-buffer-a a-end nil (- b-length a-length)))
              ((string= code "c")
               (vdiff-add-change-overlays
                vdiff-buffer-a a-beg a-length b-norm-range)
               (vdiff-add-change-overlays
                vdiff-buffer-b b-beg b-length a-norm-range)))))))

;; * Moving changes

(defun vdiff-push-changes (beg end &optional pull)
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
             (vdiff-push-pull-change-overlay ovr pull))
            ((eq (overlay-get ovr 'vdiff-type) 'subtraction)
             (vdiff-push-pull-subtraction-overlay ovr pull))))
    (vdiff-refresh-diff-overlays)))

(defun vdiff-pull-changes (beg end)
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
  (vdiff-push-changes beg end t))

(defun vdiff-push-pull-change-overlay (chg-ovr &optional pull)
  (cond ((not (overlayp chg-ovr))
         (message "No change found"))
        (pull
         (let* ((target-rng (overlay-get chg-ovr 'vdiff-target-range))
                (pos (vdiff-pos-at-line-beginning
                      (car target-rng) (vdiff-other-buffer))))
           (vdiff-with-other-window
            (vdiff-push-changes pos (1+ pos)))))
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

(defun vdiff-push-pull-subtraction-overlay (sub-ovr &optional pull)
  (cond ((not (overlayp sub-ovr))
         (message "No change found"))
        (pull
         (let* ((target-rng (overlay-get sub-ovr 'vdiff-target-range))
                (pos (vdiff-pos-at-line-beginning
                      (car target-rng) (vdiff-other-buffer))))
           (vdiff-with-other-window
            (vdiff-push-changes pos (1+ pos)))))
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

(defvar vdiff-line-map nil)

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
  (vdiff-refresh-diff-overlays)
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

(defvar vdiff-window-a-start nil)
(defvar vdiff-window-b-start nil)

(defun vdiff-pos-at-line-beginning (line &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (vdiff-move-to-line line)
      (line-beginning-position))))

(defun vdiff-scroll-other (window window-start)
  (let ((win-a (get-buffer-window vdiff-buffer-a))
        (win-b (get-buffer-window vdiff-buffer-b)))
    (when (and (eq window (selected-window))
               (window-live-p win-a)
               (window-live-p win-b)
               (memq window (list win-a win-b)))
      (let* ((in-b (eq window win-b))
             (this-window (if in-b win-b win-a))
             (other-window (if in-b win-a win-b))
             (other-buffer (if in-b vdiff-buffer-a
                             vdiff-buffer-b))
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
  (with-current-buffer vdiff-buffer-a
    (vdiff-sync-line (line-number-at-pos) t)
    (scroll-all-mode)))

(defvar vdiff-scroll-command-cnt 0)
(defvar vdiff-copied-commands '(next-line
                                previous-line
                                evil-next-line
                                evil-previous-line
                                beginning-of-buffer
                                end-of-buffer))
(defvar vdiff-inhibit-sync nil)

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
  (let (a-window b-window)
    (delete-other-windows)
    (find-file A)
    (goto-char (point-min))
    (setq vdiff-buffer-a (current-buffer))
    (setq b-window (if horizontal
                       (split-window-vertically)
                     (split-window-horizontally)))
    (find-file-other-window B)
    (setq vdiff-buffer-b (window-buffer b-window))
    (setq vdiff-window-a-start
          (window-start (get-buffer-window vdiff-buffer-a)))
    (setq vdiff-window-b-start (window-start b-window))
    (with-current-buffer vdiff-buffer-a
      (vdiff-mode 1))
    (with-current-buffer vdiff-buffer-b
      (vdiff-mode 1))
    (vdiff-refresh-diff-overlays)))

(defvar vdiff-mode-map (make-sparse-keymap))
(define-key vdiff-mode-map "\C-l"  'vdiff-sync-and-center)
(define-key vdiff-mode-map "\C-cg" 'vdiff-goto-corresponding-line)
(define-key vdiff-mode-map "\C-cn" 'vdiff-next-change)
(define-key vdiff-mode-map "\C-cp" 'vdiff-previous-change)
(define-key vdiff-mode-map "\C-ct" 'vdiff-push-changes)
(define-key vdiff-mode-map "\C-co" 'vdiff-pull-changes)

(define-minor-mode vdiff-mode
  " "
  nil " VDIFF" 'vdiff-mode-map
  (if vdiff-mode
      (progn
        ;; (add-hook 'evil-insert-state-exit-hook
        ;;           #'vdiff-refresh-diff-overlays)
        (setq cursor-in-non-selected-windows nil)
        (add-hook 'after-save-hook #'vdiff-refresh-diff-overlays nil t)
        (add-hook 'window-scroll-functions #'vdiff-scroll-other)
        (add-hook 'post-command-hook #'vdiff-sync-scroll)
        )
    ;; (add-hook 'evil-insert-state-exit-hook
    ;;           #'vdiff-refresh-diff-overlays)
    (setq cursor-in-non-selected-windows t)
    (setq vdiff-diff-data nil)
    (setq vdiff-diff-data-w-text nil)
    (vdiff-remove-all-overlays)
    (remove-hook 'post-command-hook #'vdiff-sync-scroll)
    (remove-hook 'after-save-hook #'vdiff-refresh-diff-overlays t)
    (remove-hook 'window-scroll-functions #'vdiff-scroll-other)))

;; (evil-define-minor-mode-key 'normal vdiff-mode
;;   (kbd "<up>") 'vdiff-previous-change
;;   (kbd "<right>") 'vdiff-push-changes
;;   (kbd "<left>") 'vdiff-pull-changes
;;   (kbd "<down>") 'vdiff-next-change)
