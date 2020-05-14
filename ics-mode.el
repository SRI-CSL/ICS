;; The MIT License (MIT)
;;
;; Copyright (c) 2020 SRI International
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(require 'derived)
(require 'comint)
(require 'cl)

(defgroup ics nil
  "ICS emacs interface.")

(defvar ics-font-lock-keywords
  `(
		("%[^\n]*" . 'font-lock-comment-face)
		("\\<\\(assert\\|can\\|sigma\\|cnstrnt\\|diseq\\|solve\\|ctxt\\|partition\\|solution\\|show\\|forget\\|reset\\|find\\|inv\\|verbose\\|def\\|type\\|sig\\|save\\|restore\\|remove\\|symtab\\|help\\)\\>" . 'font-lock-keyword-face)
		("\\<\\(true\\|false\\)\\>" . 'font-lock-variable-name-face)
		("\\<\\(int\\|real\\)\\>" . 'font-lock-variable-name-face)
    "Default font-lock-keywords for ICS mode."
		)
	)

(defvar ics-mode-map ()
  "Local keymap used for ICS mode.")

(defvar ics-mode-hook nil
  "*List of functions to call when ICS mode is invoked.
This is a good place to add ICS environment specific bindings.")

(defvar ics-mode-syntax-table nil
  "Syntax table for ICS.")

(defun ics-mode-commands (map)
	)

(defun ics-create-syntax-table ()
  "Create the syntax table for ICS mode."
  (setq ics-mode-syntax-table (make-syntax-table))
  
  ; A % starts a comment
  (modify-syntax-entry ?% "<" ics-mode-syntax-table)
  ; A \f and \n end a comment
  (modify-syntax-entry ?\n ">" ics-mode-syntax-table)

  (modify-syntax-entry ?:  "." ics-mode-syntax-table)
  (modify-syntax-entry ?\; "." ics-mode-syntax-table)
  (modify-syntax-entry ?\|  "." ics-mode-syntax-table)
  (modify-syntax-entry ?+  "." ics-mode-syntax-table)
  (modify-syntax-entry ?-  "." ics-mode-syntax-table)
  (modify-syntax-entry ?*  "." ics-mode-syntax-table)
  (modify-syntax-entry ?/  "." ics-mode-syntax-table)
  (modify-syntax-entry ?=  "." ics-mode-syntax-table)
  (modify-syntax-entry ?<  "." ics-mode-syntax-table)
  (modify-syntax-entry ?>  "." ics-mode-syntax-table)
  (modify-syntax-entry ?. "." ics-mode-syntax-table)
  (modify-syntax-entry ?\\ "." ics-mode-syntax-table)
  (modify-syntax-entry ?\' "." ics-mode-syntax-table)
  (modify-syntax-entry ?# "." ics-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()" ics-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" ics-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" ics-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" ics-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" ics-mode-syntax-table)
  (modify-syntax-entry ?\} "){" ics-mode-syntax-table)
  (set-syntax-table ics-mode-syntax-table))

(if ics-mode-map
    ()
  (let ((map (make-sparse-keymap "ICS")))
    (setq ics-mode-map (make-sparse-keymap))
		(define-key ics-mode-map [menu-bar] (make-sparse-keymap))
    (define-key ics-mode-map [menu-bar ics]
      (cons "ICS" map))
    (define-key map [reset-ics] '("Reset ICS" . reset-ics))
    (define-key map [run-ics] '("Run Inferior ICS Process" . run-ics))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    ))

;;;###autoload
(defun ics-mode ()
  "ICS mode is a major mode for interacting with ICS."
  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  (make-local-variable 'comment-start)
  (setq comment-start "%")

  ;; comment end must be set because it may hold a wrong value if
  ;; this buffer had been in another mode before. RE
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'comment-start-skip) ;; used by autofill
  (setq comment-start-skip "%+[ \t]*")

;  (make-local-variable 'indent-line-function)
;  (setq indent-line-function 'ada-indent-current-function)

  (make-local-variable 'fill-column)
  (setq fill-column 75)
  
  (make-local-variable 'comment-column)
  (setq comment-column 40)

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)

  (setq major-mode 'ics-mode)
  (setq mode-name "ICS")

  (use-local-map ics-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ics-font-lock-keywords nil nil ((?_ . "w"))))

  (if ics-mode-syntax-table
	  (set-syntax-table ics-mode-syntax-table)
	(ics-create-syntax-table))

  (run-hooks 'ics-mode-hook))



;;; INFERIOR ICS MODE STUFF
;;;============================================================================

(defcustom inferior-ics-mode-hook nil
  "*Hook for customising inferior-ics mode."
  :type 'hook
  :group 'ics)

(defvar inferior-ics-mode-map nil)

(cond ((not inferior-ics-mode-map)
       (setq inferior-ics-mode-map
						 (copy-keymap comint-mode-map))
       (define-key inferior-ics-mode-map "\C-x\C-e" 'ics-send-current-command)
       (define-key inferior-ics-mode-map "\C-c\C-l" 'ics-load-file)
       (ics-mode-commands inferior-ics-mode-map))) 

;; Install the process communication commands in the ics-mode keymap.
(define-key ics-mode-map "\C-x\C-e" 'ics-send-current-command);gnu convention
(define-key ics-mode-map "\C-c\C-e" 'ics-send-current-command-and-show);gnu convention
(define-key ics-mode-map "\C-c\C-r" 'ics-send-region)
(define-key ics-mode-map "\C-c\M-r" 'ics-send-region-and-go)
(define-key ics-mode-map "\C-c\C-z" 'switch-to-ics)
(define-key ics-mode-map "\C-c\C-l" 'ics-load-file)

(let ((map (lookup-key ics-mode-map [menu-bar ics])))
  (define-key map [separator-eval] '("--"))
  (define-key map [load-file]
    '("Load ICS File" . ics-load-file))
  (define-key map [switch]
    '("Switch to ICS" . switch-to-ics))
  (define-key map [send-region-go]
    '("Evaluate Region & Go" . ics-send-region-and-go))
  (define-key map [send-region]
    '("Evaluate Region" . ics-send-region))
  (define-key map [send-sexp]
    '("Evaluate Current Command" . ics-send-current-command))
)

(defvar ics-buffer)

(defun inferior-ics-mode ()
  "Major mode for interacting with an inferior ICS process.

The following commands are available:
\\{inferior-ics-mode-map}

A ICS process can be fired up with M-x run-ics.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-ics-mode-hook (in that order).

You can send text to the inferior ICS process from other buffers containing
ICS source.  
    switch-to-ics switches the current buffer to the ICS process buffer.
    ics-send-region sends the current region to the ICS process.
    ics-send-region-and-go switchs to the ICS process buffer after sending its text.
For information on running multiple processes in multiple buffers, see
documentation for variable ics-buffer.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for ICS; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  ;; Customise in inferior-ics-mode-hook
  (setq comint-prompt-regexp "^ics> *") 
  ;; (ics-mode-variables)
  (setq major-mode 'inferior-ics-mode)
  (setq mode-name "Inferior ICS")
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-ics-mode-map)
  (setq comint-input-filter (function ics-input-filter))
  (setq comint-get-old-input (function ics-get-old-input))
  (run-hooks 'inferior-ics-mode-hook))

(defcustom inferior-ics-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'ics)

(defun ics-input-filter (str)
  "Don't save anything matching inferior-ics-filter-regexp"
  (not (string-match inferior-ics-filter-regexp str)))

(defun backward-ics-command ()
	(search-backward-regexp "\\<\\(assert\\|can\\|sigma\\|cnstrnt\\|diseq\\|solve\\|ctxt\\|partition\\|solution\\|show\\|forget\\|reset\\|find\\|inv\\|verbose\\|def\\|type\\|sig\\|save\\|restore\\|remove\\|symtab\\|help\\)\\>"))

(defun forward-ics-end-of-command ()
	(search-forward ".")
	(search-forward "\n"))

(defun ics-get-old-input ()
  "Snarf the command ending at point"
  (save-excursion
		(backward-ics-command)
		(let ((ini (point)))
			(forward-ics-end-of-command)
      (buffer-substring ini (point)))))

(defcustom ics-program-name "ics"
  "*Program invoked by the run-ics command"
  :type 'string
  :group 'ics)

(defun ics-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
					((not (= where 0))
					 (cons (substring string 0 where)
								 (ics-args-to-list (substring string (+ 1 where)
																							(length string)))))
					(t (let ((pos (string-match "[^ \t]" string)))
							 (if (null pos)
									 nil
								 (ics-args-to-list (substring string pos
																							(length string)))))))))

;;;###autoload
(defun run-ics (cmd)
  "Run an inferior ICS process, input and output via buffer *ics*.
If there is a process already running in `*ics*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `ics-program-name').  Runs the hooks `inferior-ics-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
												 (read-string "Run ICS: " ics-program-name)
											 ics-program-name)))
  (if (not (comint-check-proc "*ics*"))
      (let ((cmdlist (ics-args-to-list cmd)))
				(set-buffer (apply 'make-comint "ics" (car cmdlist)
													 nil (cdr cmdlist)))
				(inferior-ics-mode)))
  (setq ics-program-name cmd)
  (setq ics-buffer "*ics*")
  (pop-to-buffer "*ics*"))
;;;###autoload (add-hook 'same-window-buffer-names "*ics*")

(defun reset-ics ()
  "Reset the ICS process."
  (interactive)
  (comint-send-string (ics-proc) "reset.\n"))

(defun ics-send-region (start end)
  "Send the current region to the inferior ICS process."
  (interactive "r")
  (comint-send-region (ics-proc) start end)
  (comint-send-string (ics-proc) "\n"))

(defun ics-send-current-command ()
  "Send the previous sexp to the inferior ICS process."
  (interactive)
	(save-excursion
		(ics-send-region (progn (backward-ics-command) (point))
										 (progn (forward-ics-end-of-command) (point)))))

(defun ics-send-current-command-and-show ()
  "Send the previous sexp to the inferior ICS process and call `show.'."
  (interactive)
	(ics-send-current-command)
	(comint-send-string (ics-proc) "show.\n"))

(defun switch-to-ics (eob-p)
  "Switch to the ICS process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer ics-buffer)
      (pop-to-buffer ics-buffer)
      (error "No current process buffer. See variable ics-buffer."))
  (cond (eob-p
				 (push-mark)
				 (goto-char (point-max)))))

(defun ics-send-region-and-go (start end)
  "Send the current region to the inferior ICS process.
Then switch to the process buffer."
  (interactive "r")
  (ics-send-region start end)
  (switch-to-ics t))

(defcustom ics-source-modes '(ics-mode)
  "*Used to determine if a buffer contains ICS code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ICS file by ics-load-file 
Used by these commands to determine defaults."
  :type '(repeat function)
  :group 'ics)

(defvar ics-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last ics-load-file command. 
Used for determining the default in the next one.")

(defun ics-load-file (file-name)
  "Load a ICS file into the inferior ICS process."
  (interactive (comint-get-source "Load ICS file: " ics-prev-l/c-dir/file
																	ics-source-modes t)) ; T because LOAD 
																				; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq ics-prev-l/c-dir/file (cons (file-name-directory    file-name)
																		(file-name-nondirectory file-name)))
  (comint-send-string (ics-proc) (concat "load \""
																				 file-name
																				 "\".\n")))

(defvar ics-buffer nil "*The current ICS process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple ICS processes, you start the first up with
\\[run-ics]. It will be in a buffer named *ics*. Rename this buffer
with \\[rename-buffer]. You may now start up a new process with another
\\[run-ics]. It will be in a new buffer, named *ics*. You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to ICS processes --
like ics-send-region -- have to choose a
process to send to, when you have more than one ICS process around. This
is determined by the global variable ics-buffer. Suppose you
have three inferior ICS running:
    Buffer	Process
    foo		ics
    bar		ics<2>
    *ics*    ics<3>
If you do a \\[ics-send-region-and-go] command on some ICS source
code, what process do you send it to?

- If you're in a process buffer (foo, bar, or *ics*), 
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer ics-buffer.
This process selection is performed by function ics-proc.

Whenever \\[run-ics] fires up a new process, it resets ics-buffer
to be the new process's buffer. If you only run one process, this will
do the right thing. If you run multiple processes, you can change
ics-buffer to another process buffer with \\[set-variable].")

(defun ics-proc ()
  "Returns the current ICS process. See variable ics-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-ics-mode)
																			(current-buffer)
																		ics-buffer))))
    (or proc
				(error "No current process. See variable ics-buffer"))))

(defcustom ics-load-hook nil
  "This hook is run when ICS is loaded in.
This is a good place to put keybindings."
  :type 'hook
  :group 'ics)

(run-hooks 'ics-load-hook)

(provide 'ics-mode)
