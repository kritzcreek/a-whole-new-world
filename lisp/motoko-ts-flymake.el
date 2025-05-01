;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(defvar-local motoko-ts--flymake-proc nil)

(defun motoko-ts-flymake (report-fn &rest _args)
  ;; Because we don't have a easy to call checker tool I rely on a
  ;; "justfile" being around
  (unless (executable-find
           "just") (error "I'm primitive and only know how to work with just"))
  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `motoko-ts-flymake-proc' to a different value
  ;;
  (when (process-live-p motoko-ts--flymake-proc)
    (kill-process motoko-ts--flymake-proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (let ((source (current-buffer))
        (path (buffer-file-name (current-buffer))))
    ;; Reset the `motoko-ts--flymake-proc' process to a new process
    ;; calling just.
    (setq
     motoko-ts--flymake-proc
     (make-process
      :name "motoko-ts-flymake" :noquery t :connection-type 'pipe
      ;; Make output go to a temporary buffer.
      ;;
      :buffer (generate-new-buffer " *motoko-ts-flymake*")
      :command '("just" "check")
      :sentinel
      (lambda (proc _event)
        ;; Check that the process has indeed exited, as it might
        ;; be simply suspended.
        ;;
        (when (memq (process-status proc) '(exit signal))
          (unwind-protect
              ;; Only proceed if `proc' is the same as
              ;; `motoko-ts--flymake-proc', which indicates that
              ;; `proc' is not an obsolete process.
              ;;
              (if (with-current-buffer source (eq proc motoko-ts--flymake-proc))
                  (with-current-buffer (process-buffer proc)
                    (goto-char (point-min))
                    ;; Parse the output buffer for diagnostic's
                    ;; messages and locations, collect them in a list
                    ;; of objects, and call `report-fn'.
                    ;;
                    (cl-loop
                     while (search-forward-regexp
                            "^\\(.*.mo\\):\\([0-9]+\\)\\(.*\\)$"
                            nil t)
                                        ;/Users/christoph.hegemann/code/motoko-snafu/src/Snafu.mo:8.24-8.25:
                     for msg-path = (match-string 1)
                     for msg = (match-string 3)
                     for (beg . end) = (flymake-diag-region
                                        source
                                        (string-to-number (match-string 2)))
                     for type = (if (string-match "warning" msg)
                                    :warning
                                  :error)
                     when (string-suffix-p msg-path path)
                     when (and beg end)
                     collect (flymake-make-diagnostic source
                                                      beg
                                                      end
                                                      type
                                                      msg)
                     into diags
                     finally (funcall report-fn diags)))
                (flymake-log :warning "Canceling obsolete check %s"
                             proc))
            ;; Cleanup the temporary buffer used to hold the
            ;; check's output.
            ;;
            (kill-buffer (process-buffer proc)))))))))

(defun motoko-ts-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'motoko-ts-flymake nil t))


(provide 'motoko-ts-flymake)
(provide 'motoko-ts-setup-flymake-backend)
