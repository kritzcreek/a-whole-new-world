;; -*- lexical-binding: t; -*-

(defun nemo-font-lock-rules ()
  (treesit-font-lock-rules
   :language 'nemo
   :override t
   :feature 'delimiter
   '((["(" ")" "#[" "[" "]" "{" "}"] @font-lock-bracket-face)
     ([";" "," ":" "::"] @font-lock-delimiter-face))

   :language 'nemo
   :override t
   :feature 'comment
   '(((comment) @font-lock-comment-face))

   :language 'nemo
   :override t
   :feature 'constant
   '(((bytes_lit) @font-lock-string-face)
     ([(int_lit) (float_lit)] @font-lock-number-face)
     ((bool_lit) @font-lock-constant-face))

   :language 'nemo
   :override t
   :feature 'keyword
   '(["if" "else" "while" "fn" "if" "use" "exports"
      "module" "let" "set" "return" "match" "struct"
      "variant" "import" "from" "global"] @font-lock-keyword-face)

   :language 'nemo
   :override t
   :feature 'delimiter
   '((["+" "&&" "||" "==" "!=" "<" "<="
       ">" ">=" "+" "-" "*" "/"] @font-lock-operator-face))


   :language 'nemo
   :override t
   :feature 'calls
   '((top_func name: (_) @font-lock-function-name-face)
     (call_e (var_e (lower_ident) @font-lock-function-call-face)))

   :language 'nemo
   :override t
   :feature 'calls
   '((struct_field_top name: (lower_ident) @font-lock-property-use-face)
     (struct_field_e name: (lower_ident) @font-lock-property-use-face))

   :language 'nemo
   :override t
   :feature 'keyword
   '(([(ty_i32) (ty_f32) (ty_bytes) (ty_bool) (ty_unit)] @font-lock-type-face)
     ((ty_cons (upper_ident) @font-lock-type-face))
     ((ty_var) @font-lock-variable-use-face))

   :language 'nemo
   :override t
   :feature 'keyword
   '(((variant_pat (upper_ident) @font-lock-type-face))
     ((top_struct name: (upper_ident) @font-lock-type-face))
     ((top_variant name: (upper_ident) @font-lock-type-face))
     ((struct_e struct: (upper_ident) @font-lock-type-face)))
   ))

(defvar nemo-indent 2)

;; For debugging
; (setq treesit--indent-verbose t)
(defvar nemo-indent-rules
  `((nemo
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((parent-is "block_e") parent-bol nemo-indent)
     ((parent-is "match_e") parent-bol nemo-indent)
     ((parent-is "let_decl") parent-bol nemo-indent)
     ((parent-is "set_decl") parent-bol nemo-indent)
     ((parent-is "top_global") parent-bol nemo-indent)
     ((parent-is "func_params") parent-bol nemo-indent)
     ((parent-is "top_struct") parent-bol nemo-indent)
     ((parent-is "top_variant") parent-bol nemo-indent)
     ((parent-is "call_args") parent-bol nemo-indent)
     )))

(defun nemo-setup ()
  "Setup treesit for nemo-mode."

  (setq-local treesit-font-lock-feature-list
              '((comment delimiter)
                (constant keyword calls)
                (declaration)))

  (setq-local treesit-font-lock-settings (nemo-font-lock-rules))

  (setq-local treesit-simple-indent-rules nemo-indent-rules)

  (treesit-major-mode-setup))

(define-derived-mode nemo-mode prog-mode "Nemo"
  "Major mode for editing Nemo source files"
  (when (treesit-ready-p 'nemo)
    (treesit-parser-create 'nemo)
    (nemo-setup)))

(add-to-list 'auto-mode-alist '("\\.nemo\\'" . nemo-mode))

(when 'treesit-language-source-alist
  (add-to-list 'treesit-language-source-alist '(nemo "https://github.com/kritzcreek/tree-sitter-nemo")))

;; Do not commit this to the nemo repo as we don't have a "simple"
;; build command we could run here yet.
;; (require 'nemo-flymake)
;; (add-hook 'nemo-mode-hook 'nemo-setup-flymake-backend)
;; (add-hook 'nemo-mode-hook 'flymake-mode)

(provide 'nemo-mode)
