(package "copy-file-on-save"
         "0.1.0"
         "Automatic syncronize when file saved.")

(website-url "https://github.com/emacs-php/emacs-auto-deployment")
(keywords "files" "comm" "deploy")

(package-file "copy-file-on-save.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "24.3")

(development
 (depends-on "projectile")
 )

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
