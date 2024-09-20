;;; ros.el --- Package to write code for ROS systems -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Max Beutelspacher
;;
;; Author: Max Beutelspacher <https://github.com/mtb>
;; Maintainer: Max Beutelspacher <max@beutelspacher.eu>
;; Created: February 14, 2021
;; Version: 1.0.0
;; Keywords: convenience tools
;; Homepage: https://github.com/DerBeutlin/ros.el
;; Package-Requires: ((emacs "27.1") s with-shell-interpreter kv cl-lib transient hydra grep string-inflection)
;;
;; This file is not part of GNU Emacs.
;;
;; ; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A package to ease the development of ROS software in C++ and python..
;; This includes the interaction with the colcon build system, interactively exploring
;; messages, topics, and services, as well as some convenience functions when coding for ROS in
;; C++ and Python.
;;
;;; Code:

(require 's)
(require 'with-shell-interpreter)
(require 'kv)
(require 'cl-lib)
(require 'transient)
(require 'hydra)
(require 'grep)
(require 'string-inflection)
(eval-and-compile
  (if (>= emacs-major-version 29)
      (require 'tramp-container)
    (require 'docker-tramp)))

(defvar ros-tramp-prefix "")

(defvar ros-current-workspace nil)

(defvar ros-version-name "")

(defvar ros-workspaces nil)

(defvar ros-master-uri "http://localhost:11311")

(defvar ros-ip "")

(defvar ros-network-settings '(("http://localhost:11311"  "")))

(defvar ros-last-debug-action "")

(defun ros-set-network-setting ()
  (interactive)
  (let* ((descriptions (mapcar  (lambda (element) (format "%s | %s" (cl-first element) (cl-second element))) ros-network-settings))
         (selected (completing-read "ROS Network Settings" descriptions nil t nil nil (format "%s | %s" ros-master-uri ros-ip)))
         (selection (nth (cl-position selected descriptions :test #'equal) ros-network-settings)))
    (setq ros-master-uri (cl-first selection))
    (setq ros-ip (cl-second selection))))

(defun ros-current-tramp-prefix ()
  (cdr (assoc "tramp-prefix" ros-current-workspace)))

(defun ros-current-workspace ()
  (cdr (assoc "workspace" ros-current-workspace)))

(defun ros-current-extensions ()
  (cdr (assoc "extends" ros-current-workspace)))

(defun ros-current-source-command ()
  (ros-source-command ros-current-workspace))

(defun ros-current-network-command ()
  (format "export ROS_MASTER_URI=%s && export ROS_IP=%s" ros-master-uri ros-ip))

(defvar ros-cache nil)

(defun ros-cache-key ()
  (format "%s_%s_%s" (ros-current-tramp-prefix) (ros-current-workspace) (string-join (ros-current-extensions) "_")))

(defun ros-cache-store (key value)
  (let ((cache (cdr (assoc (ros-cache-key) ros-cache))))
    (setf (alist-get key cache) value)
    (setf (alist-get (ros-cache-key) ros-cache) cache)))

(defun ros-cache-load (key &optional generate)
  (let ((cached-value (cdr (assoc key (cdr (assoc (ros-cache-key) ros-cache))))))
    (if cached-value cached-value (when generate (progn (ros-cache-store key (funcall generate)) (ros-cache-load key))))))

(defun ros-ignore-package ()
  (interactive)
  (ros-ignore-package--helper))

(defun ros-unignore-package ()
  (interactive)
  (ros-ignore-package--helper t)
  (ros-cache-clean))

(defun ros-ignore-package--helper (&optional remove)
  (let* ((locations  (ros-list-package-locations remove))
         (candidates (seq-filter (lambda (package) (if (file-exists-p (concat (file-name-as-directory (cdr (assoc package locations))) "COLCON_IGNORE")) remove (not remove))) (kvalist->keys locations)))
         (package-to-ignore (completing-read (concat "Package(s) to " (when remove "un") "ignore: ") (append '("ALL") candidates) nil t nil nil)))
    (if (string= package-to-ignore "ALL") (mapc (lambda (loc) (ros-ignore-package--ignore-one loc remove)) (kvalist->values locations) ) (ros-ignore-package--ignore-one (cdr (assoc package-to-ignore locations)) remove))))

(defun ros-ignore-package--ignore-one (location &optional remove)
  (let ((filename (concat (file-name-as-directory location) "COLCON_IGNORE")))
    (if remove (when (file-exists-p filename) (delete-file filename nil)) (unless (file-exists-p filename) (make-empty-file filename)))))

(defun ros-remove-every-second-item (lst)
  "Helper function removes every second entry from list."
  (if (null lst)
      nil
    (cons (car lst) (ros-remove-every-second-item (cddr lst)))))

(defun ros-clean-package (package)
  (interactive (list (ros-completing-read-package)))
  (when (y-or-n-p (format "Clean package %s?" package))
    (mapc (lambda (subfolder) (delete-directory (concat (ros-current-tramp-prefix) (ros-current-workspace) "/" subfolder "/" package) t)) '("install" "build"))))

(defun ros-clean-workspace ()
  (interactive )
  (let ((path (concat (ros-current-tramp-prefix) (ros-current-workspace))))
    (when (y-or-n-p (format "Clean workspace %s?" path))
      (mapc (lambda (subfolder) (delete-directory (concat path  "/" subfolder) t)) '("install" "build" "log")))))

(defun ros-clean-test-results (package)
  (interactive (list (ros-completing-read-package t)))
  (let ((delete-function (lambda (package) (delete-directory (concat (ros-current-tramp-prefix) (ros-current-workspace) "/build/" package "/test_results") t))))
    (if (string= package "ALL") (mapc delete-function (ros-list-packages)) (apply delete-function (list package)))))

(defun ros-cache-clean ()
  (interactive)
  (setq ros-cache nil))

(defun ros-source-command (workspace &optional use-to-build)
  (let ((extends (cdr (assoc "extends" workspace)))
        (ws-setup-bash (concat (file-name-as-directory (cdr (assoc "workspace" workspace))) "install/setup.bash")))
    (concat (if extends (string-join (mapcar (lambda (extension) (concat "source " (file-name-as-directory extension) "setup.bash")) extends) " && ") "true") (unless use-to-build (format " && test -f %s && source %s 2> /dev/null|| true" ws-setup-bash ws-setup-bash)))))


(cl-defun ros-dump-workspace (&key tramp-prefix workspace extends)
  (list (cons "tramp-prefix"  tramp-prefix)
        (cons "workspace" workspace)
        (cons "extends" extends)))

(defun ros-workspace-to-string (workspace)
  (let ((tramp-prefix (cdr (assoc "tramp-prefix" workspace))))
    (concat ( cdr (assoc "workspace" workspace)) " on " (if tramp-prefix tramp-prefix "localhost") " extending " (string-join (cdr (assoc "extends" workspace)) ", "))))

(defun ros-current-version ()
  (string-to-number (ros-shell-command-to-string "printenv ROS_VERSION")))

(defun ros-shell-command-to-string (cmd &optional use-default-directory)
  (let ((path (if use-default-directory default-directory (concat (ros-current-tramp-prefix) "~"))))
    (with-shell-interpreter
      :path path
      :form (s-trim (shell-command-to-string (format "%s  -c \"%s && %s && %s\" | sed -r \"s/\x1B\\[([0-9]{1,3}(;[0-9]{1,2})?)?[mGK]//g\"" (apply ros-shell-command-bash-func (list ros-current-workspace)) (ros-current-source-command) (ros-current-network-command) cmd))))))

(defun ros-shell-command-to-list (cmd)
  (split-string (ros-shell-command-to-string cmd) "\n" t  "[\s\f\t\n\r\v\\]+"))

(defun ros-list-packages ()
  (kvalist->keys (ros-list-package-locations)))

(defun ros-parse-colcon-list-line (line)
  (let ((components (split-string line)))
    (list (car components) (concat (file-name-as-directory  (ros-current-workspace)) (cl-second components)))))

(defcustom ros-cache-packages t
  "If t cache ros package locations, otherwise call `ros-list-package-locations-func' every time.")

(defun ros-list-package-locations (&optional include-ignored)
  (let ((generate (lambda nil (kvplist->alist (apply #'append (mapcar 'ros-parse-colcon-list-line (apply ros-list-package-locations-func nil)))))))
    (if include-ignored
        (mapcar (lambda (path)
                  (cons (file-name-nondirectory path) path))
                (mapcar (lambda (path)
                          (directory-file-name (file-name-directory path)))
                        (mapcar (lambda (path) (concat (ros-current-tramp-prefix) path)) (ros-shell-command-to-list (format " find %s -iname \"package.xml\"" (concat (ros-current-workspace) "/src"))))))
      (if ros-cache-packages (ros-cache-load "package-locations" generate) (apply generate nil)))))

(defvar ros-list-package-locations-func 'ros-colcon-list)

(defun ros-colcon-list ()
  (ros-shell-command-to-list (format "cd %s && colcon list" (ros-current-workspace))))

(defun ros-package-files (package-name)
  (ros-cache-load (concat "package" "_" package-name)
                  (lambda nil
                    (let ((package-path (concat (ros-current-tramp-prefix)
                                                (cdr (assoc package-name (ros-list-package-locations))) "/")))
                      (mapcar (lambda (str) (string-trim-left str package-path))
                              (directory-files-recursively package-path ".*"
                                                           nil
                                                           (lambda (subdir) (not (string-prefix-p "." (file-name-nondirectory subdir))))))))))

(defun ros-completing-read-workspace (&optional default-workspace)
  (let ((descriptions (mapcar 'ros-workspace-to-string ros-workspaces)))
    (nth (cl-position (completing-read "Workspace: " descriptions nil t nil nil (if default-workspace (ros-workspace-to-string default-workspace) (when ros-current-workspace (ros-workspace-to-string ros-current-workspace)))) descriptions :test #'equal)  ros-workspaces)))

(defun ros-set-workspace ()
  (interactive)
  (setq ros-current-workspace (ros-completing-read-workspace)))


(defun ros-completing-read-package (&optional add-catch-all)
  (let ((packages (ros-list-packages))
        (current-package (ros-current-package)))
    (completing-read "Package: " (if add-catch-all (append '("ALL") packages) packages)  nil t nil nil (when (member current-package packages) current-package))))

(defun ros-completing-read-package-path ()
  (let* ((locations (ros-list-package-locations))
         (package (completing-read "Package: " (kvalist->keys locations) nil t nil nil)))
    (cdr (assoc package locations))))

(defun ros-go-to-package (path)
  (interactive (list (ros-completing-read-package-path)))
  (find-file (concat (ros-current-tramp-prefix) path)))

(defun ros-find-file-in-current-package ()
  (interactive)
  (let ((current-package (ros-current-package)))
    (if current-package (ros-find-file-in-package current-package) (call-interactively 'ros-find-file-in-package))))

(defun ros-find-file-in-package (package)
  (interactive (list (ros-completing-read-package)))
  (let ((path (concat (ros-current-tramp-prefix) (cdr (assoc package (ros-list-package-locations))) "/"))
        (file (completing-read "File: " (ros-package-files package) nil t)))
    (find-file (concat path file))))

(defun ros-grep-in-package (package-path string)
  (interactive (list (ros-completing-read-package-path) (read-string "Regex: ")))
  (let ((grep-find-ignored-directories  (mapcar (lambda (name) (string-trim-right name "/")) (seq-filter (lambda (name) (and (directory-name-p name) (s-starts-with-p "." name))) (file-name-all-completions "" package-path)))))
    (unless grep-find-template (grep-compute-defaults))
    (rgrep string "*" package-path)))

(defun ros-grep-in-current-package (string)
  (interactive (list  (read-string "Regex: ")))
  (ros-grep-in-package (locate-dominating-file default-directory "package.xml") string))

(defun ros-list-messages ()
  (ros-cache-load "messages" (lambda nil (if (eq (ros-current-version) 1) (ros-list-messages-ros1) (ros-list-messages-ros2)))))

(defun ros-generic-cmd (type)
  (if (eq (ros-current-version) 1) (format "ros%s" type) (format "ros2 %s" type)))

(defun ros-generic-list (type)
  (ros-shell-command-to-list  (format "%s list" (ros-generic-cmd type))))

(defun ros-generic-type (type name)
  (ros-shell-command-to-string (format "%s type %s" (ros-generic-cmd type) name)))

(defun ros-generic-completing-read (type)
  (completing-read (format "%s: " type) (ros-generic-list type) nil t))

(defun ros-list-messages-ros1 ()
  (ros-shell-command-to-list "rosmsg list"))

(defun ros-list-messages-ros2 ()
  (cdr (ros-shell-command-to-list "ros2 interface list -m")))

(defun ros-completing-read-message ()
  (completing-read "Message: " (ros-list-messages) nil t))

(defun ros-completing-read-srv ()
  (completing-read "Srv:" (ros-list-srvs) nil t))

(defun ros-completing-read-action ()
  (completing-read "Action:" (ros-list-actions) nil t))

(defun ros-select-import-line ()
  (let ((avy-all-windows nil))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((require 'avy nil nil)
        (avy--line nil (point-min) (point-max)))
       ((require 'consult nil nil)
        (marker-position (consult-line)))
       (t (user-error "You need to install `avy' or `consult' to use this feature"))))))

(defun ros-insert-message (msg)
  (interactive (list (ros-completing-read-message)))
  (ros-insert-interface "msg" msg))

(defun ros-insert-message-import (msg line)
  (interactive (list (ros-completing-read-message) (ros-select-import-line)))
  (save-excursion
    (goto-char line)
    (ros-insert-interface "msg" msg t)
    (insert "\n")))

(defun ros-insert-srv (srv)
  (interactive (list (ros-completing-read-srv)))
  (ros-insert-interface "srv" srv))

(defun ros-insert-srv-import (srv line)
  (interactive (list  (ros-completing-read-srv) (ros-select-import-line)))
  (save-excursion
    (goto-char line)
    (ros-insert-interface "srv" srv t)
    (insert "\n")))

(defun ros-insert-action (action)
  (interactive (list (ros-completing-read-action)))
  (ros-insert-interface "action" action))

(defun ros-insert-action-import (action line)
  (interactive (list (ros-completing-read-action)  (ros-select-import-line)))
  (save-excursion
    (goto-char line)
    (ros-insert-interface "action" action t)
    (insert "\n")))

(defun ros-insert-interface (type item &optional import)
  (let ((data (ros-parse-interface type item)))
    (cond ((string= major-mode "python-mode") (ros-insert-interface-python data import))
          ((string= major-mode "c++-mode") (ros-insert-interface-cpp data import))
          (t (message "Only C++-mode and Python mode are supported.")))))

(defun ros-insert-interface-python (data import)
  (let ((name (cdr (assoc "name" data)))
        (package (cdr (assoc "package" data)))
        (type (cdr (assoc "type" data))))
    (if import (insert (format "from %s.%s import %s" package type name)) (insert name))))

(defun ros-insert-interface-cpp (data import)
  (if (= (ros-current-version) 1) (ros-insert-interface-cpp-ros1 data import) (ros-insert-interface-cpp-ros2 data import)))

(defun ros-insert-interface-cpp-ros1 (data import)
  (let ((name (cdr (assoc "name" data)))
        (package (cdr (assoc "package" data))))
    (if import (insert (format "#include <%s/%s.h>" package name)) (insert (format "%s::%s" package name)))))

(defun ros-insert-interface-cpp-ros2 (data import)
  (let ((name (cdr (assoc "name" data)))
        (package (cdr (assoc "package" data)))
        (type (cdr (assoc "type" data))))
    (if import (insert (format "#include <%s/%s/%s.hpp>" package type (downcase (string-inflection-underscore-function name)))) (insert (format "%s::%s::%s" package type name)))))

(defun ros-parse-interface (type item)
  (if (= (ros-current-version) 1) (ros-parse-interface-ros1 type item) (ros-parse-interface-ros2 item)))

(defun ros-parse-interface-ros1 (type item)
  (let* ((splits (split-string item "/"))
         (package (cl-first splits))
         (name (cl-second splits)))
    (list (cons "package" package)
          (cons "name" name)
          (cons "type" type))))

(defun ros-parse-interface-ros2 (item)
  (message item)
  (let* ((splits (split-string item "/"))
         (package (cl-first splits))
         (type (cl-second splits))
         (name (cl-third splits)))
    (list (cons "package" package)
          (cons "name" name)
          (cons "type" type))))



(defun ros-list-srvs ()
  (ros-cache-load "srvs" (lambda nil  (if (eq (ros-current-version) 1) (ros-list-srvs-ros1) (ros-list-srvs-ros2)))))

(defun ros-list-srvs-ros1 ()
  (ros-shell-command-to-list "rossrv list"))

(defun ros-list-srvs-ros2 ()
  (cdr (ros-shell-command-to-list "ros2 interface list -s")))

(defun ros-list-actions ()
  (ros-cache-load "actions" (lambda nil  (if (eq (ros-current-version) 1) (ros-list-actions-ros1) (ros-list-actions-ros2)))))

(defun ros-list-actions-ros1 ()
  (error "Actions can only be queried in ROS1"))

(defun ros-list-actions-ros2 ()
  (cdr (ros-shell-command-to-list "ros2 interface list -a")))

(defun ros-show-message-info (msg)
  (interactive (list (ros-completing-read-message)))
  (ros-interface-show-info "msg" msg)
  (ros-msg-info-mode))

(defun ros-show-srv-info (srv)
  (interactive (list (ros-completing-read-srv)))
  (ros-interface-show-info "srv" srv)
  (ros-srv-info-mode))

(defun ros-show-action-info (action)
  (interactive (list (ros-completing-read-action)))
  (ros-interface-show-info "action" action)
  (ros-action-info-mode))

(defun ros-interface-show-info (type name)
  (let ((buffer-name (format "* ros-%s: %s" type name)))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))
    (switch-to-buffer buffer-name))
  (erase-buffer)
  (insert (ros-interface-info type name)))

(defun ros-show-message-info-at-point ()
  (interactive)
  (ros-show-message-info (thing-at-point 'symbol)))

(define-minor-mode ros-msg-info-mode  "Mode to display information about rosmsg."   :lighter "rosmsg show" :keymap '(([return] . ros-show-message-info-at-point)))
(define-minor-mode ros-srv-info-mode  "Mode to display information about rossrv."  :lighter "rossrv show" :keymap '(([return] . ros-show-message-info-at-point)))
(define-minor-mode ros-action-info-mode  "Mode to display information about ros action."  :lighter "rosaction show" :keymap '(([return] . ros-show-message-info-at-point)))

(defun ros-interface-info (type name)
  (if (= (ros-current-version) 1) (ros-shell-command-to-string (format "ros%s show %s" type  name))
    (ros-shell-command-to-string (format "ros2 interface show %s" name))))

(defun ros-topic-list ()
  (ros-generic-list "topic"))

(defun ros-topic-type (topic)
  (ros-generic-type "topic" topic))

(defun ros-topic-echo (topic)
  (interactive (list (ros-generic-completing-read "topic")))
  (let ((cmd (format "%s echo %s" (ros-generic-cmd "topic") topic))
        (default-directory (concat (ros-current-tramp-prefix ) (ros-current-workspace))))
    (start-file-process-shell-command cmd cmd
                                 (format "%s -c '%s && %s'" (apply ros-shell-command-bash-func (list ros-current-workspace)) (ros-source-command ros-current-workspace) cmd))
    (switch-to-buffer cmd)))

(defun ros-generic-info (type name &optional flags)
  (let* ((buffer-name (format "* %s: %s" (ros-generic-cmd type) name))
         (flag (if (and (string= type "topic") (eq (ros-current-version) 2)) " -v " ""))
         (output (ros-shell-command-to-string (format "%s info %s %s" (ros-generic-cmd type) flag name))))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (insert output)))

(defun ros-topic-info (topic)
  (interactive (list (ros-generic-completing-read "topic")))
  (ros-generic-info "topic" topic)
  (ros-topic-info-mode))


(defun ros-topic-mode-info-thing-at-point ()
  (interactive)
  (let ((line (thing-at-point 'line))
        (symbol (thing-at-point 'symbol)))
    (cond ((or (s-starts-with-p "Topic type:" line) (s-starts-with-p "Type: " line)) (ros-show-message-info symbol))
          (t (ros-node-info (if (s-starts-with-p "/" symbol) symbol (concat "/" symbol)))))))

(define-minor-mode ros-topic-info-mode  "Mode to display information about rostopics."  :lighter "rostopic info" :keymap '(([return] . ros-topic-mode-info-thing-at-point )))

(defun ros-node-list ()
  (ros-generic-list "node"))

(defun ros-node-info (node)
  (interactive (list (ros-generic-completing-read "node")))
  (ros-generic-info "node" node)
  (ros-node-info-mode))

(defun ros-node-kill (node)
  (interactive (list (ros-generic-completing-read "node")))
  (if (eq (ros-current-version) 2) (error "ros node kill is not supported on ROS2") (ros-shell-command-to-string (format "rosnode kill %s" node))))

(define-minor-mode ros-node-info-mode  "Mode to display information about rosnodes." :lighter  "rosnode info" :keymap '(([return] . ros-node-mode-info-thing-at-point)))

(defun ros-service-list ()
  (ros-generic-list "service"))

(defun ros-service-type (service)
  (ros-generic-type "service" service))

(defun ros-service-info (service)
  (interactive (list (ros-generic-completing-read "service")))
  (ros-generic-info "service" service)
  (ros-service-info-mode))

(defun ros-service-mode-info-thing-at-point ()
  (interactive)
  (let ((line (thing-at-point 'line))
        (symbol (thing-at-point 'symbol)))
    (cond ((s-starts-with-p "Type: " line) (ros-show-srv-info symbol))
          (t (ros-node-info (if (s-starts-with-p "/" symbol) symbol (concat "/" symbol)) )))))

(define-minor-mode ros-service-info-mode  "Mode to display information about rosservices."  :lighter "rosnode info" :keymap '(([return] . ros-service-mode-info-thing-at-point)))

(defun ros-node-mode-info-thing-at-point ()
  (interactive)
  (if (eq (ros-current-version )2) (ros-node-mode-info-thing-at-point-ros2) (ros-node-mode-info-thing-at-point-ros1)))

(defun ros-node-mode-info-thing-at-point-ros2 ()
  (let* ((end-of-line (save-excursion  (end-of-line) (point)))
         (before-p (save-excursion (search-forward ":" end-of-line t)))
         (symbol (thing-at-point 'symbol)))
    (cond
     ((save-excursion (search-backward "Service Servers:" nil t)) (message "Describe Services is not implemented"))
     ((save-excursion (search-backward "Publishers:" nil t)) (if before-p (ros-topic-info symbol) (ros-show-message-info symbol)))
     ((save-excursion (search-backward "Subscribers:" nil t)) (if before-p (ros-topic-info symbol) (ros-show-message-info symbol))))))

(defun ros-node-mode-info-thing-at-point-ros1 ()
  (let* ((line (thing-at-point 'line))
         (surrounded-p (let ((ppss (syntax-ppss))) (when (nth 1 ppss) (char-after (nth 1 ppss)))))
         (symbol (thing-at-point 'symbol)))
    (cond
     ((s-starts-with-p "Node" line) (ros-node-info symbol))
     ((save-excursion (search-backward "Connections:" nil t)) (if (s-starts-with-p " * topic:" line) (ros-topic-info symbol) (ros-node-info symbol)))
     ((save-excursion (search-backward "Services:" nil t)) (ros-service-info symbol))
     (t (if surrounded-p (ros-show-message-info symbol) (ros-topic-info symbol))))))

(cl-defun ros-dump-colcon-action (&key workspace verb flags post-cmd)
  (list (cons "workspace" workspace)
        (cons "verb" verb)
        (cons "flags" flags)
        (cons "post-cmd" post-cmd)))

(defun ros-load-colcon-action (action)
  (let* ((ros-current-workspace (cdr (assoc "workspace" action)))
         (source-command (ros-source-command ros-current-workspace t))
         (verb (cdr (assoc "verb" action)))
         (flags (cdr (assoc "flags" action)))
         (post-cmd (cdr (assoc "post-cmd" action))))
    (concat source-command " && cd " (ros-current-workspace) " && colcon " verb " " (when flags (string-join flags " ")) (when post-cmd (concat " && " post-cmd)))))

(defun ros-edit-colcon-action (action)
  (interactive (list (ros-completing-read-colcon-action-from-history)))
  (let* ((section (completing-read "Which section to edit: " (kvalist->keys action) nil t))
         (new-value
          (cond
           ((string-equal section "workspace") (ros-completing-read-workspace (cdr (assoc "workspace" action))))
           ((string-equal section "verb") (completing-read "Verb: " '("build" "test") nil t nil nil (cdr (assoc "verb" action))))
           ((string-equal section "flags") (ros-edit-colcon-flags (cdr (assoc "flags" action))))
           ((string-equal section "post-cmd") (read-string "Edit: " (cdr (assoc "post-cmd" action))))
           (t (error "Invalid choice"))))
         (new-action (mapcar (lambda (pair) (if (equal (car pair) section) (cons section new-value) pair)) action)))
    (setq ros-colcon-action-history (remove action ros-colcon-action-history))
    (if (y-or-n-p "Do you want to run the updated compile command: ") (ros-compile-action new-action) (ros-push-colcon-action-to-history new-action))))


(defun ros-edit-colcon-flags (flags)
  (let* ((candidate (completing-read "Flag to edit: " flags nil t))
         (replacement (read-string "Edit: " candidate)))
    (append (remove candidate flags) (list replacement))))

(defvar ros-compile-bash-func (lambda (ws) "bash"))
(defvar ros-shell-command-bash-func (lambda (ws) "bash"))

(defun ros-compile-action (action)
  (interactive (list (ros-completing-read-colcon-action-from-history)))
  (let* ((ros-current-workspace (cdr (assoc "workspace" action)))
         (compilation-buffer-name-function (lambda (m) (concat "*Compile " (ros-current-workspace) "*")))
         (default-directory (concat (ros-current-tramp-prefix) (ros-current-workspace))))
    (ros-push-colcon-action-to-history action)
    (compile (format "%s -c \"%s\"" (apply ros-compile-bash-func (list ros-current-workspace)) (ros-load-colcon-action action)))
    (other-window -1)))


(defun ros-push-colcon-action-to-history (action)
  (when (member action ros-colcon-action-history) (setq ros-colcon-action-history (remove action ros-colcon-action-history)))
  (push action ros-colcon-action-history))

(defun ros-display-colcon-action (action)
  "Display string which describes the ACTION."
  (let* ((workspace (cdr(assoc "workspace" action)))
         (workspace-path (cdr (assoc "workspace" workspace)))
         (tramp-prefix (cdr (assoc "tramp-prefix" workspace)))
         (verb (cdr(assoc "verb" action)))
         (flags (cdr(assoc "flags" action)))
         (post-cmd (cdr(assoc "post-cmd" action))))
    (format "%s | %s |  colcon %s %s %s" (if tramp-prefix tramp-prefix "localhost") workspace-path verb (string-join flags " ") (if post-cmd (concat "&& " post-cmd) ""))))

(defvar ros-colcon-action-history '())

(defun ros-completing-read-colcon-action-from-history ()
  (let* ((history-strings (cl-mapcar 'ros-display-colcon-action ros-colcon-action-history))
         (action-string (completing-read "Action: " history-strings nil t))
         (index (seq-position history-strings action-string)))
    (nth index ros-colcon-action-history)))


(defun ros-current-package ()
  (let ((default-directory (locate-dominating-file default-directory "package.xml")))
    (when default-directory
      (let ((current-package (ros-parse-current-package)))
        (when (not (string= current-package "")) current-package)))))

(defun ros-parse-current-package ()
  (if (executable-find "colcon") (ros-shell-command-to-string "colcon list -n" t)
    (when (executable-find "xmllint") (string-trim (shell-command-to-string "xmllint --xpath \"string(//name)\" package.xml 2> /dev/null")))))

(defun ros-colcon-build-and-test-current-package (&optional flags use-tcr)
  (interactive (list (ros-merge-cmake-args-commands (transient-args 'ros-colcon-build-transient)) nil))
  (ros-colcon-build-current-package flags t use-tcr))

(defun ros-colcon-build-and-test-current-package-with-tcr (&optional flags)
  (interactive (list (ros-merge-cmake-args-commands (transient-args 'ros-colcon-build-transient))))
  (ros-colcon-build-and-test-current-package flags t))

(defun ros-colcon-build-current-package (&optional flags test use-tcr)
  (interactive (list (ros-merge-cmake-args-commands (transient-args 'ros-colcon-build-transient))))
  (let ((current-package (ros-current-package)))
    (ros-colcon-build-package (if current-package current-package (ros-completing-read-package)) flags test use-tcr)))

(defun ros-colcon-build-and-test-package (package &optional flags use-tcr)
  (interactive (list (ros-completing-read-package) (ros-merge-cmake-args-commands (transient-args 'ros-colcon-build-transient))))
  (ros-colcon-build-package package flags t use-tcr))

(defun ros-colcon-build-and-test-package-with-tcr (package &optional flags)
  (interactive (list (ros-completing-read-package) (ros-merge-cmake-args-commands (transient-args 'ros-colcon-build-transient))))
  (ros-colcon-build-and-test-package package flags t))

(defun ros-get-tcr-command (package)
  (let ((default-directory (cdr (assoc package (ros-list-package-locations)))))
    (let ((git-root-dir (vc-root-dir)))
      (format "&& (cd %s && git commit -am \"chore: TCR\"; echo \"Commit\" && cd - && exit 0) || (cd %s && git reset --hard HEAD; echo \"Revert\" &&  cd - && exit 1) " git-root-dir git-root-dir))))


(defun ros-colcon-build-package (package &optional flags test use-tcr)
  "Run a build action to build PACKAGE with FLAGS."
  (interactive (list (ros-completing-read-package) (ros-merge-cmake-args-commands (transient-args 'ros-colcon-build-transient))))
  (let ((real-flags (seq-filter  (lambda (flag) (not (string= flag "ISOLATED"))) flags))
        (is-isolated (member "ISOLATED" flags))
        (post-cmd (when test (ros-load-colcon-action (ros-dump-colcon-action :workspace ros-current-workspace :verb "build" :flags (list "--cmake-args" (if (eq (ros-current-version) 1) "-DCATKIN_ENABLE_TESTING=ON" "-DBUILD_TESTING=ON") "--packages-select" package) :post-cmd
                                                                             (concat "colcon test-result --delete-yes && colcon test --packages-select " package " && colcon test-result --verbose" (when use-tcr (ros-get-tcr-command package))))))))
    (ros-compile-action (ros-dump-colcon-action :workspace ros-current-workspace :verb "build" :flags (append (list (concat (if is-isolated "--packages-select " "--packages-up-to ") package)) real-flags)  :post-cmd post-cmd))))

(defun ros-colcon-build-workspace (&optional flags test)
  (interactive (list (ros-merge-cmake-args-commands (transient-args 'ros-colcon-build-transient))))
  (let ((real-flags (seq-filter  (lambda (flag) (not (string= flag "ISOLATED")) )flags)))
    (ros-compile-action (ros-dump-colcon-action :workspace ros-current-workspace :verb "build" :flags real-flags :post-cmd (when test (concat "colcon test --packages-select " package " && colcon test-result --verbose"))))))

(defun ros-2-find-debug-executable ()
  "Search for, then launch a ROS2 node in GDB mode."
  (interactive)
  (let* ((lib-path-orig (ros-shell-command-to-string (concat "env | grep " (nth 3 (split-string (car (ros-current-extensions)) "/")))))
         (lib-path (replace-regexp-in-string "\n" "\" --eval-command \"set env " lib-path-orig))
         (package-list (split-string (ros-shell-command-to-string "ros2 pkg executables")))
         (trimmed-package-list (delete-dups (ros-remove-every-second-item package-list)))
         (package (completing-read "Package: " trimmed-package-list nil t))
         (executables (split-string (ros-shell-command-to-string (format "ros2 pkg executables %s" package))))
         (filtered-executables (cl-remove-if-not #'(lambda (x) (not (zerop (% (cl-position x executables) 2)))) executables))
         (executable (completing-read "Executable: " filtered-executables nil t))
         (prefix (ros-shell-command-to-string (format "ros2 pkg prefix %s" package)))
         (gdb-command (if (ros-current-tramp-prefix)
                          (format "gdb -i=mi %s%s/lib/%s/%s --eval-command \"set env %s\"" (ros-current-tramp-prefix) prefix package executable lib-path)
                        (format "gdb -i=mi %s/lib/%s/%s --eval-command \"set env %s\"" prefix package executable lib-path))))
    (setq ros-last-debug-action gdb-command)
    (gdb gdb-command)))

(defun ros-1-find-debug-executable ()
  "Search for, then launch a ROS1 node in GDB mode."
  (interactive)
  (let* ((lib-path-orig (ros-shell-command-to-string (concat "env | grep " (nth 3 (split-string (car (ros-current-extensions)) "/")))))
         (lib-path (replace-regexp-in-string "\n" "\" --eval-command \"set env " lib-path-orig))
         (package (completing-read "Package: " (ros-list-packages) nil t))
         (nodes (split-string (ros-shell-command-to-string (format "find %s -type f -executable" (concat (ros-current-workspace) "build/" package "/devel/lib/" package)))))
         (nodelets (split-string (ros-shell-command-to-string (format "rosrun nodelet declared_nodelets | grep %s" package))) )
         (executables (append nodelets nodes))
         (executable (completing-read "Executable: " executables nil t))
         (gdb-prefix (if (member executable nodes)
                         executable (concat (car (ros-current-extensions)) "lib/nodelet/nodelet --eval-command \" set args standalone " executable "\"")))
         (gdb-command (if (ros-current-tramp-prefix)
                          (format "gdb -i=mi %s%s --eval-command \"set env %s\"" (ros-current-tramp-prefix) gdb-prefix lib-path)
                        (format "gdb -i=mi %s --eval-command \"set env %s\"" gdb-prefix lib-path))))
    (setq ros-last-debug-action gdb-command)
    (gdb gdb-command)))

(defun ros-find-debug-executable ()
  "Select an executable to run through GDB."
  (interactive)
  (if (eq (ros-current-version) 1) (ros-1-find-debug-executable)(ros-2-find-debug-executable)))

(defun ros-repeat-last-debug()
  "If there has been a last debug action, repeat it."
  (interactive)
  (if (null ros-last-debug-action)
      (ros-find-debug-executable)
    (gdb ros-last-debug-action)))

(defvar ros-additional-cmake-args nil)

(defun ros-merge-cmake-args-commands (flags)
  (let* ((is-cmake-args (lambda (f) (s-starts-with-p "--cmake-args" f)))
         (flags-without-cmake-args (cl-remove-if is-cmake-args flags))
         (replace-testing (lambda (input-str) (if (= 1 (ros-current-version)) (replace-regexp-in-string "-DBUILD_TESTING" "-DCATKIN_ENABLE_TESTING" input-str) input-str)))
         (cmake-args-flags (mapcar replace-testing (seq-filter is-cmake-args flags))))
    (append flags-without-cmake-args '("--cmake-args") (mapcar (lambda (f) (string-trim-left f "--cmake-args")) cmake-args-flags) ros-additional-cmake-args)))

(transient-define-infix ros-colcon-build-transient:--DCMAKE_BUILD_TYPE()
  :description "-DCMAKE_BUILD_TYPE"
  :class 'transient-switches
  :key "-bt"
  :argument-format "--cmake-args -DCMAKE_BUILD_TYPE=%s"
  :argument-regexp "\\(--cmake-args -DCMAKE_BUILD_TYPE=\\(Release\\|Debug\\|RelWithDebInfo\\|MinSizeRel\\)\\)"
  :choices '("Release" "Debug" "RelWithDebInfo" "MinSizeRel"))


(transient-define-infix ros-colcon-build-transient:--DBUILD_TESTING ()
  :description "-DBUILD_TESTING"
  :class 'transient-switches
  :key "-T"
  :argument-format "--cmake-args -DBUILD_TESTING=%s"
  :argument-regexp "\\(--cmake-args -DBUILD_TESTING=\\(ON\\|OFF\\)\\)"
  :choices '("ON" "OFF"))


(transient-define-infix ros-colcon-build-transient:--DCMAKE_EXPORT_COMPILE_COMMANDS()
  :description "-DCMAKE_EXPORT_COMPILE_COMMANDS"
  :class 'transient-switches
  :key "-ec"
  :argument-format "--cmake-args -DCMAKE_EXPORT_COMPILE_COMMANDS=%s"
  :argument-regexp "\\(--cmake-args -DCMAKE_EXPORT_COMPILE_COMMANDS=\\(ON\\|OFF\\)\\)"
  :choices '("ON" "OFF"))

(transient-define-argument ros-colcon-build-transient:--parallel-workers()
  :description "The maximum number of packages to process in parallel"
  :class 'transient-option
  :shortarg "-pw"
  :argument "--parallel-workers "
  :reader 'transient-read-number-N+)

(transient-define-prefix ros-colcon-build-transient ()
  "Transient command for catkin build."
  ["Arguments"
   ("-i" "only build the package not its dependencies" "ISOLATED")
   ("-c" "continue on failure" "--continue-on-error")
   ("-K" "clean first" "--cmake-clean-first")
   ("-C" "Use CCache" "--cmake-args \"-DCMAKE_C_COMPILER_LAUNCHER=ccache\" \"-DCMAKE_CXX_COMPILER_LAUNCHER=ccache\"")
   ("-fc" "force CMake configure step" "--cmake-force-configure")
   ("-s" "Use symlinks instead of copying files where possible" "--symlink-install")
   ("-m" "Merge packages for install" "--merge-install")
   ("-d" "Use console direct for output" "--event-handlers console_direct+")
   (ros-colcon-build-transient:--DCMAKE_BUILD_TYPE)
   (ros-colcon-build-transient:--DCMAKE_EXPORT_COMPILE_COMMANDS)
   (ros-colcon-build-transient:--DBUILD_TESTING)
   (ros-colcon-build-transient:--parallel-workers)]

  ["Actions"
   ("p" "Build current package" ros-colcon-build-current-package)
   ("P" "Build a package" ros-colcon-build-package)
   ("w" "Build current workspace" ros-colcon-build-workspace)
   ("t" "Build and test current package" ros-colcon-build-and-test-current-package)
   ("T" "Build and test a package" ros-colcon-build-and-test-package)
   ("r" "Build and test current package and revert changes in repository if test fail" ros-colcon-build-and-test-current-package-with-tcr)
   ("R" "Build and test a package with TCR and revert changes in repository if test fail" ros-colcon-build-and-test-package-with-tcr)])




(defun ros-colcon-test-current-package (&optional flags)
  (interactive (list (transient-args 'ros-colcon-test-transient)))
  (let ((current-package (ros-current-package)))
    (ros-colcon-test-package (if current-package current-package (ros-completing-read-package)) flags)))


(defun ros-colcon-test-package (package &optional flags)
  (interactive (list (ros-completing-read-package) (transient-args 'ros-colcon-test-transient)))
  (ros-compile-action (ros-dump-colcon-action :workspace ros-current-workspace :verb "test" :flags (append (list (concat "--packages-select " package)) flags) :post-cmd "colcon test-result --verbose")))

(defun ros-colcon-test-package-with-tcr (package &optional flags)
  (interactive (list (ros-completing-read-package) (transient-args 'ros-colcon-test-transient)))
  (ros-colcon-test-package package flags t))

(defun ros-colcon-test-workspace (&optional flags)
  (interactive (list (transient-args 'ros-colcon-test-transient)))
  (ros-compile-action (ros-dump-colcon-action :workspace ros-current-workspace :verb "test" :flags flags :post-cmd  "colcon test-result --verbose")))

(transient-define-argument ros-colcon-test-transient:--retest-until-fail()
  :description "Rerun tests up to N times if they pass"
  :class 'transient-option
  :shortarg "-rp"
  :argument "--retest-until-fail "
  :reader 'transient-read-number-N+)

(transient-define-argument ros-colcon-test-transient:--retest-until-pass()
  :description "Rerun failing tests up to N times"
  :class 'transient-option
  :shortarg "-rf"
  :argument "--retest-until-pass "
  :reader 'transient-read-number-N+)

(transient-define-prefix ros-colcon-test-transient ()
  "Transient command for catkin build."
  ["Arguments"
   ("-a" "abort on error" "--abort-on-error")
   (ros-colcon-build-transient:--parallel-workers)
   (ros-colcon-test-transient:--retest-until-fail)
   (ros-colcon-test-transient:--retest-until-pass)]

  ["Actions"
   ("p" "Test current package" ros-colcon-test-current-package)
   ("P" "Test a package" ros-colcon-test-package)
   ("w" "Test current workspace" ros-colcon-test-workspace)])

(defhydra hydra-ros-main (:color blue :hint nil :foreign-keys warn)
  "
_c_: Compile   _t_: Test      _d_: Debug          _w_: Set Workspace  _p_: packages
_i_: ignore    _m_: Messages  _s_: Srvs           _a_: Actions        _x_: Clean
_T_: Topic     _N_: Node      _S_: Service        _M_: ROS-Master
"
  ("c" ros-colcon-build-transient)
  ("t" ros-colcon-test-transient)
  ("w" ros-set-workspace)
  ("p" hydra-ros-packages/body)
  ("d" hydra-ros-debug/body)
  ("i" hydra-ros-ignore/body)
  ("m" hydra-ros-messages/body)
  ("s" hydra-ros-srvs/body)
  ("a" hydra-ros-actions/body)
  ("x" hydra-ros-clean/body)
  ("T" hydra-ros-topic/body)
  ("N" hydra-ros-node/body)
  ("S" hydra-ros-service/body)
  ("M" ros-set-network-setting)

  ("q" nil "quit" :color blue))


(defhydra hydra-ros-packages (:color blue :hint nil :foreign-keys warn)
  "
_g_: Go to package _f_:  Find file current package  _F_: Find file in a package
_s_:  Search in current package  _S_: Search in a package
"
  ("g" ros-go-to-package)
  ("F" ros-find-file-in-package)
  ("f" ros-find-file-in-current-package)
  ("s" ros-grep-in-current-package)
  ("S" ros-grep-in-package)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-debug (:color blue :hint nil :foreign-keys warn)
  "
 _f_: Find executable in workspace to debug _l_: Repeat last debug action
"
  ("f" ros-find-debug-executable)
  ("l" ros-repeat-last-debug)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-messages (:color blue :hint nil :foreign-keys warn)
  "
_i_: Insert message type at point               _s_: Show message
_I_: Insert import statement for message type
"
  ("i" ros-insert-message)
  ("I" ros-insert-message-import)
  ("s" ros-show-message-info)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-srvs (:color blue :hint nil :foreign-keys warn)
  "
_i_: Insert srv type at point               _s_: Show srv
_I_: Insert import statement for srv type
"
  ("i" ros-insert-srv)
  ("I" ros-insert-srv-import)
  ("s" ros-show-srv-info)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-actions (:color blue :hint nil :foreign-keys warn)
  "
_i_: Insert action type at point               _s_: Show action
_I_: Insert import statement for action type
"
  ("i" ros-insert-action)
  ("I" ros-insert-action-import)
  ("s" ros-show-action-info)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-ignore (:color blue :hint nil :foreign-keys warn)

  "
_+_: Ignore a package                _-_: Unignore an ignored package
"
  ("+" ros-ignore-package)
  ("-" ros-unignore-package)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-clean (:color blue :hint nil :foreign-keys warn)

  "
_w_: Clean workspace    _p_: Clean package
_c_: Clean cache        _t_: Test Results
"
  ("w" ros-clean-workspace)
  ("p" ros-clean-package)
  ("c" ros-cache-clean)
  ("t" ros-clean-test-results)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-topic (:color blue :hint nil :foreign-keys warn)
  "
_i_: rostopic info   _e_: rostopic echo
"
  ("i" ros-topic-info)
  ("e" ros-topic-echo)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-node (:color blue :hint nil :foreign-keys warn)
  "
_i_: rosnode info  _k_: rosnode kill
"
  ("i" ros-node-info)
  ("k" ros-node-kill)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-service (:color blue :hint nil :foreign-keys warn)
  "
_i_: rosservice info
"
  ("i" ros-service-info)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(provide 'ros)
;;; ros.el ends here
