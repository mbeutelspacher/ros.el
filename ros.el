;;; ros.el --- package to interact with and write code for ROS systems

;; Copyright (C) 2019 Max Beutelspacher

;; Author: Max Beutelspacher <max.beutelspacher@mailbox.org>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; package to interact with and write code for ROS systems

;;; Code:

(defcustom ros-distro (getenv "ROS_DISTRO") "Name of ROS Distribution")

(defcustom ros-default-workspace (format "/opt/ros/%s"
                                         ros-distro)
  "Path to binary/devel directory of default catkin workspace."
  :group 'ros-workspace
  :type 'directory)

(defvar ros-current-workspace nil "Path to binary/devel directory of current catkin workspace.")

(defun ros-current-workspace ()
  "Return path to binary/devel directory of current catkin workspace or to default workspace if not set."
  (if ros-current-workspace ros-current-workspace
    ros-default-workspace))

(defcustom ros-workspaces '(ros-default-workspace)
  "List of paths to binary/devel directories of catkin workspaces."
  :group 'ros-workspace
  :type 'sexp)

(defvar ros-setup-file-extension (let ((shell (getenv "SHELL")))
                                      (cond
                                       ((s-suffix-p "zsh" shell) ".zsh")
                                       ((s-suffix-p "bash" shell) ".bash")
                                       (t ".sh"))))
(defun ros-setup-file-path (path)
  "Returns the path to the right setup file in PATH"
  (let ((path (concat (file-name-as-directory path) "setup" ros-setup-file-extension))))
  )

(defun ros-source-workspace-command (path)
  "Returns the right sourcing command for this workspace at PATH"
  (format "source %s" path))

(defun ros-completing-read-workspace ()
  "Read a workspace from the minibuffer."
  (completing-read "Workspace: " ros-workspaces nil t nil nil (ros-current-workspace)))

(defun ros-select-workspace (path)
  "Set `ros-current-workspace' to PATH."
  (interactive (list (ros-completing-read-workspace)))
  (setq ros-current-workspace path))


(defun ros-shell-command-to-string (cmd)
  "Source the current workspace and run CMD and return the output as string."
  (shell-command-to-string (format "%s && %s" (ros-source-workspace-command (ros-current-workspace)) cmd)))

(defun ros-shell-output-as-list (cmd)
  "Run CMD and return a list of each line of the output."
  (split-string (ros-shell-command-to-string cmd)
                "\n"))

(defun ros-packages ()
  "List all available ros packages in the current workspace."
  (ros-shell-output-as-list "rospack list-names"))

(provide 'ros)

;;; ros.el ends here
