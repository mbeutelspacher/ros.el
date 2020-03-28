;;; ros.el --- Package to interact with and write code for ROS systems -*- lexical-binding: t -*-

;; Copyright (C) 2019 Max Beutelspacher

;; Author: Max Beutelspacher <max.beutelspacher@mailbox.org>
;; URL: https://github.com/DerBeutlin/ros.el
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

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

;; A package to ease the interaction ROS nodes and the development of ROS software.
;; This includes the interaction with the catkin build system, interactively exploring ROS nodes,
;; messages, topics, and services, as well as some convenience functions when coding for ROS in
;; C++ and Python.

;;; Code:

(require 'with-shell-interpreter)
(require 's)
(require 'kv)

(defgroup ros nil "Related to the Robot Operating System."
  :group 'external)

(defvar ros-current-workspace nil "Path to binary/devel directory of current catkin workspace.")

(defvar ros-version "melodic" "Name of the ros version.")

(defvar ros-current-profile "default" "Name of the current profile in `ros-current-workspace'.")

(defvar ros-current-tramp-prefix nil "Prefix to all paths such that tramp can be used to run commands on remote systems.")

(defvar ros-workspaces '("localhost" . nil) "Assoc list of candidates for `ros-current-workspace' grouped by `ros-current-tramp-prefix'.")

(defun ros-shell-command-to-string (cmd &optional source)
  "Source `ros-current-workspace' if SOURCE is `t',run CMD and return output as string.

Run in `ros-current-workspace' on `ros-current-tramp-prefix' or the host system if `ros-current-tramp-prefix' is nil.
"
  (let ((command (if source (format "%s && %s" (ros-shell-source-command) cmd) cmd)))
  (s-trim (with-shell-interpreter :path (concat ros-current-tramp-prefix ros-current-workspace):form
            (shell-command-to-string (format "/bin/bash -c \"%s\"" command))))))

(defun ros-shell-command-to-list (cmd &optional source)
  "Source `ros-current-workspace' if SOURCE is `t',run CMD and return output as list.

Run in `ros-current-workspace' on `ros-current-tramp-prefix' or the host system if `ros-current-tramp-prefix' is nil.
"
    (split-string (ros-shell-command-to-string cmd source)))


(defun ros-catkin-locate-devel ()
  "Return the path to the devel folder in `ros-current-workspace' with profile `ros-current-profile'."
  (s-trim (ros-shell-command-to-string (concat "catkin locate -d --profile " ros-current-profile))))

(defun ros-shell-source-command ()
  "Return the source command to source `ros-current-workspace' with `ros-current-profile'.

If `ros-current-workspace' is nil, source /opt/ros/`ros-version'/setup.bash instead."
  (if ros-current-workspace
      (format "source %s/setup.bash"
              (ros-catkin-locate-devel))
    (format "source /opt/ros/%s/setup.bash" ros-version)))

(defun ros-catkin-list-profiles (workspace)
  "Get list of profiles for WORKSPACE."
  (let ((ros-current-workspace workspace))
    (ros-shell-command-to-list "catkin profile list -u")))

(defun ros-set-workspace ()
  "Read `ros-current-tramp prefix', `ros-current-workspace' and `ros-current-profile' from `ros-workspaces' and set the corresponding variables."
  (interactive)
  (let* ((tramp-prefix (completing-read "Tramp prefix: " (kvalist->keys ros-workspaces) nil t nil nil (if ros-current-tramp-prefix ros-current-tramp-prefix "localhost")))
         (workspace (completing-read "Workspace: " (cdr (assoc tramp-prefix ros-workspaces)) nil t nil nil ros-current-workspace))
         (profile (completing-read "Profile: " (ros-catkin-list-profiles workspace) nil t nil nil ros-current-profile)))
    (setq ros-current-tramp-prefix (when (not(string= tramp-prefix "localhost")) tramp-prefix))
    (setq ros-current-workspace workspace)
    (setq ros-current-profile profile)))

(defun ros-packages-list ()
  "List all the available Ros packages."
  (ros-shell-command-to-list "rospack list-names" t))

(defun ros-packages-location-list ()
  "Return assocation list of all the available Ros packages and their paths."
  (kvplist->alist (split-string (ros-shell-command-to-string "rospack list" t))))

(defun ros-completing-read-ros-package()
  "Completing read function for `ros-packages-list'."
  (completing-read "Package: " (ros-packages-list) nil t))

(defun ros-completing-read-ros-package-path()
  "Completing read function for `ros-packages-location-list' locations."
  (let* ((locations (ros-packages-location-list))
         (package (completing-read "Package: " (kvalist->keys locations) nil t)))
    (cdr (assoc package locations))))

(defun ros-packages-go-to-package(path)
  "Read package and open PATH to package in file manager."
  (interactive (list (ros-completing-read-ros-package-path)))
  (find-file (concat ros-current-tramp-prefix path)))

(provide 'ros)

;;; ros.el ends here
