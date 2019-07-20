;;; ros-test.el --- Tests for ros.el

;; Copyright (C) 2013 Max Beutelspacher

;; Author: Max Beutelspacher <max.beutelspacher@mailbox.org>

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

;; Tests for ros.el

;;; Code:

(require 'ert)
(require 'ros)

(ert-deftest ros-test-default-workspace-is-used-when-no-workspace-is-set ()
  (let ((ros-current-workspace nil)
        (ros-default-workspace "~/catkin_ws"))
    (should (string= (ros-current-workspace) ros-default-workspace))))

(ert-deftest ros-test-current-workspace-is-used-if-set ()
  (let ((ros-default-workspace "~/catkin_ws")
        (ros-current-workspace "~/catkin_ws2"))
    (should (string= (ros-current-workspace) ros-current-workspace)))
  )

(ert-deftest ros-test-shell-output-as-list ()
  (should (cl-every 'string=
                    (ros-shell-output-as-list "for VAR in 1 2 3 4 5; do; echo $VAR;done;")
                    '("1" "2" "3" "4" "5"))))

(ert-deftest ros-test-package-list ()
  (should (member "roscpp" (ros-packages))))

(ert-deftest ros-sourcing-command-for-zsh()
  (skip-unless (string= (getenv "SHELL") "/usr/bin/zsh"))
  (should (string= (ros-source-workspace-command "/opt/ros/melodic") "source /opt/ros/melodic/setup.zsh")))

(ert-deftest ros-sourcing-command-for-zsh()
  (skip-unless (string= (getenv "SHELL") "/bin/bash"))
  (should (string= (ros-source-workspace-command "/opt/ros/melodic") "source /opt/ros/melodic/setup.bash")))

(ert-deftest ros-test-generic-list-msg  ()
  (should (member "std_msgs/String" (ros-generic-list "msg")))
  )
(ert-deftest ros-generic-get-msg-returns-msg ()
  (should (string= (s-trim (ros-generic-get-info "msg" "std_msgs/String")) "string data")))

(ert-deftest ros-test-generic-list-topic ()
  (with-roscore
   (should (member "/rosout" (ros-generic-list "topic")))))

(ert-deftest ros-test-generic-list-node ()
  (with-roscore
   (should (member "/rosout" (ros-generic-list "node")))))

(ert-deftest ros-test-generic-list-services ()
  (with-roscore
   (should (member "/rosout/get_loggers" (ros-generic-list "service")))))


(ert-deftest ros-insert-msg-python-import-statement()
  (with-temp-buffer
    (python-mode)
    (ros-insert-import "msg" "package/FooMsg")
    (should (string= (s-trim (buffer-string)) "from package.msg import FooMsg"))
    )
  )

(ert-deftest ros-insert-msg-python--import-statement-do-not-import-twice()
  (with-temp-buffer
    (python-mode)
    (insert "\n")
    (ros-insert-import "msg" "package/FooMsg")
    (ros-insert-import "msg" "package/FooMsg")
    (should (string= (s-trim (buffer-string)) "from package.msg import FooMsg"))
    )
  )

(ert-deftest ros-insert-msg-python-import-statement-with-same-package-already-present ()
  (with-temp-buffer
    (insert "from package.msg import TestMsg\nfoo\nbar")
    (python-mode)
    (ros-insert-import "msg" "package/FooMsg")
    (should (string= (buffer-string) "from package.msg import TestMsg, FooMsg\nfoo\nbar"))
    )
  )

(ert-deftest ros-insert-msg-python-import-statement-import-next-to-other-imports()
  (with-temp-buffer
    (insert "foo\nbar\nimport test\nfoo\nbar")
    (python-mode)
    (ros-insert-import "msg" "package/FooMsg")
    (should (string= (buffer-string) "foo\nbar\nimport test\nfrom package.msg import FooMsg\nfoo\nbar"))
    )
  )

(ert-deftest ros-insert-msg-cpp-import-statement()
  (with-temp-buffer
    (c++-mode)
    (ros-insert-import "msg" "package/FooMsg")
    (should (string= (s-trim (buffer-string)) "#include <package/FooMsg.h>"))
    )
  )

(ert-deftest ros-insert-msg-cpp-import-statement-do-not-import-twice()
  (with-temp-buffer
    (c++-mode)
    (ros-insert-import "msg" "package/FooMsg")
    (ros-insert-import "msg" "package/FooMsg")
    (should (string= (s-trim (buffer-string)) "#include <package/FooMsg.h>"))
    )
  )

(ert-deftest ros-insert-msg-cpp-import-statement-next-to-other-imports-same-package()
  (with-temp-buffer
    (c++-mode)
    (insert "#include <foo/bar.h>")
    (ros-insert-import "msg" "package/FooMsg")
    (ros-insert-import "msg" "package/FooMsg2")
    (should (string= (s-trim (buffer-string)) "#include <foo/bar.h>\n#include <package/FooMsg.h>\n#include <package/FooMsg2.h>"))
    )
  )

(ert-deftest ros-insert-msg-cpp-import-statement-next-to-other-imports-other-msg()
  (with-temp-buffer
    (c++-mode)
    (insert "#include <foo/bar.h>")
    (ros-insert-import "msg" "std_msgs/FooMsg")
    (ros-insert-import "msg" "other_msgs/FooMsg")
    (should (string= (s-trim (buffer-string)) "#include <foo/bar.h>\n#include <std_msgs/FooMsg.h>\n#include <other_msgs/FooMsg.h>"))
    )
  )

(ert-deftest ros-insert-msg-cpp-import-statement-next-to-other-imports-other-includes()
  (with-temp-buffer
    (c++-mode)
    (insert "foo\nbar\n")
    (insert "#include <foo/bar.h>\n")
    (insert "foo\nbar\n")
    (ros-insert-import "msg" "package/FooMsg")
    (should (string= (s-trim (buffer-string)) "foo\nbar\n#include <foo/bar.h>\n#include <package/FooMsg.h>\nfoo\nbar"))
    )
  )

(ert-deftest ros-parse-package-xml-for-package-name-returns-correct-name()
  (let ((path "/opt/ros/melodic/share/geometry_msgs/package.xml"))
    (should (string= (ros-parse-package-xml-for-package path) "geometry_msgs"))
    )
  )



(provide 'ros-test)

;;; ros-test.el ends here
